import Control.Monad
import UParser2

type NumType = Double
type ParseNum = UParse UInput NumType

binOpers = [('+', (+)), ('-', (-)), ('*', (*)), ('/', (/)), ('^', (**))]


main = forever $ putStrLn . formatResult . parseExp =<< getLine

parseExp :: String -> Either UParseError NumType
parseExp = parse (expression >>= \exp -> eof >> return exp)

operatorExpression :: Char -> ParseNum -> ParseNum
operatorExpression ch lower = do
    wsp
    x <- lower
    wsp
    y <- optionMaybe $ do
        op <- char ch
        exp <- operatorExpression ch lower
        wsp
        return . flip (getBinOp op) $ exp
    return $ case y of Just ex -> ex x
                       Nothing -> x
      where wsp = many space

opExps = foldr (\(opCh,_) opExps -> operatorExpression opCh (head opExps) : opExps) [factor] binOpers
expression = head opExps

factor :: ParseNum
factor = do
    sgn <- optionMaybe $ oneOf "+-"
    x <- between (char '(') (char ')') expression <|> float
    return $ case sgn of Just '-' -> negate x
                         _        -> x

float :: ParseNum
float = do
    x <- many1 (oneOf ['0'..'9'])
    y <- optionMaybe $ string "." >> many (oneOf ['0'..'9'])
    return . read $ x ++ case y of Just st -> '.':st
                                   Nothing -> ""


formatResult :: Either UParseError NumType -> String
formatResult (Left  x) = "Error at " ++ show x ++ "\n"
formatResult (Right x) = show x ++ "\n"

getBinOp :: Char -> NumType -> NumType -> NumType
getBinOp op = head . foldl (\acc (ch,func) -> if ch==op then func:acc else acc) [] $ binOpers