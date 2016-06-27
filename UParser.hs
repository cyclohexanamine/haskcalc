module UParser
( UParse(..), UInput, UParseError
, (<|>), char, eof, optionMaybe, many, many1, oneOf, space, between, string
) where

import Control.Monad (ap)

type UInput = Char
type UParseError = String
data UParse a b = UParse {cons :: [a] -> Int, runParser :: [a] -> Either UParseError b}

mkUP :: ([a] -> (Int, Either UParseError b)) -> UParse a b
mkUP f = UParse (fst . f) (snd . f)

getUP :: UParse a b -> [a] -> (Int, Either UParseError b)
getUP p st = (cons p st, runParser p st)

instance Monad (UParse a) where
    return x = UParse (pure 0) (pure . Right $ x)
    p >>= g = mkUP $ \str -> let (fn, fres) = getUP p str in case fres of Right r  -> let (gn, gres) = getUP (g r) $ drop fn str in (fn+gn, gres)
                                                                          Left err -> (0, Left err)

instance Applicative (UParse a) where
    pure = return
    (<*>) = ap

instance Functor (UParse a) where
    fmap f (UParse c p) = UParse c $ \str -> fmap f (p str)


(<|>) :: UParse UInput a -> UParse UInput a -> UParse UInput a
p1 <|> p2 = mkUP $ \str -> case runParser p1 str of Right _ -> getUP p1 str
                                                    Left  _ -> getUP p2 str

char :: Char -> UParse UInput Char
char ch = mkUP $ \str -> case str of (head:tail) | ch==head -> (1, Right ch)
                                     _                      -> (0, Left $ "Expected "++[ch])

eof :: UParse UInput ()
eof = mkUP $ \str -> case str of (head:tail) -> (0, Left "Expected eof")
                                 _           -> (0, Right ())

optionMaybe p = (p >>= return . Just) <|> return Nothing
many p = optionMaybe p >>= \x -> case x of Just a  -> many p >>= return . (a:)
                                           Nothing -> return []
many1 p = p >>= \x -> many p >>= return . (x:)
oneOf (head:tail) = foldl (\acc nextCh -> acc <|> char nextCh) (char head) tail
space = oneOf " \n\t\r"
between leftp rightp centp = leftp >> centp >>= \x -> rightp >> return x
string (head:tail) = (foldl (\acc nextCh -> acc >> char nextCh) (char head) tail) >> return (head:tail)