module Calculator
( TokenKind
, Token

, Number(..)
, Factor(..)
, Component(..)
, Expression(..)

, lexed
) where

import Data.Maybe



-- LEXER
tKind :: Char -> Maybe TokenKind
tKind ch
    | null resultList  = Nothing
    | otherwise        = Just . fst . head $ resultList
    where 
        resultList = filter isCharOfKind kindList
        isCharOfKind = \(kind, charList) -> ch `elem` charList
        kindList = [(Digit, ['0'..'9']), (Sign, "+-"), (Operator, "/*^%"), (OParenthesis, "("), (CParenthesis, ")")]
        
tokenise :: Char -> Maybe Token
tokenise ch
    | isNothing kind    = Nothing
    | otherwise         = Just $ Tok (fromJust kind) ch
    where
        kind = tKind ch
      
lexed :: [Char] -> [Token]
lexed str = map fromJust . filter isJust . map tokenise $ str




-- PARSER
addToExpression :: Expression -> Token -> Expression
addToExpression expr token@(Tok kind ch)
    | expr == EmptyExpression   = case kind of Digit        -> expressionFromNumber . NumberFromDigits $ [token]
                                               Sign         -> expressionFromNumber . NumberFromSignDigits token $ [Tok Digit '0']
                                               OParenthesis -> expressionFromFactor . FactorFromExpression
                                               _       -> error("Bad character "++[ch])
    
    
    -- | expr == 


parsed :: [Token] -> Expression
parsed = foldl addToExpression EmptyExpression
        


data TokenKind = Digit | Sign | Operator | OParenthesis | CParenthesis  deriving (Show, Read, Eq, Ord) 
data Token = Tok TokenKind Char  deriving (Show, Read, Eq, Ord) 

data Number = NumberFromDigits [Token] | NumberFromSignDigits Token [Token]  deriving (Show, Read, Eq, Ord) 
data Factor = FactorFromNumber Number | FactorFromExpression Token Expression Token  deriving (Show, Read, Eq, Ord) 
data Component = ComponentFromFactor Factor | ComponentFromFactors Factor [(Token, Factor)]  deriving (Show, Read, Eq, Ord) 
data Expression = EmptyExpression | ExpressionFromComponent Component | ExpressionFromComponents Component [(Token, Component)]  deriving (Show, Read, Eq, Ord) 


expressionFromFactor = ExpressionFromComponent . ComponentFromFactor
expressionFromNumber = expressionFromFactor . FactorFromNumber
