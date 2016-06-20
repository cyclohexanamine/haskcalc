module Calculator
( TokenKind
, Token

, Number(..)
, Expression(..)

, eval
, lexed
, parsed
, evaluated
) where

import Data.Maybe
import Data.List
import qualified Data.Map
import Data.Fixed (mod')


eval :: [Char] -> OutNum
eval = evaluated . parsed . lexed



-- TYPES
type OutNum = Float

data TokenKind = Digit | Sign | Operator | OParenthesis | CParenthesis                      deriving (Show, Read, Eq)
data Token = Token TokenKind Char                                                           deriving (Show, Read, Eq)

type Op = Token
type Sign = Token
data Number = Number Sign [Token]                                                           deriving (Show, Read, Eq, Ord)

data Expression = EmptyExp | ExpOpen Expression | ExpClosed Expression | ExpFromNum Number |  ExpFromOp Expression Op Expression        deriving (Show, Read, Eq)

instance Ord Token where
    (Token Operator ch1) `compare` (Token Operator ch2) = compareOps ch1 ch2


kindList = [(Digit, '.':['0'..'9']), (Sign, "+-"), (Operator, "/*^%"), (OParenthesis, "("), (CParenthesis, ")")]
whitespace = " \t\n"
precedenceList = ["^", "()", "/", "*", "-", "+", "%"]
operatorFuncs = [('+', (+)), ('-', (-)), ('*', (*)), ('/', (/)), ('^', (**)), ('%', mod')]





-- LEXER
tKind :: Char -> Maybe TokenKind
tKind ch
    | null resultList  = Nothing
    | otherwise        = Just . fst . head $ resultList
    where
        resultList = filter isCharOfKind kindList
        isCharOfKind = \(kind, charList) -> ch `elem` charList


tokenise :: Char -> Token
tokenise ch
    | isNothing kind    = error("Invalid character "++[ch])
    | otherwise         = Token (fromJust kind) ch
    where
        kind = tKind ch

lexed :: [Char] -> [Token]
lexed str = map tokenise . filter (not . flip elem whitespace) $ str





-- PARSER
addToExpression :: Expression -> Token -> Expression
addToExpression expr token@(Token kind ch)
    = case expr of  EmptyExp                      ->    case kind of Digit        -> ExpFromNum . numFromDigit $ token
                                                                     Sign         -> ExpFromOp EmptyExp (opFromSign token) EmptyExp
                                                                     OParenthesis -> ExpOpen EmptyExp
                                                                     CParenthesis -> EmptyExp
                                                                     _            -> error ("Bad character at start of expression: "++[ch])

                    openExp@(ExpOpen ex1)                   -> case kind of CParenthesis -> ExpClosed ex1
                                                                            _            -> ExpOpen (addToExpression ex1 token)



                    closedExp@(ExpClosed ex1)     ->    case kind of Digit        -> ExpFromOp ex1 (op '*') (ExpFromNum . numFromDigit $ token)
                                                                     Sign         -> ExpFromOp ex1 (opFromSign token) EmptyExp
                                                                     Operator     -> ExpFromOp ex1 (token :: Op) EmptyExp
                                                                     OParenthesis -> ExpFromOp ex1 (op '*') (ExpOpen EmptyExp)
                                                                     CParenthesis -> closedExp

                    numExp@(ExpFromNum num)       ->    case kind of Digit        -> ExpFromNum (appendToNumber token num)
                                                                     Sign         -> ExpFromOp numExp (opFromSign token) EmptyExp
                                                                     Operator     -> ExpFromOp numExp (token :: Op) EmptyExp
                                                                     OParenthesis -> ExpFromOp numExp (op '*') (ExpOpen EmptyExp)
                                                                     CParenthesis -> ExpClosed numExp

                    opExp@(ExpFromOp ex1 oper ex2@(ExpOpen ex2')) -> ExpFromOp ex1 oper (addToExpression ex2 token)

                    opExp@(ExpFromOp ex1 oper ex2@(ExpFromNum n2)) -> let
                                                                            leftHasPrecedence = op ch < oper
                                                                            leftOperate  = ExpFromOp opExp (op ch) EmptyExp
                                                                            rightOperate = ExpFromOp ex1 oper (addToExpression ex2 token)
                                                                            decideLeftRight = if leftHasPrecedence then leftOperate else rightOperate
                                                                         in case kind of Sign         -> decideLeftRight
                                                                                         Operator     -> decideLeftRight
                                                                                         OParenthesis -> decideLeftRight
                                                                                         CParenthesis -> ExpClosed opExp
                                                                                         _            -> rightOperate

                    opExp@(ExpFromOp ex1 oper ex2@(ExpClosed ex3@(ExpFromNum n2))) -> let
                                                                                            leftHasPrecedence = op ch < oper
                                                                                            leftOperate  = ExpFromOp (ExpFromOp ex1 oper ex3) (op ch) EmptyExp
                                                                                            rightOperate = ExpFromOp ex1 oper (addToExpression ex3 token)
                                                                                            decideLeftRight = if leftHasPrecedence then leftOperate else rightOperate
                                                                                         in case kind of Sign         -> decideLeftRight
                                                                                                         Operator     -> decideLeftRight
                                                                                                         OParenthesis -> decideLeftRight
                                                                                                         CParenthesis -> ExpClosed opExp
                                                                                                         _            -> rightOperate

                    opExp@(ExpFromOp ex1 oper ex2) -> ExpFromOp ex1 oper (addToExpression ex2 token)


parsed :: [Token] -> Expression
parsed = foldl addToExpression EmptyExp


appendToNumber :: Token -> Number -> Number
appendToNumber token (Number sign l) = Number sign (l ++ [token] )

op :: Char -> Op
op '(' = op '*'
op ch = Token Operator ch :: Op

opFromSign :: Token -> Op
opFromSign (Token Sign ch) = op ch

sign :: Char -> Sign
sign ch = Token Sign ch :: Sign

charFromToken :: Token -> Char
charFromToken (Token _ ch) = ch

numFromDigit :: Token -> Number
numFromDigit token@(Token Digit _) = Number (Token Sign '+' :: Sign) [token]

compareOps :: Char -> Char -> Ordering
compareOps ch1 ch2
    | and . map hasPrecedence $ [ch1, ch2]   = compare (getPrecedence ch2) (getPrecedence ch1)
    | otherwise                              = EQ
    where
        hasPrecedence = isJust . findPrecedence
        getPrecedence =  fromJust . findPrecedence
        findPrecedence ch = elemIndex (head . filter (\l -> elem ch l) $ precedenceList) precedenceList :: Maybe Int




-- EVALUATOR
evaluated :: Expression -> OutNum
evaluated exp
  = case exp of EmptyExp                 -> 0
                ExpOpen ex1              -> evaluated ex1
                ExpClosed ex1            -> evaluated ex1
                ExpFromNum num           -> readNum num
                ExpFromOp ex1 oper ex2   -> let (n1, n2) = (evaluated ex1, evaluated ex2)
                                            in (fromJust . Data.Map.lookup (charFromToken oper) . Data.Map.fromList $ operatorFuncs) n1 n2

readNum :: Number -> OutNum
readNum (Number sign tokenList)
    | charFromToken sign == '-' = read . map charFromToken $ sign : tokenList :: OutNum
    | charFromToken sign == '+' = read . map charFromToken $ tokenList        :: OutNum

