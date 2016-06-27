module UParser2
( UParse(..), UInput, UParseError, parse
, (<|>), char, eof, optionMaybe, many, many1, oneOf, space, between, string
) where

import Control.Monad (ap)
import Control.Monad.State

type UInput = Char
type UParseError = String
newtype UParse a b = UParse {getState :: State [a] (Either UParseError b)}

makeParser = UParse . state
runParser = runState . getState
parse p str = fst . runParser p $ str

instance Monad (UParse a) where
    return x = makeParser $ \str -> (Right x, str)
    p >>= g  = makeParser $ \str -> let (res, newStr) = runParser p str in case res of Right r  -> runParser (g r) newStr
                                                                                       Left err -> (Left err, str)

instance Applicative (UParse a) where
    pure = return
    (<*>) = ap

instance Functor (UParse a) where
    fmap f (UParse st) = UParse (fmap (fmap f) st)


(<|>) :: UParse UInput a -> UParse UInput a -> UParse UInput a
p1 <|> p2 = makeParser $ \str -> case runParser p1 str of res@(Right _, st) -> res
                                                          res@(Left  _, _ ) -> runParser p2 str
 
char :: Char -> UParse UInput Char
char ch = makeParser $ \str -> case str of (head:tail) | ch==head -> (Right ch, drop 1 str)
                                           _                      -> (Left $ "Expected "++[ch], str)

eof :: UParse UInput ()
eof = makeParser $ \str -> case str of (head:tail) -> (Left "Expected eof", str)
                                       _           -> (Right (), str)

optionMaybe p = (p >>= return . Just) <|> return Nothing
many p = optionMaybe p >>= \x -> case x of Just a  -> many p >>= return . (a:)
                                           Nothing -> return []
many1 p = p >>= \x -> many p >>= return . (x:)
oneOf (head:tail) = foldl (\acc nextCh -> acc <|> char nextCh) (char head) tail
space = oneOf " \n\t\r"
between leftp rightp centp = leftp >> centp >>= \x -> rightp >> return x
string (head:tail) = (foldl (\acc nextCh -> acc >> char nextCh) (char head) tail) >> return (head:tail)