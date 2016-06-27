import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import Data.Fixed (mod')
import Math.Gamma

consts = M.fromList [("e", exp 1), ("pi", pi)]
monOpers = M.fromList [("neg", negate), ("!", gamma . (+1)), ("sin", sin), ("cos", cos), ("tan", tan), ("ln", log), ("sqrt", sqrt), ("exp", exp), ("inv", (1/))]
binOpers = M.fromList [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/)), ("^", (**)), ("%", mod'), ("root", \x y -> x ** (1/y))]
stackOpers = M.fromList [("drop", drop 1), ("clr", pure []), ("sum", (:[]) . sum), ("prod", (:[]) . product), ("switch", \(x:y:xs)->(y:x:xs)), ("ans", \(x:xs)->(x:x:xs))]
operList = "+*/!%^"
whitespaceList = " \n\t"

type NumType = Double


main = putStrLn "Interactive RPN calculator; type \"quit\" to quit." >> nextLine []

nextLine :: [NumType] -> IO ()
nextLine stack = do
    printState stack
    inStr <- getLine
    when (not . elem inStr $ ["q", "quit"]) $ do
        nextLine . foldl calcNext stack . lexStr $ inStr

printState :: [NumType] -> IO ()
printState stack = do
    putStrLn "__________________"
    mapM_ print . reverse $ stack
    putStrLn "------------------"

lexStr :: [Char] -> [[Char]]
lexStr inStr =
        let
          splitStr = split (oneOf $ operList ++ whitespaceList) inStr
          splitMinus = foldr (\op acc -> let l = length op in   if (l > 1) && (op!!(l-1) == '-') then (take (l-1) op):"-":acc else op:acc) [] splitStr
        in
          filter (null . intersect whitespaceList) . filter (not . null) $ splitMinus

calcNext :: [NumType] -> [Char] -> [NumType]
calcNext stack next
    | nextMem stackOpers = (nextLookup stackOpers) stack
    | nextMem monOpers   = (nextLookup monOpers) (stack!!0)            : (drop 1 stack)
    | nextMem binOpers   = (nextLookup binOpers) (stack!!1) (stack!!0) : (drop 2 stack)
    | nextMem consts     = (nextLookup consts)                         : stack
    | otherwise          = (reads next >>= ((:[]) . fst)) ++ stack
    where
        nextMem = M.member next
        nextLookup = fromJust . M.lookup next