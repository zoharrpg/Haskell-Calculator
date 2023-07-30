module EitherAlgExp
  (eitherCheckChars,
   eitherTokenizer,
   eitherCheckBalanced,
   eitherInfix2Postfix,
   eitherEvaluate) where


eitherCheckChars :: [Char] -> Either [Char] [Char]
eitherCheckChars  xs = eitherCheckChars' xs xs

eitherCheckChars' :: [Char]->[Char]->Either [Char] [Char]
eitherCheckChars' [] ys = Right ys
eitherCheckChars' (x:xs) ys
  | x `elem` validChars = eitherCheckChars' xs ys
  | otherwise = Left ("Error [checkChars]: The character"++show x ++ "is not allowed.")
  where validChars = ['0'..'9']++"+-*/()[]{} "


eitherTokenizer :: [Char] -> Either [Char] [[Char]]
eitherTokenizer xs
  | isPrefixOf "Error [checkChars]" xs = Left xs
  | otherwise = Right $ tokenize xs

tokenize :: [Char] -> [String]
tokenize xs = tokenize' xs [] []

tokenize' :: [Char]-> [String]->[Char]->[String]
tokenize' [] result stack = result++[stack]
tokenize' (x:xs) result []
    | x==' ' = tokenize' xs (result) []
    | isOperator [x] = tokenize' xs (result++[[x]]) []
    | isOpenParenthesis [x] || isCloseParenthesis [x] = tokenize' xs (result++[[x]]) []
    | isNumber [x] = tokenize' xs result [x]
tokenize' (x:xs) result stack
    | x == ' ' = tokenize' xs (result++[stack]) []
    | isOperator [x] = tokenize' xs (result++[stack]++[[x]]) []
    | isOpenParenthesis [x] = tokenize' xs (result++[[x]]) stack
    | isCloseParenthesis [x] = tokenize' xs (result++[stack]++[[x]]) []
    | isNumber [x] = tokenize' xs result (stack++[x])

eitherCheckBalanced :: [[Char]] -> Either [Char] [[Char]]
eitherCheckBalanced [x]
  | isPrefixOf "Error" x = Left x
  | otherwise = Left "Error [checkBalanced]: The expression is unbalanced"
eitherCheckBalanced xs
  | isBalanced (stringListToString xs) = Right xs
  | otherwise = Left "Error [checkBalanced]: The expression is unbalanced"

isBalanced :: [Char] -> Bool
isBalanced xs = isBalanced' xs []

isBalanced' :: [Char]->[Char]->Bool
isBalanced' [] [] = True
isBalanced' [] _ = False
isBalanced' (x:xs) stack
    | x `elem` leftBrackets = isBalanced' xs (x:stack)
    | x `elem` rightBrackets = case stack of
        [] -> False
        y:ys -> if y == matchBrackets [x] then isBalanced' xs ys else False
    | otherwise = isBalanced' xs stack
        where
            leftBrackets = "([{"
            rightBrackets = ")]}"

eitherInfix2Postfix :: [[Char]] -> Either [Char] [[Char]]
eitherInfix2Postfix [x] = Left x
eitherInfix2Postfix xs = Right (words $ infixToPostfix (stringListToString xs))

infixToPostfix :: [Char]->[Char]
infixToPostfix xs =stringListToString $ infixToPostfix' (tokenize xs) [] []
infixToPostfix' :: [String]->[String]->[String]->[String]
infixToPostfix' [] postfix stack = postfix++stack
infixToPostfix' (x:xs) postfix stack
    | isNumber x = infixToPostfix' xs (postfix++[x]) stack
    | isOperator x = infixToPostfix' xs (postfix++fst poped) (snd poped)
    | isOpenParenthesis x = infixToPostfix' xs (postfix) (x:stack)
    | isCloseParenthesis x = infixToPostfix' xs (postfix++fst popedUntilpar) (snd popedUntilpar)
    | otherwise = error "Invalid expression"
    where
        poped = popOperatorFromstack stack x []
        popedUntilpar = popOperatorUntilParenthesis stack x []


eitherEvaluate :: [[Char]] -> Either [Char] [Char]
eitherEvaluate [] =Left "Invalid Empty Expression"
eitherEvaluate [x] = Left x
eitherEvaluate xs =  evaluate' xs []

evaluate' ::  [String] -> [String]-> Either String String
evaluate' [] [y] =  Right y
evaluate' [] (y:ys) = Left "Error [evaluate]: Too many operator(s)"
evaluate' (x:xs) stack
    | isNumber x = evaluate' xs (x:stack)
    | x == "+" = evaluate' xs (show (uncurry (+) numbers) :popTwice stack)
    | x == "-" = evaluate' xs (show (uncurry (-) numbers) :popTwice stack)
    | x == "*" = evaluate' xs (show (uncurry (*) numbers) :popTwice stack)
    | x == "/" = evaluate' xs (show (uncurry div numbers) :popTwice stack)
    where
        numbers = getTopTwoNumber stack

popTwice :: [String]->[String]
popTwice [] = repeat "Error"
popTwice [x] = repeat "Error"
popTwice (x:y:ys)=ys

getTopTwoNumber :: [String]->(Int,Int)
getTopTwoNumber [] = error "Invalid Expression: Too many operand(s)3"
getTopTwoNumber [x] = error "Invalid Expression: Too many operator(s)4"
getTopTwoNumber (x:y:ys) = (read y :: Int,read x :: Int)

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

isOperator :: String->Bool
isOperator [c] = c `elem` "+-*/"

isOpenParenthesis :: [Char]->Bool
isOpenParenthesis [c] = c `elem` "([{"

isCloseParenthesis :: [Char]->Bool
isCloseParenthesis [c] = c `elem` ")]}"


isNumber :: String->Bool
isNumber [] = True
isNumber (x:xs) = isDigit x && isNumber xs
    where isDigit x = x >= '0'&& x<='9'

stringListToString :: [String] -> [Char]
stringListToString [] = ""
stringListToString (x:xs) = x++" "++result
    where result=stringListToString xs

matchBrackets :: [Char]->Char
matchBrackets [x]
    | x == ')' = '('
    | x == ']' = '['
    | x == '}' = '{'


popOperatorUntilParenthesis :: [String]->String->[String]->([String],[String])
popOperatorUntilParenthesis [] y result = (result,[])
popOperatorUntilParenthesis (x:xs) y result
    | x == [matchBrackets y] = (result,xs)
    | otherwise = popOperatorUntilParenthesis xs y (result++[x])

popOperatorFromstack :: [String]->String->[String]->([String],[String])
popOperatorFromstack [] y result= (result,[y])
popOperatorFromstack (x:xs) y result
    | isLowerPrecedence x y = (result,y:x:xs)
    | otherwise = popOperatorFromstack xs y (result++[x])

isLowerPrecedence :: [Char]->[Char]->Bool
isLowerPrecedence [x] [y]
    | isOpenParenthesis [x] = True
    | x  == '*' || x == '/'= False
    | x == '+' || x == '-' = case y of
        '+'-> False
        '-'->False
        '*'->True
        '/'->True
{-
--This is how the function eitherCalculate is defined in BasicCalculator.hs.

eitherCalculate :: [Char] -> Either [Char] [Char]
eitherCalculate str = pure str >>=
                      eitherCheckChars >>=
                      eitherTokenizer >>=
                      eitherCheckBalanced >>=
                      eitherInfix2Postfix >>=
                      eitherEvaluate
-}
