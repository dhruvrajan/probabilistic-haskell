module Monads where

isBigGang :: Int -> Bool
isBigGang x = x > 9

data Expr1 = Val1 Int
           | Div1 Expr1 Expr1
           | ITE Expr1 Expr1
             deriving (Show)

eval1 :: Expr1 -> Int
eval1 (Val1 n) = n
eval1 (Div1 x y) = eval1 x `div` eval1 y

eval1' :: Expr1 -> Maybe Int
eval1' (Val1 n) = Just n
eval1' (Div1 x y) = safeDiv' (eval1' x) (eval1' y)

safeDiv' :: Maybe Int -> Maybe Int -> Maybe Int
safeDiv' _ (Just 0) = Nothing
safeDiv' (Just n) (Just m) = Just (n `div` m)
safeDiv' _ _ = Nothing

safediv     :: Int -> Int -> Maybe Int
safediv n m  =  if m == 0 then Nothing else Just (n `div` m)

eval :: Expr1 -> Maybe Int
eval (Val1 n) = Just n
eval (Div1 x y) = do n <- eval x
                     m <- eval y
                     safediv n m

data Tree a = Leaf a
            | Node (Tree a) (Tree a)


leafLabel :: Tree a -> Tree (a, Int)
leafLabel = leafLabel' 0

leafLabel' :: Int -> Tree a -> Tree (a, Int)
leafLabel' i (Leaf x) = Leaf (x, i + 1)
leafLabel' i (Node (left) (right)) = Node (leafLabel' i left ) (leafLabel' i right)

main :: IO ()
main = do
  putStrLn("hey there!")

type State = Int
data ST0 a = S0 (State -> (a, State))

apply0 :: ST0 a -> State -> (a, State)
apply0 (S0 f) x = f x

instance Functor ST0 where
  fmap 

instance Applicative ST0 where
  

instance Monad ST0 where
  return x = S0 (\s -> (x, s))
  st >>= f = S0 (\s -> (let (x, s') = apply0 st s in apply0 (f x) s'))
