
-- https://wiki.haskell.org/Monads_as_computation

-- the value monad
data Value t = Value t
    deriving Show

f x = Value $ x + x

ret x = Value x
bind :: Value t1 -> (t1 -> Value t2) -> Value t2
bind (Value x) g = g x

-- monad rules
--
-- bind (return x) g == g x
-- bind v return == v
-- bind (bind v g) h == bind v (\x -> (bind (g x) h))

--lhs v g h = bind (bind v g) h
--rhs v g h = bind v (\x -> (bind (g x) h))

instance Functor Value where
    fmap f (Value x) = Value $ f x

instance Applicative Value where
    pure = ret
    (Value g) <*> (Value x) = Value $ g x

instance Monad Value where
    return = pure
    (>>=) = bind

-- do notation

-- rules -> https://wiki.haskell.org/Monads_as_computation#Do_notation
-- see:
--do { x } = x
--
--do { x ; <stmts> }
--  = x >> do { <stmts> }
--
--do { v <- x ; <stmts> }
--  = x >>= \v -> do { <stmts> }
--
--do { let <decls> ; <stmts> }
--  = let <decls> in do { <stmts> }
--
-- do { expr1; expr2; expr3 }  ==>  do { expr1; do { expr2; expr3 } }
-- do { x <- expr1; expr2 }   ==>  expr1 >>= \x -> expr2
-- do { expr1; expr2 }  ==>  expr1 >> expr2


myfun = do
    ten <- Value 10
    twenty <- f ten
    forty <- f twenty
    let (***) x y = return $ x * y
    fortyTwenty <- forty *** twenty
    return fortyTwenty

-- see also: infixl 7 ***, infixr 10 ***


-- IO monad

myprint = do
    input <- getLine
    putStr "you typed '"
    putStr input
    putStrLn "'"
    putStrLn "done"

