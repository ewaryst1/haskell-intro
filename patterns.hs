-- a language pragma
{-# LANGUAGE ViewPatterns #-}

-- Algebraic Data Types

data Tree = Leaf Int | Branch Tree Tree

-- patterns

depth (Leaf _) = 0
depth (Branch t1 t2) = 1 + (max (depth t1) $ depth t2)
-- what about this:
-- depth (Branch t1 t2) = 1 + max (depth t1) $ depth t2

first 0 _ = []
first n [] = []
first n (hd:tl) = hd : first (n-1) tl


ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

ack2 0 n = n+1
ack2 m n = case n of
            0 -> ack2 (m-1) 1
            _ -> ack2 (m-1) (ack2 m (n-1))



-- strict stuff
firstStrict n !l = first n l

-- try first 0 undefined vs. firstStrict 0 undefined

-- ViewPatterns
example :: Maybe ((String -> Int,Integer), String) -> Bool
example (Just ((f,_), f -> 4)) = True
example _ = False



-- Guards
ackQ m n | m <= 0 = n+1
         | n <= 0 = ackQ (m-1) n+1
         | otherwise = ackQ (m-1) (ackQ m (n-1))
