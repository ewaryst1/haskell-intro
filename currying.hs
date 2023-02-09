ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

ackmann (x::Int,y) = ack x y

ackTuple (x,y,z) = ack (x+z) y+z

f a b c d e = a + b + c + d + e

-- :t f 1 2 3 4

-----------------------------------


ackQ z n | z <= 0 = n+1
ackQ m z | z <= 0 = ackQ (m-1) z+1
ackQ m n = ackQ (m-1) (ackQ m (n-1))


ackQString z n | z <= 0 = show $ n + 1
ackQString m z | z <= 0 = "ackQString " ++ show (m - 1) ++ " " ++ show (z + 1)
ackQString m n = "ackQString " ++ show (m - 1) ++ " (ackQString " ++ show m ++ " " ++ show (n - 1) ++ ")"

ackQString2 z n | z <= 0 = show n ++ "+1"
ackQString2 m z | z <= 0 = "ackQ (" ++ show m ++ "-1) 1"
ackQString2 m n = "ackQ (" ++ show m ++ "-1) (ackQ " ++ show m ++ " (" ++ show n ++ "-1))"

ackQPrint m n = putStrLn $ ackQString m n