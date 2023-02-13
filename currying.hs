ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

ackmann (x::Int,y) = ack x y

ackTuple (x,y,z) = ack (x+z) y+z

-- lambda
ack2 = \(x, y) -> ack x y
ack3 = \x y -> ack x y

f a b c d e = a + b + c + d + e
-- :t f 1 2 3 4

g z = x where x = 2*z
h x = let z = 2*x in z

