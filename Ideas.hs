


° Is ° :: a -> (a -> Bool)
a is b = b a

° and ° :: a -> b -> (a, b)
a and b = (a, b)

add ° :: Num a: (a, a) -> a
add (a, b) = a + b

° is between ° :: Ord a => a -> (a, a) -> Bool
a is between (b, c) = b <= a && a <= c


