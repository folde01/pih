double x = x + x
quadruple x = double (double x)
factorial n = product [1..n]
average ns = sum ns `div` length ns

a = b + c
  where
    b = 1
    c = 2
d = a * 2

-- comment

{- nested -}

n = a `div` length xs
  where
    a = 10     
    xs = [1,2,3,4,5]

myLast ns = drop (length ns - 1) ns

myLast2 ns = take 1 (reverse ns)

myInit ns = take (length ns - 1) ns

myInit2 ns = reverse (drop 1 (reverse ns))

