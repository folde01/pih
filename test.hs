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

add :: (Int, Int) -> Int
add (x,y) = x + y
 
zeroto :: Int -> [Int]
zeroto n = [1..n]

myMax :: (Ord a) => [a] -> a
myMax [] = error "maximum of empty list"
myMax [x] = x
myMax (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = myMax xs

myMax2 :: (Ord a) => [a] -> a
myMax2 [] = error "maximum of empty list"
myMax2 [x] = x
myMax2 (x:xs)
  | x > myMax2 xs = x
  | otherwise = myMax2 xs

myMax3 :: (Ord a) => [a] -> a
myMax3 [] = error "maximum of empty list"
myMax3 [x] = x
myMax3 (x:xs) = max x (myMax3 xs)

myReplicate :: (Num i, Ord i) => i -> a -> [a] 
myReplicate n x
  | n <= 0 = []
  | otherwise = x:myReplicate (n-1) x

-- take 3 [5,4,3,2,1]
-- [5,4,3]
--
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a] 
reverse' [] = [] 
reverse' [x] = [x] 
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x

-- zip' [1,2] [11,22,33] -- [(1,11), (2,22)]
zip' :: [a] -> [a] -> [(a,a)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

