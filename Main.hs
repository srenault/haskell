doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallerNumber x = if x > 100
                      then x
                           else doubleMe(x)

lostNumbers = [1, 2, 3, 4]
anotherNumbers = [5, 6, 7, 8]
bigList = lostNumbers ++ anotherNumbers
prependList = 0 : lostNumbers
index2 = prependList !! 2
supFalse = lostNumbers > anotherNumbers
infTrue = lostNumbers < anotherNumbers
headLost = head lostNumbers
tailLost = tail lostNumbers
lastLost = last lostNumbers
initLost = init lostNumbers
lengthLost = length lostNumbers
emptyLost = null lostNumbers
reverseLost = reverse lostNumbers
take2Lost = take 2 lostNumbers
drop2Lost = drop 2 lostNumbers
minLost = minimum lostNumbers
maxLost = maximum lostNumbers
sumLost = sum lostNumbers
productLost = product lostNumbers
index4 = 4 `elem` lostNumbers

rangeNumbers = [1..20]
rangeChars = ['a'..'b']
cycleList = take 10 (cycle [1, 2, 3])
repeatNumber = take 10 (repeat 5)
listComprehension = [x*2 | x <- [1..10]]
listComprehension1 = [ x | x <- [50..100], x `mod` 7 == 3]
falseOdd = odd 1
trueEven = even 1
boomBangs xs = [ if x < 10 then "BOOOOMMMMM" else "BANGGGGG" | x <- xs, even x, x /= 2]
multiArrays = [ x*y | x <- [2, 3, 4], y <- [5, 6, 7], x*y > 50]
lengthLost1 = sum [1 | _ <- lostNumbers]
removeNonUpperCase st = [ c | c <- st, c `elem` ['A'..'Z']]
tuple1 = [(1,2),(3,4)]
firstTuple = fst (head tuple1)
sndTuple = snd (head tuple1)
zippedLists = zip [1, 2, 3] [4, 5, 6]

oneFunction :: [Char] -> [Char]
oneFunction xs = xs

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

factorial :: Integer -> Integer
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

-- Typeclass : Eq Ord Show Read Enum Bounded Num Integral Floating
-- Type constraint : ghci> :t (==)
-- (==) :: (Eq a) => a -> a -> Bool

sayMe :: (Integral a) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe x = "Default result"

factorial' :: (Integral a) => a -> a
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

xs = [(1,1), (2,2)]
fold = [a+b | (a,b) <- xs]

head' :: [a] -> a
head' [] = error "Empty list"
head' (h:_) = h

tell :: (Show a) => [a] -> String
tell [] = error "empty input"
tell (x:[]) = show x
tell (x:y:[]) = show y
tell xs = show xs

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Ooops, empty String"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
   | bmi <= 18.5 = "You're underweight"
   | bmi <= 25.0 = "You're normal"
   | bmi <= 30.0 = "You're overweight"
   | otherwise   = "You're a whale"

max' :: (Ord a) => a -> a-> a
max' a b
   | a > b     = a
   | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
   | a < b     = LT
   | a > b     = GT
   | otherwise = EQ

bmiTell' weight height
   | bmi <= 18.5 = "You're underweight"
   | bmi <= 25.0 = "You're normal"
   | bmi <= 30.0 = "You're overweight"
   | otherwise   = "You're a whale"
   where bmi = weight / height ^ 2
         a = 2
         (b, c, d) = (1, 2, 3)

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea -- let is an expression whereas where is a syntactic construct.

-- 4 * (let a = 9 in a + 1) + 2
-- [let square x = x * x in (square 5, square 3, square 2)]
-- (let (a,b,c) = (1,2,3) in a + b + c) * 100

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty list"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a sigleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a sigleton list"
          what xs = "a longer list."

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list."
maximum' [x] = x
maximum' (x:xs)
         | x > maxTail = x
         | otherwise = maxTail
         where maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = xs ++ [x]

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' a [] = False
elem'' a (x:xs)
    | a == x = True
    | otherwise = a `elem''` xs
