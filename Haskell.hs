last_ xs = head ( reverse xs )

init_ xs = take (length xs - 1) xs

even_ n = n `mod` 2 == 0

splitAt_ n xs = (take n xs, drop n xs)

abs_ :: Int -> Int
abs_ n = if n>0 then n else -n

abs_ n | n>=0 = n
       | otherwise = -n

signum_ :: Int -> Int
signum_ n = if n<0 then -1 else
        if n==0 then 0 else 1
       
signum_ n | n<0 = -1
        | n==0 = 0
        | otherwise = 1

halve xs = splitAt (length xs `div` 2) xs        
              
third :: [a] -> a
third xs = head (tail (tail xs)

third xs = xs !! 2

third :: [a] -> a
third (_:_:x:_) = x
third _ = error "List doesn't have a third element"

safeTail xs | null xs = xs
    | otherwise = tail xs

luhnDouble d = if n>9 then (n-9)
    else n
    where n = 2*d

luhn a b c d = (a1+b+c1+d) `mod` 10 == 0
    where a1 = luhnDouble a
    c1 = luhnDouble c    
