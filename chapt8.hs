-- Ackermann Function
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

-- Collatz Function
collatz 1 = 1
collatz n
    | even n = collatz (n `div` 2)
    | otherwise = 1 + collatz (n * 3 + 1)

-- Reverse
myrev [] = []
myrev (x:xs) = (myrev xs) ++ [x]

-- Fibonacci
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Fibonacci with memoization
fastFib n = fastFib' 1 1 n
    where
        fastFib' _ _ 0 = 0
        fastFib' n1 n2 1 = n1
        fastFib' n1 n2 c = fastFib' n2 (n1 + n2) (c - 1)

-- Factorial with memoization
factorial n = factorial' n 1
    whereoL
        factorial' 0 acc = acc
        factorial' m acc = factorial' (m - 1) (m * acc)