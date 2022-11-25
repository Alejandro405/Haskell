module Main where
    

main :: IO ()
main = print (fac 20)



fac 0 = 1
fac n = n * fac (n-1)


clear :: String
clear = ":! clear"

reciprocal :: (Eq a, Fractional a) => a -> a
reciprocal x | x == 0 = error "Pobre diabla"
             | otherwise = 1 / (x + 1)