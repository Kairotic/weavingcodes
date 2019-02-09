module WWL where

up :: Bool
up = True

down :: Bool
down = False

width = 12
height = 12

every :: Int -> ([a] -> [a]) -> [a] -> [a]
every _ _ [] = []
every 0 _ xs = xs
-- take width added after the applicaiton of `f` to make sure transformations don't go outside the row
every n f xs = (take width $ f $ take width xs) ++ (take (width * (n-1)) $ drop width xs) ++ every n f (drop (width*n) xs)

shift :: [a] -> [a]
shift [] = []
shift (x:xs) = xs ++ [x]

invert :: [Bool] -> [Bool]
invert = map not

double :: [a] -> [a]
double [] = []
double (x:xs) = (x:x:double xs)

weave :: [Bool] -> IO ()
weave bits = putStrLn $ weave' height bits
  where
    weave' 0 bits' = []
    weave' n bits' | even n = (showRow $ take width bits') ++ "\n" ++ weave' (n-1) (drop width bits')
                   | otherwise = (showRow $ reverse $ take width bits') ++ "\n" ++ weave' (n-1) (drop width bits')
    showRow [] = []
    showRow (True:xs) = '|':showRow xs
    showRow (False:xs) = '—':showRow xs
  
