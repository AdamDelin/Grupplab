{- Lab 1
   Authors: Adam Delin, Christine Pettersson, Danilo Vergara
   Lab group: 52
 -}
---------------------------------------------
import Graphics.Win32.Key (kLF_REORDER)
import GHC.Types (Bool)
power :: Integer -> Integer -> Integer
power n k
   | k < 0 = error "power: negative argument"
power n 0  = 1
power n k  = n * power n (k-1)

-- A -------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1



-- B -------------------------
-- power1

power1 :: Integer  -> Integer  -> [Integer] 
power1 n k
   | k < 0 = error "power1: negative argument"
power1 n k
   |k > 0 = n : power1 n (k-1)
   |otherwise  = [1]

-- C -------------------------
-- power2

power2 :: Integer -> Integer -> Integer  
power2 n k
   | k < 0 = error "power2: negative argument"
power2 n k 
   |k == 0 = 1
   |even k = power (n*n) (div k 2)
   |odd k = n*power2 n (k-1)

-- D -------------------------
{- 
n [-4, 0, 2, 4, 6, 8, 10]
k [-4, 0, 3, 12, 137, 200]
Vi valde dessa talen för de testar vilkor i de olika funktionerna
Vi har valt positiva negativa och relativt höga/låga tal.
 -}

-- comparePower1

comparePower1 :: Integer  -> Integer  -> Bool
comparePower1 n k = power n k == product(power1 n k)



-- comparePower2
comparePower2 :: Integer  -> Integer  -> Bool
comparePower2 n k = power n k == power2 n k
-- Test functions: 
tp :: Integer -> [Integer] -> [Bool]
tp n ks 
   |null ks = error "testpower: no arguments in list"
   |length ks == 1 = [comparePower1 n (head ks) == comparePower2 n (head ks)]
   |comparePower1 n (head ks)==comparePower2 n (head ks)=True : tp n (tail ks)
tp2 :: [Integer] -> [Integer] -> [Bool] 
tp2 ns ks
   |null ns = error "testpower2: no arguments in list"
   |length ns == 1 = tp (head ns) ks
   |otherwise = tp (head ns) ks ++ tp2 (tail ns) ks