data Nat
  = Zero
  | Suc Nat
  deriving (Show)

integer2Nat :: Integer -> Nat
integer2Nat n
  | n == 0 = Zero
  | n > 0 = Suc (integer2Nat (n - 1))
  | otherwise = Zero

nat2Integer :: Nat -> Integer
nat2Integer Zero = 0
nat2Integer (Suc n) = 1 + nat2Integer n

natAdd :: Nat -> Nat -> Nat
natAdd x Zero = x
natAdd x (Suc y) = Suc (natAdd x y)

natSub :: Nat -> Nat -> Nat
natSub x Zero = x
natSub Zero _ = Zero
natSub (Suc x) (Suc y) = natSub x y

natMul :: Nat -> Nat -> Nat
natMul _ Zero = Zero
natMul Zero _ = Zero
natMul x (Suc y) = natAdd x (natMul x y)

n0 = Zero -- 0

n1 = Suc n0 -- 1

n2 = Suc n1 -- 2

n3 = Suc n2 -- 3

n4 = Suc n3 -- 4
