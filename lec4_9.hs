
module Numbers where


import Data.Ratio 

-- ghci> :t 2
-- 2 :: Num a => a

-- ghci> :i Num
-- type Num :: * -> Constraint
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--   {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
--         -- Defined in ‘GHC.Num’
-- instance Num Double -- Defined in ‘GHC.Float’
-- instance Num Float -- Defined in ‘GHC.Float’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Num Integer -- Defined in ‘GHC.Num’
-- instance Num Word -- Defined in ‘GHC.Num’






-- ghci> 10 `div` 2
-- 5

-- ghci> 100 `divMod` 7
-- (14,2)

-- ghci> 100 `div` 0
-- *** Exception: divide by zero

-- ghci> recip 0.1
-- 10.0


-- ghci> :i Rational
-- type Rational :: *
-- type Rational = GHC.Real.Ratio Integer
--         -- Defined in ‘GHC.Real’


-- ghci> 0.353432 :: Rational 
-- 44179 % 125000

-- ghci> 0.12 :: Rational
-- 3 % 25



-- ghci> :i RealFrac
-- type RealFrac :: * -> Constraint
-- class (Real a, Fractional a) => RealFrac a where
--   properFraction :: Integral b => a -> (b, a)
--   truncate :: Integral b => a -> b
--   round :: Integral b => a -> b
--   ceiling :: Integral b => a -> b
--   floor :: Integral b => a -> b
--   {-# MINIMAL properFraction #-}
--         -- Defined in ‘GHC.Real’
-- instance RealFrac Double -- Defined in ‘GHC.Float’
-- instance RealFrac Float -- Defined in ‘GHC.Float’



-- ghci> floor 3.5
-- 3

-- ghci> floor 3.7
-- 3

-- ghci> ceiling 3.2
-- 4



-- ghci> round 3.4
-- 3

-- ghci> round 3.5
-- 4

-- ghci> round 3.6
-- 4


-- fromIntegral :: (Integral a, Num b) => a -> b

-- ghci> fromIntegral 3 :: Double
-- 3.0

