{-# LANGUAGE KindSignatures, RankNTypes, PolyKinds, DataKinds #-}

module Numeric.Factorization.LU ( doolittle, crout ) where

import           Control.Applicative
import           GHC.TypeLits
import           Linear
import           Linear.V
import qualified Data.Vector as V
import           Data.Maybe

-- | @a `idx` (i,j)@ is the element of @a@ from row @i@, column @j@
idx :: V n (V m a) -> (Int, Int) -> a
a `idx` (i,j) = (toVector $ toVector a V.! i) V.! j

-- | The diagonal elements of a matrix
diagonal :: Dim n => V n (V n a) -> V n a
diagonal = fromJust . fromVector . V.imap (\i row -> toVector row `V.unsafeIndex` i) . toVector

fromMatrix :: forall n m a. (Dim n, Dim m)
           => V.Vector (V.Vector a) -> Maybe (V n (V m a))
fromMatrix v = V.mapM fromVector v >>= fromVector

-- | Generate a matrix
generateMatrix :: (Dim n, Dim m) => Int -> ((Int,Int) -> a) -> V (n::Nat) (V (m::Nat) a)
generateMatrix n f =
    fromJust $ fromVector
    $ V.generate n (\i->fromJust $ fromVector $ V.generate n (\j->f (i,j)))

-- | Doolittle's LU factorization
--
-- Doolittle's decomposition yields a factorization with upper
-- unittriangular matrix @U@ and general lower triangular matrix @L@
doolittle :: (Dim k, Epsilon a, Fractional a, SingI k)
          => V (k::Nat) (V k a) -> Maybe (V k (V k a), V k (V k a))
doolittle a
    | V.any nearZero $ toVector $ diagonal a = Nothing
    | otherwise =
  let n = dim a
      fl (i,0)              = a `idx` (i,0) / u `idx` (0,0)
      fl (i,j) | i == j     = 1
               | i <  j     = 0
               | otherwise  =
                   let uii = u `idx` (j,j)
                       aij = a `idx` (i,j)
                       f k = let lik = l `idx` (i,k)
                                 ukj = u `idx` (k,j)
                             in lik * ukj
                       s = sum $ map f $ enumFromTo 0 (j-1)
                   in (aij - s) / uii
      l = generateMatrix n fl

      fu (0,j)              = a `idx` (0,j)
      fu (i,j) | i > j      = 0
               | otherwise  =
                   let aij = a `idx` (i,j)
                       f k = let lik = l `idx` (i,k)
                                 ukj = u `idx` (k,j)
                             in lik * ukj
                       s = sum $ map f $ enumFromTo 0 (i-1)
                   in aij - s
      u = generateMatrix n fu
  in Just (l,u)

-- | Crout's LU factorization
--
-- Crout's decomposition yields a factorization with upper
-- unittriangular matrix @U@ and general lower triangular matrix @L@
crout :: (Dim k, Epsilon a, Fractional a, SingI k)
      => V (k::Nat) (V k a) -> Maybe (V k (V k a), V k (V k a))
crout a
    | V.any nearZero $ toVector $ diagonal a = Nothing
    | otherwise =
  let n = dim a
      fl (i,0)              = a `idx` (i,0)
      fl (i,j) | i < j      = 0
               | otherwise  =
                   let aij = a `idx` (i,j)
                       f k = let lik = l `idx` (i,k)
                                 ukj = u `idx` (k,j)
                             in lik * ukj
                       s = sum $ map f $ enumFromTo 0 (j-1)
                   in aij - s
      l = generateMatrix n fl

      fu (0,j)              = a `idx` (0,j) / l `idx` (0,0)
      fu (i,j) | i == j     = 1
               | i >  j     = 0
               | otherwise  =
                   let lii = l `idx` (i,i)
                       aij = a `idx` (i,j)
                       f k = let lik = l `idx` (i,k)
                                 ukj = u `idx` (k,j)
                             in lik * ukj
                       s = sum $ map f $ enumFromTo 0 (i-1)
                   in (aij - s) / lii
      u = generateMatrix n fu
  in Just (l,u)

partialPivot :: V (k::Nat) (V k a) -> V k (V k a)
partialPivot = undefined
