{-# LANGUAGE KindSignatures, DataKinds #-}

import GHC.TypeLits
import qualified Data.Vector as V
import Data.Maybe
import Data.List
import Linear
import Linear.V
import Numeric.Factorization.LU

matrixFromList :: SingI k => [[a]] -> V (k::Nat) (V k a)
matrixFromList =
    fromJust . fromVector . V.fromList . map (fromJust . fromVector . V.fromList)

formatMatrix :: (Show a, SingI k) => V (k::Nat) (V k a) -> String
formatMatrix v =
    intercalate "\n" $ map (intercalate "    " . map show . V.toList . toVector)
    $ V.toList $ toVector v

a :: (V 2 (V 2 Float))
a = matrixFromList [ [2,0]
                   , [3,5] ]

b :: (V 3 (V 3 Float))
b = matrixFromList [ [6,0,3]
                   , [3,9,9]
                   , [3,0,8] ]

c :: (V 10 (V 10 Float))
c = matrixFromList [ [6,0,3,5,6, 3,4,6,1,4]
                   , [3,9,9,8,2, 4,7,4,1,4]
                   , [7,3,4,4,0, 2,6,0,4,1]
                   , [4,7,3,2,7, 2,8,5,0,2]
                   , [4,7,3,3,1, 2,5,8,1,3]

                   , [4,7,4,8,2, 1,2,4,6,2]
                   , [2,8,3,6,8, 3,7,2,4,7]
                   , [1,5,3,8,2, 2,7,3,2,1]
                   , [1,6,7,2,1, 1,2,5,1,1]
                   , [1,6,2,6,1, 5,7,2,1,4]
                   ]

m = c

main = do
    let Just (l,u) = crout m
    putStrLn "A = "
    putStrLn $ formatMatrix m
    putStrLn ""
    putStrLn "L = "
    putStrLn $ formatMatrix l
    putStrLn ""
    putStrLn "U = "
    putStrLn $ formatMatrix u
    putStrLn ""
    putStrLn "L U = "
    putStrLn $ formatMatrix $ l !*! u
    putStrLn ""
    putStrLn $ "det a = "++show (croutDet m)
    putStrLn $ "det a = "++show (doolittleDet m)