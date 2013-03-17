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

m = b

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