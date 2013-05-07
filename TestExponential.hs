import Numeric.Log hiding (Exp)
import Data.Random.Lift
import qualified Data.Vector as VB
import qualified Data.Vector.Unboxed as V                
import Control.Applicative                
import Control.Arrow (first, second)                
import Control.Monad                
import           Data.Random                 
import           Data.Random.Distribution.Exponential (exponential)
import           Data.Random.Distribution.StretchedExponential (stretchedExponential)
import           Data.Random.Distribution.Categorical
import System.Random.MWC (create)                 
import System.IO       

import Numeric.MixtureModel.Exponential
       
dists :: [(Weight, Exponential)]
dists = [ (0.5, Exp 70)
        , (0.5, StretchedExp 5000 2)
        ]

secondM :: Monad m => (b -> m c) -> (a,b) -> m (a,c)
secondM f (a,b) = f b >>= \c->return (a,c)

testData :: Int -> [(Weight, Exponential)] -> RVar [(ComponentIdx, Sample)]
testData n dists = do
  comps <- replicateM n $ categorical
                        $ map (\(n,(w,d))->(w,(n,d))) $ zip [0..] dists
  forM comps $ secondM drawDist
  where drawDist :: Exponential -> RVar Double
        drawDist (Exp lambda) = exponential (1/lambda)
        drawDist (StretchedExp lambda beta) = stretchedExponential beta (1/lambda)

initial :: VB.Vector (Weight, Exponential)
initial = VB.fromList
        $ [ (0.4, Exp 40)
          , (0.6, StretchedExp 8000 2)
          ]

main = do
  mwc <- create
  test <- V.fromList <$> sampleFrom mwc (testData 200000 dists)
  withFile "samples.txt" WriteMode $ \f->V.mapM_ (hPrint f . snd) test
  let samples = V.map snd test
  assignments0 <- sampleFrom mwc
                  $ updateAssignments samples initial
  print $ paramsFromAssignments samples (VB.map snd initial) assignments0
  let f :: (ComponentParams, Assignments) -> RVarT IO (ComponentParams, Assignments)
      f (params, a) = do
        a' <- lift $ updateAssignments samples params
        let params' = estimateWeights a' $ paramsFromAssignments samples (VB.map snd params) a'
        lift $ print ( params'
                     , ln $ scoreAssignments samples params' a'
                     )
        return (params', a')
  (params, assignments) <- sampleFrom mwc $ replicateM' 20 f (initial, assignments0)
  print $ V.length $ V.filter not $ V.zipWith (==) assignments (V.map fst test)
  print params

replicateM' :: Monad m => Int -> (a -> m a) -> a -> m a
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a
replicateM' n f a = f a >>= replicateM' (n-1) f
