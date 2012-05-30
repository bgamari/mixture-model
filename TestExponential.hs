import Data.Number.LogFloat                
import Data.Random.Lift
import qualified Data.Vector as V                
import Control.Applicative                
import Control.Arrow (first, second)                
import Control.Monad                
import           Data.Random                 
import           Data.Random.Distribution.Exponential
import           Data.Random.Distribution.Categorical
import System.Random.MWC (create)                 
import System.IO       

import Numeric.MixtureModel.Exponential
       
dists :: [(Weight, ExpParam)]
dists = [ (0.5, 70)
        , (0.5, 5000)
        ]

secondM :: Monad m => (b -> m c) -> (a,b) -> m (a,c)
secondM f (a,b) = f b >>= \c->return (a,c)

testData :: Int -> [(Weight, ExpParam)] -> RVar [(ComponentIdx, Sample)]
testData n dists = do
  comps <- replicateM n $ categorical
                        $ map (\(n,(w,d))->(w,(n,d))) $ zip [0..] dists
  forM comps $ secondM $ \lambda->exponential (1/lambda)

initial :: [(Weight, ExpParam)]
initial = [ (0.4, 40)
          , (0.6, 8000)
          ]

main = do
  mwc <- create
  test <- V.fromList <$> sampleFrom mwc (testData 200000 dists)
  withFile "samples.txt" WriteMode $ \f->V.mapM_ (hPrint f . snd) test
  let samples = V.map snd test
  assignments0 <- sampleFrom mwc
                  $ updateAssignments' samples (V.fromList initial)
  print $ paramsFromAssignments samples 2 assignments0
  let f :: Assignments -> RVarT IO Assignments
      f a = do
        a' <- lift $ updateAssignments samples 2 a
        let params = estimateWeights a' $ paramsFromAssignments samples 2 a'
        lift $ print (params, logFromLogFloat $ likelihood samples params a' :: Double)
        return a'
  assignments <- sampleFrom mwc $ replicateM' 100 f assignments0
  let params = estimateWeights assignments
               $ paramsFromAssignments samples 2 assignments
  print $ V.length $ V.filter not $ V.zipWith (==) assignments (V.map fst test)
  print params

replicateM' :: Monad m => Int -> (a -> m a) -> a -> m a
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a
replicateM' n f a = f a >>= replicateM' (n-1) f
