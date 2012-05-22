import Data.Number.LogFloat                
import Data.Random.Lift
import qualified Data.Vector as V                
import Control.Applicative                
import Control.Arrow (first, second)                
import Control.Monad                
import           Data.Random                 
import           Data.Random.Distribution.Beta
import           Data.Random.Distribution.Categorical
import System.Random.MWC (create)                 

import Numeric.MixtureModel.Beta
       
dists :: [(Weight, BetaParam)]
dists = [ (0.4, paramFromMoments (0.2, 0.05))
        , (0.6, paramFromMoments (0.8, 0.01))
        ]

secondM :: Monad m => (b -> m c) -> (a,b) -> m (a,c)
secondM f (a,b) = f b >>= \c->return (a,c)

testData :: Int -> [(Weight, BetaParam)] -> RVar [(ComponentIdx, Sample)]
testData n dists = do
  comps <- replicateM n $ categorical
                        $ map (\(n,(w,d))->(w,(n,d))) $ zip [0..] dists
  forM comps $ secondM (uncurry beta)

priors :: [(Weight, BetaParam)]
priors = [ (0.5, paramFromMoments (0.1, 0.01))
         , (0.5, paramFromMoments (0.9, 0.01))
         ]

main = do
  mwc <- create
  test <- V.fromList <$> sampleFrom mwc (testData 2000 dists)
  let samples = V.map snd test
  assignments0 <- sampleFrom mwc
                  $ updateAssignments' samples (V.fromList priors)
  print $ V.map paramToMoments
        $ paramsFromAssignments samples 2 assignments0
  let f :: Assignments -> RVarT IO Assignments
      f a = do
        a' <- lift $ updateAssignments samples 2 a
        let params = estimateWeights a' $ paramsFromAssignments samples 2 a'
        lift $ print $ V.map paramToMoments $ V.map snd params
        lift $ print (logFromLogFloat $ likelihood samples params a' :: Double)
        return a'
  assignments <- sampleFrom mwc
                 $ replicateM' 1000 f assignments0
  let params = estimateWeights assignments
               $ paramsFromAssignments samples 2 assignments
  print $ V.length $ V.filter not $ V.zipWith (==) assignments (V.map fst test)
  print $ V.map (second paramToMoments) params

replicateM' :: Monad m => Int -> (a -> m a) -> a -> m a
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a
replicateM' n f a = f a >>= replicateM' (n-1) f

