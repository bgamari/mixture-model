import qualified Data.Vector as V                
import Control.Applicative                
import           Data.Random                 
import           Data.Random.Distribution.Beta
import           Data.Random.Distribution.Categorical
import System.Random.MWC (create)                 

import Numeric.MixtureModel.Beta hiding (beta)
       
dists = [ paramFromMoments (0.2, 0.1)
        , paramFromMoments (0.8, 0.1)
        ]

testData :: Int -> [BetaParam] -> RVar [Sample]
testData n dists =
  mapM (uncurry beta) $ concat $ replicate n dists

priors = [ paramFromMoments (0.1, 0.01)
         , paramFromMoments (0.9, 0.01)
         ]

weights :: [(Double, ComponentIdx)]       
weights = [(0.5, 0), (0.5, 1)]

main = do
  mwc <- create
  samples <- sampleFrom mwc $ testData 2000 dists
  assignments0 <- sampleFrom mwc $ updateAssignments' (V.fromList samples) 2 (V.fromList priors)
  print $ V.map paramToMoments $ paramsFromAssignments (V.fromList samples) 2 assignments0
  assignments <- sampleFrom mwc
                 $ replicateM' 500 (updateAssignments (V.fromList samples) 2) assignments0
  print $ V.map paramToMoments $ paramsFromAssignments (V.fromList samples) 2 assignments

replicateM' :: Monad m => Int -> (a -> m a) -> a -> m a
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a
replicateM' n f a = f a >>= replicateM' (n-1) f

