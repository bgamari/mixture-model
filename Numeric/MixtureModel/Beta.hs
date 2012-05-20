module Numeric.MixtureModel.Beta ( -- * General data types
                                   Sample
                                 , SampleIdx
                                 , Samples
                                 , ComponentIdx
                                 , Assignments
                                 -- * Beta parameters
                                 , BetaParam
                                 , Params
                                 , paramFromMoments
                                 , paramToMoments
                                 , paramsFromAssignments
                                 -- * Beta distribution
                                 , Prob
                                 , betaProb
                                 -- * Gibbs sampling
                                 , updateAssignments
                                 , updateAssignments'
                                 -- * Likelihood
                                 , likelihood
                                 -- * Classification
                                 , classify
                                 ) where
                              
import           Data.Function (on)       
import qualified Data.Vector as V

import           Data.Number.LogFloat hiding (realToFrac, isInfinite)
import           Numeric.SpecFunctions (logBeta)
import           Statistics.Sample (meanVarianceUnb)
       
import           Data.Random                 
import           Data.Random.Distribution.Categorical
       
type Prob = LogFloat       
type Sample = Double
type SampleIdx = Int     
type ComponentIdx = Int     
type BetaParam = (Double, Double)

-- k refers to number of components
-- N refers to number of samples
type Samples = V.Vector Sample                   -- length == N
type Assignments = V.Vector ComponentIdx         -- length == N
type Params = V.Vector BetaParam                 -- length == K     
      
beta :: Double -> Double -> LogFloat
beta a b = logToLogFloat $ logBeta a b

-- | `betaProb (a,b) x` is the probability of `x` under Beta
-- distribution defined by parameters `a` and `b`
betaProb :: BetaParam -> Sample -> Prob
betaProb (a,b) x = 1/beta a b * logFloat (x**(a-1)) * logFloat ((1-x)**(b-1))

-- | Beta parameter from sample mean and variance
paramFromMoments :: (Double, Double) -> BetaParam
paramFromMoments (xbar,v) 
  | c < 0      = error "Not a beta distribution"
  | otherwise  = (xbar * c, (1 - xbar) * c)
  where c = xbar * (1 - xbar) / v - 1
  
-- | Mean and variance of the given beta parameter
paramToMoments :: BetaParam -> (Double, Double)
paramToMoments (a,b) =
  let mean = a / (a+b)
      var  = a*b / (a+b)^2 / (a+b+1)
  in (mean, var)
         
-- | Beta parameter from samples
paramFromSamples :: V.Vector Sample -> BetaParam
paramFromSamples v | V.null v = error "Can't estimate priors from no samples"
paramFromSamples v = paramFromMoments $ meanVarianceUnb v

-- | Beta parameter for component given samples and their component assignments
paramFromAssignments :: Samples -> Assignments -> ComponentIdx -> BetaParam         
paramFromAssignments samples assignments k =
  paramFromSamples $ V.map snd $ V.filter (\(k',_)->k==k') $ V.zip assignments samples

-- | Beta parameters for all components given samples and their component assignments
paramsFromAssignments :: Samples -> Int -> Assignments -> Params
paramsFromAssignments samples ncomps assignments =
  V.fromList $ map (paramFromAssignments samples assignments) [0..ncomps-1]

-- | Draw a new assignment for a sample given beta parameters
drawAssignment :: Params -> Sample -> RVar ComponentIdx
drawAssignment params x =
  let probs = map (\p->betaProb p x) $ V.toList params
      lfIsInfinite :: LogFloat -> Bool
      lfIsInfinite = isInfinite . (fromLogFloat :: LogFloat -> Double)
  in case filter (lfIsInfinite . fst) $ zip probs [0..] of
        (x:_)     -> return $ snd x
        otherwise -> categorical
                     $ map (\(p,k)->(realToFrac $ p / sum probs :: Double, k))
                     $ zip probs [0..]
  
-- | Sample assignments for samples under given parameters
updateAssignments' :: Samples -> Int -> Params -> RVar Assignments
updateAssignments' samples ncomps params =
  V.mapM (drawAssignment params) samples

-- | Gibbs update of sample assignments
updateAssignments :: Samples -> Int -> Assignments -> RVar Assignments
updateAssignments samples ncomps =
  updateAssignments' samples ncomps . paramsFromAssignments samples ncomps

-- | Likelihood of samples assignments under given model parameters
likelihood :: Samples -> Params -> Assignments -> Prob
likelihood samples params assignments =
  product $ V.toList
  $ V.map (\(k,x)->betaProb (params V.! k) x)
  $ V.zip assignments samples

-- | Maximum likelihood classification
classify :: Params -> Sample -> ComponentIdx
classify params x =
  fst $ V.maximumBy (compare `on` \(_,p)->betaProb p x) $ V.indexed params

