module Numeric.MixtureModel.Beta ( -- * General data types
                                   Sample
                                 , SampleIdx
                                 , Samples
                                 , ComponentIdx
                                 , Assignments
                                 , Weight
                                 -- * Beta parameters
                                 , BetaParam (..)
                                 , BetaParams
                                 , ComponentParams
                                 , paramFromMoments
                                 , paramToMoments
                                 , paramsFromAssignments
                                 , paramToMode
                                 -- * Beta distribution
                                 , Prob
                                 , betaProb
                                 -- * Gibbs sampling
                                 , estimateWeights
                                 , updateAssignments
                                 , updateAssignments'
                                 -- * Likelihood
                                 , likelihood
                                 -- * Classification
                                 , classify
                                 ) where

import           Data.Function (on)
import qualified Data.Vector as VB
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MV
import           Control.Monad.ST

import           Numeric.Log hiding (sum)
import           Numeric.SpecFunctions (logBeta)
import           Statistics.Sample (meanVarianceUnb)

import           Data.Random
import           Data.Random.Distribution.Categorical

type Prob = Log Double
type Sample = Double
type SampleIdx = Int
type ComponentIdx = Int
type Weight = Double
data BetaParam = BetaParam { bpAlpha :: !Double -- ^ Alpha
                           , bpBeta  :: !Double -- ^ Beta
                           }
               deriving (Show, Eq, Ord)

-- k refers to number of components
-- N refers to number of samples
type Samples = VU.Vector Sample                         -- length == N
type Assignments = VU.Vector ComponentIdx               -- length == N
type BetaParams = VB.Vector BetaParam                   -- length == K
type ComponentParams = VB.Vector (Weight, BetaParam)    -- length == K

beta :: Double -> Double -> Log Double
beta a b = Exp $ logBeta a b

-- | `betaProb (a,b) x` is the probability of `x` under Beta
-- distribution defined by parameters `a` and `b`
betaProb :: BetaParam -> Sample -> Prob
betaProb (BetaParam a b) x =
    1/beta a b * realToFrac (x**(a-1)) * realToFrac ((1-x)**(b-1))

-- | Beta parameter from sample mean and variance
paramFromMoments :: Double            -- ^ mean
                 -> Double            -- ^ variance
                 -> Maybe BetaParam
paramFromMoments xbar v
  | c < 0      = Nothing
  | otherwise  = Just $ BetaParam (xbar * c) ((1 - xbar) * c)
  where c = xbar * (1 - xbar) / v - 1

-- | Mean and variance of the given beta parameter
paramToMoments :: BetaParam -> (Double, Double)
paramToMoments (BetaParam a b) =
  let mean = a / (a+b)
      var  = a*b / (a+b)^2 / (a+b+1)
  in (mean, var)

-- | The mode of the given beta parameter
paramToMode :: BetaParam -> Double
paramToMode (BetaParam a b) = (a - 1) / (a + b - 2)

-- | Beta parameter from samples
paramFromSamples :: VU.Vector Sample -> BetaParam
paramFromSamples v | V.null v = error "Numeric.MixtureModel.Beta: Can't estimate priors from no samples"
paramFromSamples v =
    case uncurry paramFromMoments $ meanVarianceUnb v of
      Just a  -> a
      Nothing -> error "Numeric.MixtureModel.Beta: Somehow we have a negative variance"

-- | Beta parameter for component given samples and their component assignments
paramFromAssignments :: Samples -> Assignments -> ComponentIdx -> BetaParam
paramFromAssignments samples assignments k =
  paramFromSamples $ V.map snd $ V.filter (\(k',_)->k==k') $ V.zip assignments samples

-- | Beta parameters for all components given samples and their component assignments
paramsFromAssignments :: Samples -> Int -> Assignments -> BetaParams
paramsFromAssignments samples ncomps assignments =
  V.fromList $ map (paramFromAssignments samples assignments) [0..ncomps-1]

-- | Draw a new assignment for a sample given beta parameters
drawAssignment :: ComponentParams -> Sample -> RVar ComponentIdx
drawAssignment params x =
  let probs = map (\(w,p)->realToFrac w * betaProb p x) $ V.toList params
  in case filter (isInfinite . ln . fst) $ zip probs [0..] of
        (x:_)     -> return $ snd x
        otherwise -> categorical
                     $ map (\(p,k)->(realToFrac $ p / sum probs :: Double, k))
                     $ zip probs [0..]

countIndices :: Int -> VU.Vector Int -> VU.Vector Int
countIndices n v = runST $ do
    accum <- V.thaw $ VU.replicate n 0
    V.forM_ v $ \k -> do n' <- MV.read accum k
                         MV.write accum k $! n'+1
    V.freeze accum

-- | Estimate the component weights of a given set of parameters
estimateWeights :: Assignments -> BetaParams -> ComponentParams
estimateWeights assignments params =
  let counts = countIndices (V.length params) assignments
      norm = realToFrac $ V.length assignments
      weights = V.map (\n->realToFrac n / norm) counts
  in V.zip (V.convert weights) params

-- | Sample assignments for samples under given parameters
updateAssignments' :: Samples -> ComponentParams -> RVar Assignments
updateAssignments' samples params =
  V.mapM (drawAssignment params) samples

-- | Gibbs update of sample assignments
updateAssignments :: Samples -> Int -> Assignments -> RVar Assignments
updateAssignments samples ncomps assignments =
  updateAssignments' samples
  $ estimateWeights assignments
  $ paramsFromAssignments samples ncomps assignments

-- | Likelihood of samples assignments under given model parameters
likelihood :: Samples -> ComponentParams -> Assignments -> Prob
likelihood samples params assignments =
    V.product ( V.map (\(k,x)->betaProb (snd $ params V.! k) x)
              $ V.zip assignments samples
              )
  * V.product (V.map (\k->realToFrac $ fst $ params V.! k) assignments)

-- | Maximum likelihood classification
classify :: ComponentParams -> Sample -> ComponentIdx
classify params x =
  fst
  $ V.maximumBy (compare `on` \(_,(w,p))->realToFrac w * betaProb p x)
  $ V.indexed params
