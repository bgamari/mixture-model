module Numeric.MixtureModel.Exponential ( -- * General data types
                                          Sample
                                        , SampleIdx
                                        , Samples
                                        , ComponentIdx
                                        , Assignments
                                        , Weight
                                        -- * Exponential parameters
                                        , ExpParam
                                        , ExpParams
                                        , ComponentParams
                                        , paramsFromAssignments
                                        -- * Exponential distribution
                                        , Prob
                                        , expProb
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
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import           Control.Monad.ST       

import           Data.Number.LogFloat hiding (realToFrac, isInfinite, log)
import           Data.Number.LogFloat.Vector
import           Numeric.SpecFunctions (logBeta)
import           Statistics.Sample (mean)
       
import           Data.Random                 
import           Data.Random.Distribution.Categorical
       
type Prob = LogFloat       
type Sample = Double
type SampleIdx = Int     
type ComponentIdx = Int     
type Weight = Double     
type ExpParam = Double -- Rate

-- k refers to number of components
-- N refers to number of samples
type Samples = V.Vector Sample                       -- length == N
type Assignments = V.Vector ComponentIdx             -- length == N
type ExpParams = V.Vector ExpParam                   -- length == K     
type ComponentParams = V.Vector (Weight, ExpParam)   -- length == K     
      
-- | `expProb lambda tau` is the probability of `tau` under Exponential
-- distribution defined by rate `lambda`
expProb :: ExpParam -> Sample -> Prob
expProb _ tau | tau < 0 = error "Exponential distribution undefined for tau<0"
expProb lambda tau = logToLogFloat $ log lambda - lambda * tau

-- | Exponential parameter from samples
paramFromSamples :: V.Vector Sample -> ExpParam
paramFromSamples v | V.null v = error "Can't estimate priors from no samples"
paramFromSamples v = 1 / mean v

-- | Exponential parameter for component given samples and their
-- component assignments
paramFromAssignments :: Samples -> Assignments -> ComponentIdx -> ExpParam         
paramFromAssignments samples assignments k =
  paramFromSamples $ V.map snd $ V.filter (\(k',_)->k==k') $ V.zip assignments samples

-- | Exponential parameters for all components given samples and their
-- component assignments
paramsFromAssignments :: Samples -> Int -> Assignments -> ExpParams
paramsFromAssignments samples ncomps assignments =
  V.fromList $ map (paramFromAssignments samples assignments) [0..ncomps-1]

-- | Draw a new assignment for a sample given beta parameters
drawAssignment :: ComponentParams -> Sample -> RVar ComponentIdx
drawAssignment params x =
  let probs = map (\(w,p)->realToFrac w * expProb p x) $ V.toList params
      lfIsInfinite :: LogFloat -> Bool
      lfIsInfinite = isInfinite . (fromLogFloat :: LogFloat -> Double)
  in case filter (lfIsInfinite . fst) $ zip probs [0..] of
        (x:_)     -> return $ snd x
        otherwise -> categorical
                     $ map (\(p,k)->(realToFrac $ p / sum probs :: Double, k))
                     $ zip probs [0..]
  
-- | `countIndices n v` is the list of counts
countIndices :: Int -> V.Vector Int -> V.Vector Int
countIndices n v = runST $ do
    accum <- V.thaw $ V.replicate n 0
    V.forM_ v $ \k -> do n' <- MV.read accum k
                         MV.write accum k $! n'+1
    V.freeze accum

-- | Estimate the component weights of a given set of parameters
estimateWeights :: Assignments -> ExpParams -> ComponentParams
estimateWeights assignments params =
  let counts = countIndices (V.length params) assignments
      norm = realToFrac $ V.length assignments
      weights = V.map (\n->realToFrac n / norm) counts
  in V.zip weights params

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
    V.product ( V.map (\(k,x)->expProb (snd $ params V.! k) x)
              $ V.zip assignments samples
              )
  * V.product (V.map (\k->realToFrac $ fst $ params V.! k) assignments)

-- | Maximum likelihood classification
classify :: ComponentParams -> Sample -> ComponentIdx
classify params x =
  fst
  $ V.maximumBy (compare `on` \(_,(w,p))->realToFrac w * expProb p x)
  $ V.indexed params

