module Numeric.MixtureModel.Exponential ( -- * General data types
                                          Sample
                                        , SampleIdx
                                        , Samples
                                        , ComponentIdx
                                        , Assignments
                                        , Weight
                                        -- * Exponential parameters
                                        , Rate, Beta
                                        , Exponential(..)
                                        , ComponentParams
                                        , paramFromSamples
                                        , paramsFromAssignments
                                        -- * Exponential distribution
                                        , Prob
                                        , prob
                                        , tauMean, tauVariance
                                        -- * Gibbs sampling
                                        , estimateWeights
                                        , updateAssignments
                                        -- * Score
                                        , scoreAssignments
                                        -- * Classification
                                        , classify
                                        ) where
                              
import           Control.Monad.ST
import           Data.Function (on)
import qualified Data.Vector as VB
import           Data.Vector.Algorithms.Heap
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as V

import           Data.Number.LogFloat hiding (realToFrac, isInfinite, log)
import           Data.Number.LogFloat.Vector
import           Numeric.SpecFunctions (logBeta)
import           Statistics.Sample (mean)
import           Numeric.Newton
import           Math.Gamma (gamma)
       
import           Data.Random hiding (gamma)
import           Data.Random.Distribution.Categorical
                 
type Prob = LogFloat       
type Sample = Double
type SampleIdx = Int     
type ComponentIdx = Int     
type Weight = Double     

type Rate = Double -- | The rate parameter
type Beta = Double -- | The stretching parameter
data Exponential = Exp Rate
                 | StretchedExp Rate Beta
                 deriving (Show, Read, Eq)
                 
-- k refers to number of components
-- N refers to number of samples
type Samples = V.Vector Sample                          -- length == N
type Assignments = V.Vector ComponentIdx                -- length == N
type ComponentParams = VB.Vector (Weight, Exponential)  -- length == K     
      
-- | `expProb lambda tau` is the probability of `tau` under Exponential
-- distribution defined by rate `lambda`
prob :: Exponential -> Sample -> Prob
prob _ tau | tau < 0 = error "Exponential distribution undefined for tau<0"
prob (StretchedExp lambda 1) tau = prob (Exp lambda) tau
prob (StretchedExp lambda beta) tau =
    logToLogFloat $ log beta + (beta-1) * log tau + beta * log lambda - (tau * lambda)**beta
prob (Exp lambda) tau = logToLogFloat $ log lambda - lambda * tau

-- | Mean of the given distribution
tauMean :: Exponential -> Double
tauMean (Exp lambda) = 1 / lambda
tauMean (StretchedExp lambda beta) = gamma (1/beta) / beta / lambda

-- | Variance of the given distribution
tauVariance :: Exponential -> Double
tauVariance (Exp lambda) = 1/lambda^2
tauVariance (StretchedExp lambda beta) = 2 * gamma (2/beta) / lambda^2 / beta            

-- | Exponential parameter from samples
paramFromSamples :: Exponential -> V.Vector Sample -> Exponential
paramFromSamples _ v | V.null v = error "Can't estimate parameters from no samples"
paramFromSamples (Exp _) v = Exp $ 1 / mean v
paramFromSamples (StretchedExp _ _) v =
    let v' = runST $ do a <- V.thaw v
                        sort a
                        V.freeze a
        n = realToFrac $ V.length v'
        tn = V.head v'
        s beta = V.sum $ V.map (\t->t**beta - tn**beta) v'
        betaOpt beta = let num = V.sum $ V.map (\t->t**beta * log t - tn**beta * log tn) v'
                       in num / s beta - V.sum (V.map log v') / n - 1 / beta
        beta' = findRoot 1e-5 betaOpt (numericalDeriv 1e-5 betaOpt 1e-2) 0.5
        lambda' = (s beta' / n)**(-1/beta')
    in StretchedExp lambda' beta'

-- | Exponential parameter for component given samples and their
-- component assignments
paramFromAssignments :: Samples -> Assignments -> (ComponentIdx, Exponential) -> Exponential
paramFromAssignments samples assignments (k,p) =
  paramFromSamples p $ V.map snd $ V.filter (\(k',_)->k==k') $ V.zip assignments samples

-- | Exponential parameters for all components given samples and their
-- component assignments
paramsFromAssignments :: Samples -> VB.Vector Exponential -> Assignments -> VB.Vector Exponential
paramsFromAssignments samples params assignments =
  VB.map (paramFromAssignments samples assignments) $ VB.indexed params

-- | Draw a new assignment for a sample given beta parameters
drawAssignment :: ComponentParams -> Sample -> RVar ComponentIdx
drawAssignment params x =
  let probs = map (\(w,p)->realToFrac w * prob p x) $ VB.toList params
      lfIsInfinite :: LogFloat -> Bool
      lfIsInfinite = isInfinite . (fromLogFloat :: LogFloat -> Double)
  in case filter (lfIsInfinite . fst) $ zip probs [0..] of
        (x:_)     -> return $ snd x
        otherwise -> categorical
                     $ map (\(p,k)->(realToFrac $ p / sum probs :: Double, k))
                     $ zip probs [0..]
  
-- | `countIndices n v` is the list of counts
countIndices :: Int -> V.Vector Int -> VB.Vector Int
countIndices n v = runST $ do
    accum <- VB.thaw $ VB.replicate n 0
    V.forM_ v $ \k -> do n' <- MV.read accum k
                         MV.write accum k $! n'+1
    VB.freeze accum

-- | Estimate the component weights of a given set of parameters
estimateWeights :: Assignments -> VB.Vector Exponential -> ComponentParams
estimateWeights assignments params =
  let counts = countIndices (VB.length params) assignments
      norm = realToFrac $ V.length assignments
      weights = VB.map (\n->realToFrac n / norm) counts
  in VB.zip weights params

-- | Sample new assignments for observations under given model parameters
updateAssignments :: Samples -> ComponentParams -> RVar Assignments
updateAssignments samples params =
  V.mapM (drawAssignment params) samples

-- | "Likelihood" of sample assignments under given model
-- parameters. Note that the exponential distribution is a density
-- function and as such this will give an unnormalized result unless
-- multiplied by dtau^N
scoreAssignments :: Samples -> ComponentParams -> Assignments -> Prob
scoreAssignments samples params assignments =
    V.product
    $ V.map (\(k,x)->let (w,p) = params VB.! k
                     in realToFrac w * prob p x
            )
    $ V.zip assignments samples

-- | Maximum likelihood classification
classify :: ComponentParams -> Sample -> ComponentIdx
classify params x =
  fst
  $ VB.maximumBy (compare `on` \(_,(w,p))->realToFrac w * prob p x)
  $ VB.indexed params

