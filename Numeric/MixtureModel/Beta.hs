module Numeric.MixtureModel.Beta (
                                 ) where
                              
import Control.Monad.ST       
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Number.LogFloat       
import Statistics.Math (logBeta)       
import Statistics.Sample (meanVarianceUnb)       
       
type Prob = LogFloat       
type Sample = Double
type SampleIdx = Int     
type ComponentIdx = Int     
type BetaParam = (Double, Double)

-- k refers to number of components
-- N refers to number of samples
type Samples = V.Vector Sample                   -- length == N
type Assignments s = V.Vector s ComponentIdx     -- length == N
type MAssignments s = VM.STVector s ComponentIdx -- length == N
type MCounts s = VM.STVector s Int               -- length == k

paramsFromCounts :: V.Vector Sample -> BetaParam
paramsFromCounts xs =
  let (xbar, v) = meanVarianceUnb xs
      c = (xbar * (1 - xbar) / v - 1)
      alpha = xbar * c
      beta = (1 - xbar) * c
  in (alpha, beta)
      
modifyVM :: PrimMonad m => VM.MVector (PrimState m) a -> Int -> (a -> a) -> m ()
modifyVM v i f = VM.read i >>= VM.write i . f

beta :: Double -> Double -> LogFloat
beta a b = logToLogFloat $ logBeta a b

betaProb :: Param -> Sample -> Prob
betaProb (a,b) x = 1/beta a b * logFloat (x**(a-1)) * logFloat ((1-x)**(b-1))

paramsFromAssignments :: Samples -> Assignments -> ComponentIdx -> BetaParam         
paramsFromAssignments samples assignments k =
  paramsFromCounts $ V.filter (\(k',_)->k==k') $ V.zip assignments samples

drawAssignment :: Monad m => Samples -> MCounts -> MAssignments -> SampleIdx -> RVarT m ComponentIdx
drawAssignment samples counts assignments i =
  let x = samples V.! i
  in categorical
     $ map (\(k,p)->(k, betaProb (paramsFromAssignments samples assignments k) x)
     $ V.toList $ V.indexed params
  
updateAssignment :: Samples -> MCounts -> MAssignments -> SampleIdx -> RVarT (ST s) ()
updateAssignment samples counts assignments i = do
  c <- VM.read assignments i                 
  modifyVM counts c (-1)
  c' <- drawAssignment samples params assignments i
  modifyVM counts c' (+1)

updateAssignments :: Samples -> Counts -> Assignments -> RVarT (ST s) ()
updateAssignments samples counts assignments = do
  undefined                   
                  
--learnModel :: Samples -> Assignments -> 

likelihood :: Samples -> Assignments -> Prob
likelihood = undefined           

