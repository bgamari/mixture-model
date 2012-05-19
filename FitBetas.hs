import System.Environment (getArgs)                
import Data.Number.LogFloat                 hiding (realToFrac)
import Data.Random.Lift
import qualified Data.Vector as V                
import Control.Applicative                
import           Data.Random                 
import           Data.Random.Distribution.Beta
import           Data.Random.Distribution.Categorical
import System.Random.MWC (create)                 

import Numeric.MixtureModel.Beta hiding (beta)

import "data-accessor" Data.Accessor       
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Plot.Histogram       
       
priors = [ paramFromMoments (0.1, 0.01)
         , paramFromMoments (0.9, 0.01)
         ]

weights :: [(Double, ComponentIdx)]       
weights = [(0.5, 0), (0.5, 1)]

readSamples :: FilePath -> IO (V.Vector Sample)
readSamples fname =
  (V.fromList . map read . lines) <$> readFile fname

histPlot :: V.Vector Sample -> Plot Sample Double
histPlot xs = plotNormedHist $ plot_hist_bins ^= 40
                             $ plot_hist_values ^= [V.toList xs]
                             $ defaultPlotHist

functionPlot :: (RealFrac x, Enum x) => Int -> (x, x) -> (x -> y) -> Plot x y
functionPlot n (a,b) f =
  let xs = [a,a+(b-a)/realToFrac n..b]
  in toPlot $ plot_lines_values ^= [map (\x->(x,f x)) xs]
            $ defaultPlotLines

main = do
  mwc <- create
  [fname] <- getArgs
  samples <- readSamples fname
  assignments0 <- sampleFrom mwc $ updateAssignments' samples 2 (V.fromList priors)
  print $ V.map paramToMoments
        $ paramsFromAssignments samples 2 assignments0
  let f :: Assignments -> RVarT IO Assignments
      f a = do
        a' <- lift $ updateAssignments samples 2 a
        let params = paramsFromAssignments samples 2 a'
        lift $ print (logFromLogFloat $ likelihood samples params a' :: Double)
        return a'
  assignments <- sampleFrom mwc
                 $ replicateM' 100 f assignments0
  print $ V.map paramToMoments $ paramsFromAssignments samples 2 assignments

  let params = paramsFromAssignments samples 2 assignments
      dist x = sum $ tr $ map (\p->betaProb p x) $ V.toList params
      g = (*0.01) . realToFrac . dist :: Double -> Double
  let layout = layout1_plots ^= [ Right $ functionPlot 100 (0,0.99) g
                                , Right $ histPlot samples
                                ]
             $ defaultLayout1
  renderableToPDFFile (toRenderable layout) 640 480 "hi.pdf"

replicateM' :: Monad m => Int -> (a -> m a) -> a -> m a
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a
replicateM' n f a = f a >>= replicateM' (n-1) f
