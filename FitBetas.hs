import System.Environment (getArgs)                
import Data.Number.LogFloat                 hiding (realToFrac)
import Data.Random.Lift
import qualified Data.Vector as V                
import Control.Applicative                
import Control.Monad                
import           Data.Random                 
import           Data.Random.Distribution.Beta
import           Data.Random.Distribution.Categorical
import System.Random.MWC (create)                 

import Numeric.MixtureModel.Beta

import "data-accessor" Data.Accessor       
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Plot.Histogram       
import Data.Colour
import Data.Colour.Names       
       
priors :: [(Weight, BetaParam)]       
priors = [ (0.5, paramFromMoments (0.1, 0.01))
         , (0.5, paramFromMoments (0.9, 0.01))
         ]

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
            $ plot_lines_style .> line_color ^= opaque red
            $ defaultPlotLines

main = do
  mwc <- create

  --[fname] <- getArgs
  --samples <- readSamples fname

  test <- V.fromList <$> sampleFrom mwc (testData 2000 dists)
  let samples = V.map snd test

  assignments0 <- sampleFrom mwc $ updateAssignments' samples (V.fromList priors)
  print $ V.map paramToMoments
        $ paramsFromAssignments samples 2 assignments0
  let f :: Assignments -> RVarT IO Assignments
      f a = do
        a' <- lift $ updateAssignments samples 2 a
        let params = estimateWeights a' $ paramsFromAssignments samples 2 a'
        lift $ print (logFromLogFloat $ likelihood samples params a' :: Double)
        return a'
  assignments <- sampleFrom mwc
                 $ replicateM' 100 f assignments0
  print $ V.map paramToMoments $ paramsFromAssignments samples 2 assignments

  let params = estimateWeights assignments
               $ paramsFromAssignments samples 2 assignments
      dist x = sum $ map (\(w,p)->w * realToFrac (betaProb p x)) $ V.toList params
  let layout = layout1_plots ^= [ Right $ histPlot samples
                                , Right $ functionPlot 100 (0.01,0.99) dist
                                ]
             $ defaultLayout1
  print params
  renderableToPDFFile (toRenderable layout) 640 480 "hi.pdf"

replicateM' :: Monad m => Int -> (a -> m a) -> a -> m a
replicateM' n f a | n < 1 = error "Invalid count"
replicateM' 1 f a = f a
replicateM' n f a = f a >>= replicateM' (n-1) f

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
