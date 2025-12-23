{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import System.Environment (getArgs, withArgs)
import System.Exit (die)

import FiniteFourier (plotStateAndSpectrum)
import Data.Complex (Complex((:+)), realPart, imagPart)

phi :: Double -> P2 Double -> P2 Double
phi r p@(P (V2 x y))
  | dist >= r  = p
  | otherwise  = p .+^ r2 (0.5 * bump, 0)
  where
    v     = p .-. origin
    dist  = norm v
    t     = dist / r
    bump  = if t < 1
              then exp ( - t^2 / (1 - t^2) )
              else 0

-- Helper to create a grid line from a list of points
gridLine :: [P2 Double] -> Diagram B
gridLine pts = fromVertices pts # lc gray # lw thin

-- Grid points for rendering
xRange :: [Double]
xRange = [-2.5, -2.4 .. 2.5]
yLines :: [Double]
yLines = [-2, -1.5 .. 2]
xLines :: [Double]
xLines = yLines

grid :: [Diagram B]
grid =
  [ gridLine [p2 (x, y) | x <- xRange] | y <- yLines ] ++
  [ gridLine [p2 (x, y) | y <- xRange] | x <- xLines ]

-- Deform the grid using φ
deformedGrid :: Double -> [Diagram B]
deformedGrid r =
  [ gridLine [phi r (p2 (x, y)) | x <- xRange] | y <- yLines ] ++
  [ gridLine [phi r (p2 (x, y)) | y <- xRange] | x <- xLines ]

-- A straight line through the origin, tilted right with slope m
trajectoryCurve :: [P2 Double]
trajectoryCurve =
  [ p2 (m * t, t)
  | t <- [-2, -1.9 .. 2]
  ]
  where
    m = -0.1

trajectory :: Diagram B
trajectory = fromVertices trajectoryCurve # lw thick # lc blue

deformedTrajectory :: Double -> Diagram B
deformedTrajectory r =
  fromVertices (map (phi r) trajectoryCurve) # lw thick # lc blue

-- Circle showing the deformation region
hole :: Double -> Diagram B
hole r = circle r # lc red # dashingG [0.1, 0.1] 0

-- Final composed side-by-side diagram
holeArgument :: Diagram B
holeArgument =
  (mconcat grid <> trajectory <> hole 1.2)
  |||
  (mconcat (deformedGrid 1.2) <> deformedTrajectory 1.2 <> hole 1.2)
  # centerXY
  # pad 1.1

-- Displacement vectors showing the transformation applied by φ.
displacementVectors :: Double -> Diagram B
displacementVectors r =
  mconcat [ arrowAt p d
          | p <- samplePoints
          , let d = phi r p .-. p
          , norm d > 0
          ]
  where
    samplePoints = [ p2 (x, y)
                   | x <- [-2.5, -2.0 .. 2.5]
                   , y <- [-2.0, -1.5 .. 2.0]
                   ]

--------------------------------------------------------------------------------
-- NEW: a couple extra diagrams + a selector
--------------------------------------------------------------------------------

-- Example: show the vector field alone (or overlay it)
vectorFieldOnly :: Diagram B
vectorFieldOnly =
  displacementVectors 1.2
  <> hole 1.2
  # centerXY
  # pad 1.1

holeWithVectors :: Diagram B
holeWithVectors =
  (mconcat grid <> trajectory <> hole 1.2 <> displacementVectors 1.2)
  # centerXY
  # pad 1.1

-- A simple “localized” real-valued state on Z_n (circular Gaussian-ish bump)
bumpState :: Int -> Int -> Double -> [Complex Double]
bumpState n x0 sigma =
  normalize
    [ exp (-(fromIntegral (circDist n x x0))^2 / (2*sigma*sigma)) :+ 0
    | x <- [0..n-1]
    ]
  where
    circDist m a b =
      let d = abs (a - b)
      in min d (m - d)

    normalize psi =
      let s = sqrt (sum [ let r = realPart z; i = imagPart z in r*r + i*i | z <- psi ])
      in map (/ (s :+ 0)) psi

fftDemo :: Diagram B
fftDemo =
  let n   = 4
      psi = [1 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0]   -- (1,0,0,0)
  in plotStateAndSpectrum n psi      


-- Registry: add new ones here as you grow the project
diagramByName :: String -> Maybe (Diagram B)
diagramByName name =
  case name of
    "hole"        -> Just holeArgument
    "vectors"     -> Just vectorFieldOnly
    "hole+vectors"-> Just holeWithVectors
    "fft"         -> Just fftDemo
    _             -> Nothing

usage :: String
usage = unlines
  [ "Usage:"
  , "  cabal run diagrams-exe -- <diagram-name> [diagrams-svg-options]"
  , ""
  , "Diagram names:"
  , "  hole"
  , "  vectors"
  , "  hole+vectors"
  , ""
  , "Examples:"
  , "  cabal run diagrams-exe -- hole -o hole.svg -w 900"
  , "  cabal run diagrams-exe -- vectors -o vectors.svg -w 900"
  , "  cabal run diagrams-exe -- hole+vectors -o hv.svg -w 900"
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> die usage
    (name:rest) ->
      case diagramByName name of
        Nothing  -> die usage
        Just dia -> withArgs rest (mainWith dia)

