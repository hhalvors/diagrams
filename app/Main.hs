{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG (renderSVG)


-- Deformation φ: identity on boundary, max push at center, horizontal only
phi :: Double -> P2 Double -> P2 Double
phi r p@(P (V2 x y))
  | dist >= r  = p
  | otherwise  = p .+^ r2 (0.5 * bump, 0)  -- push right only
  where
    v     = p .-. origin
    dist  = norm v
    t     = dist / r
    bump  = (1 - t^2)^2  -- max at center, 0 at edge

-- Helper to create a grid line from a list of points
gridLine :: [P2 Double] -> Diagram B
gridLine pts = fromVertices pts # lc gray # lw thin

-- Grid points for rendering
xRange :: [Double]
xRange = [-2.5, -2.4 .. 2.5]
yLines = [-2, -1.5 .. 2]
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

-- Original trajectory points
trajectoryPoints :: [P2 Double]
trajectoryPoints = map p2 [(0, -2), (0.2, -1), (0.5, 0), (0.7, 1), (0.9, 2)]

trajectoryCurve :: [P2 Double]
trajectoryCurve = [ p2 (0.05 * t^2, t) | t <- [-2, -1.8 .. 2] ]

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
          , norm d > 0  -- only draw arrows for non-zero displacements
          ]
  where
    -- Sample a grid of points; adjust these ranges and steps as needed.
    samplePoints = [ p2 (x, y)
                   | x <- [-2.5, -2.0 .. 2.5]
                   , y <- [-2.0, -1.5 .. 2.0]
                   ]
  
main :: IO ()
main = mainWith holeArgument

