import Codec.PPM
import Data.AdditiveGroup
import Data.Cross
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import Data.VectorSpace
import Data.Word
import System.Random

import Debug.Trace
traceShowId a = trace (show a) a

epsilon :: Float
epsilon = 10e-6

type Vector = (Float,Float,Float)

data Ray = Ray
    { rayOrigin    :: Vector
    , rayDirection :: Vector
    } deriving (Show)

data Shape = Shape
    { shapeMaterial :: Material
    , shapeGeometry :: Geometry
    }

data Material = Material
    { materialColor          :: Pixel
    , materialSharpness      :: Integer
    , materialReflectiveness :: Float
    }

data Geometry = Plane Ray
              | Triangle Vector Vector Vector
              | Sphere Vector Float

data Intersection = Intersection
    { intersectionRay   :: Ray
    , intersectionTime  :: Float
    , intersectionShape :: Shape
    }


data Scene = Scene
    { sceneBackground   :: Pixel
    , sceneAmbientLight :: Float
    , sceneShapes       :: [Shape]
    }

data Camera = Camera
            { cameraPointOfView  :: Ray
            , cameraUpDirection  :: Vector
            , cameraFocalLength  :: Float
            , cameraViewPortSize :: (Float,Float)
            , cameraResolution   :: (Integer,Integer)
            }

type Pixel = Vector

type PixelCoord = (Integer,Integer)
type CanvasSize = (Integer,Integer)

-- Normalize a vector.
normalize :: Vector -> Vector
normalize v = v ^/ magnitude v

-- Evaluate the point at distance t from the ray's origin.
evaluateRay :: Ray -> Float -> Vector
evaluateRay (Ray origin direction) t = origin + direction ^* t

-- Determine the normal vector of a surface at a point.
normalVector :: Geometry -> Vector -> Vector
normalVector (Plane n)           _ = normalize $ rayDirection n
normalVector (Triangle p1 p2 p3) _ = normalize $ (p2 - p1) `cross3` (p3 - p1)
normalVector (Sphere center _)   p = normalize (p - center)

intersect :: Ray -> [Shape] -> [Intersection]
intersect r shapes = sortBy (comparing intersectionTime) intersections
  where intersections = concatMap (intersect' r) shapes

intersect' :: Ray -> Shape -> [Intersection]
intersect' r s@(Shape m g) = [ Intersection r t s | t <- intersect'' r g ]

intersect'' :: Ray -> Geometry -> [Float]
intersect'' r@(Ray p v) g = case g of
    Plane (Ray off n) -> if hit then [t] else []
      where
        cos = v <.> n
        t = ((off - p) <.> n) / cos
        hit = abs cos > epsilon && t > -epsilon
    Triangle p1 p2 p3 -> case intersect'' r $ Plane (Ray p1 n) of
        [] -> []
        [t] -> if hit then [t] else []
          where
            q = evaluateRay r t
            a  = (p2-q) `cross3` (p3-q) <.> n
            b  = (p3-q) `cross3` (p1-q) <.> n
            g' = (p1-q) `cross3` (p2-q) <.> n
            hit = a > -epsilon && b > -epsilon && g' > -epsilon
      where n = normalVector g p1 
    Sphere center radius -> [ t | t <- [tMid - dt, tMid + dt], d2 < r2 && t > -epsilon ] 
      where
        l = center - p -- Vector torwards the center.
        tMid = l <.> v -- Distance to nearest point on ray the center.
        d2 = l <.> l - tMid * tMid -- Distance from ray to center squared.
        r2 = radius * radius
        dt = sqrt (r2 - d2)

traceRay :: Scene -> Ray -> Integer -> Pixel
traceRay s r 0 = (0,0,0)
traceRay s@(Scene bg ambient shapes) r depth = computeShading (intersect r shapes)
  where
    computeShading [] = bg
    computeShading (Intersection _ t s':_) = ambient*^(1,1,1) + total
      where
        light = normalize (-0.5,-1,1)
        lightIntensity = 1

        Material color sharpness ref = shapeMaterial s'
        ip = evaluateRay r t
        n = normalize $ normalVector (shapeGeometry s') ip
        diffuse = color ^* (n <.> light)

        v = - normalize (rayDirection r)
        r' = normalize ((2 *^ (n <.> light) *^ n) - light)
        specular = case sharpness of
            0 -> (0,0,0)
            _ -> (1,1,1) ^* (v <.> r')^sharpness
        recursive = traceRay s (Ray (evaluateRay (Ray ip r') (10*epsilon)) r') (depth - 1)
        total = intensity *^ (1-ref) *^ (diffuse + specular) + ref *^ recursive

        ip' = evaluateRay (Ray ip light) (10 * epsilon)
        shadowRay = Ray ip' light
        intensity = case intersect shadowRay shapes of
            [] -> lightIntensity
            _  -> ambient

-- Compute a ray from the camera that goes through the (x,y)th pixel.
castRay :: Camera -> (Float,Float) -> Ray
castRay (Camera pov@(Ray p v) up fl (vw,vh) (imgW,imgH)) (x,y) = ray
  where
    -- Center of view port and right vector.
    center = evaluateRay pov fl
    right = normalize (v `cross3` up)
    -- Pixel size in terms of the coordinate system.
    pixelWidth = vw / fromIntegral imgW
    pixelHeight = vh / fromIntegral imgH
    -- Offsets from center of view port.
    hOff = (x + 0.5) * pixelWidth - vw / 2
    vOff = (y + 0.5) * pixelWidth - vh / 2
    -- Point in view port through which to cast the ray.
    p' = center - up^*vOff + right^*hOff
    ray = Ray p $ normalize (p' - p)

-- Render a scene from a camera into an array of pixels.
renderScene :: Scene -> Camera -> StdGen -> [Pixel]
renderScene scene camera gen = do
    let (w,h) = cameraResolution camera
    y <- [0..h-1]
    x <- [0..w-1]
    let samples = do i <- [0..100]
                     let (dx,_) = randomR (0,1) gen
                         (dy,_) = randomR (0,1) gen
                         x' = fromIntegral x + dx - 0.5
                         y' = fromIntegral y + dy - 0.5
                     return $ traceRay scene (castRay camera (x',y')) 3
    return $ (sum samples) ^/ (fromIntegral $ length samples)

-- Output a scene from a camera into a ppm file.
renderImage :: String -> Scene -> Camera -> IO ()
renderImage path scene camera = render >>= writePPM path imgSize
  where
    imgSize = cameraResolution camera
    render = do
        gen <- getStdGen
        return $ fmap castPixel (renderScene scene camera gen)
    castPixel (r,g,b) = (cast r, cast g, cast b)
    cast = round . (255*) . clip
    clip x | x < 0 = 0
           | x > 1 = 1
           | otherwise = x

main :: IO ()
main = renderImage "out.ppm" scene camera
  where
    scene = Scene (0.9,1,1) 0 (redBall : mirrorBall : chessBoard)
    redBall = Shape (Material(1,0,0) 0 0) $ Sphere (2,5,0.5) 0.5
    mirrorBall = Shape (Material (1,1,1) 0 0.99) $ Sphere (4,5,2) 1.5
    chessBoard = concat $ do
        xi <- [0..7]
        yi <- [0..7]
        let m = Material (if even xi /= even yi then (0.87,0.75,0.51) else (0.62,0.44,0.10)) 100 0.2
            x = fromIntegral xi
            y = fromIntegral yi
            t1 = Triangle (x,y,0) (x+1,y,0) (x,y+1,0)
            t2 = Triangle (x+1,y+1,0) (x,y+1,0) (x+1,y,0)
        return [Shape m t1, Shape m t2]
    cameraFocus = (4,4,1)
    cameraPos = (1,-2,3)
    cameraDir@(dx,dy,_) = normalize $ cameraFocus - cameraPos
    cameraUp = normalize $ cameraDir `cross3` (-dy,dx,0)
    camera = Camera { cameraPointOfView  = Ray cameraPos cameraDir
                    , cameraUpDirection  = cameraUp
                    , cameraFocalLength  = 1
                    , cameraViewPortSize = (1.6,0.9)
                    , cameraResolution   = (640,360)
                    }
