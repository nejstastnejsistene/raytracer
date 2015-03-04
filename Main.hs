import Codec.PPM
import Data.AdditiveGroup
import Data.Cross
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import Data.VectorSpace
import Data.Word

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
    { materialColor       :: Pixel
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
    { sceneBackground :: Pixel
    , sceneShapes     :: [Shape]
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
normalVector (Plane n)           _ = rayDirection n
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

traceRay :: Scene -> Ray -> Pixel
traceRay s@(Scene bg shapes) r = computeShading (intersect r shapes)
  where
    computeShading [] = bg
    computeShading (Intersection _ t (Shape m g):_) = (diffuse + specular) ^* intensity
      where
        light = normalize (-1,0,1)
        lightIntensity = 1

        ip = evaluateRay r t
        diffuse = materialColor m ^* (normalVector g ip <.> light)
        specular = (0,0,0)

        ip' = evaluateRay (Ray ip light) (10 * epsilon)
        shadowRay = Ray ip' light
        intensity = case intersect shadowRay shapes of
            [] -> lightIntensity
            _  -> 0

-- Compute a ray from the camera that goes through the (x,y)th pixel.
castRay :: Camera -> (Integer,Integer) -> Ray
castRay (Camera pov@(Ray p v) up fl (vw,vh) (imgW,imgH)) (x,y) = ray
  where
    -- Center of view port and right vector.
    center = evaluateRay pov fl
    right = traceShow (v <.> up) $ normalize (v `cross3` up)
    -- Pixel size in terms of the coordinate system.
    pixelWidth = vw / fromIntegral imgW
    pixelHeight = vh / fromIntegral imgH
    -- Offsets from center of view port.
    hOff = (fromIntegral x + 0.5) * pixelWidth - vw / 2
    vOff = (fromIntegral y + 0.5) * pixelWidth - vh / 2
    -- Point in view port through which to cast the ray.
    p' = center - up^*vOff + right^*hOff
    ray = Ray p $ normalize (p' - p)

-- Render a scene from a camera into an array of pixels.
renderScene :: Scene -> Camera -> [Pixel]
renderScene scene camera = do
    let (w,h) = cameraResolution camera
    y <- [0..h-1]
    x <- [0..w-1]
    return $ traceRay scene $ castRay camera (x,y)

-- Output a scene from a camera into a ppm file.
renderImage :: String -> Scene -> Camera -> IO ()
renderImage path scene camera = writePPM path imgSize pixelData
  where
    imgSize = cameraResolution camera
    pixelData = map castPixel (renderScene scene camera)
    castPixel (r,g,b) = (round r, round g, round b)

main :: IO ()
main = renderImage "out.ppm" scene camera
  where
    scene = Scene (0xe6,0xff,0xff) (redBall : mirrorBall : chessBoard)
    redBall    = Shape (Material (255,0,0)) $ Sphere (2,3,0.5) 0.25
    mirrorBall = Shape (Material (255,255,255)) $ Sphere (4,5,2) 1.5
    chessBoard = concat $ do
        xi <- [0..7]
        yi <- [0..7]
        let m = Material $ if even xi /= even yi then (0xde,0xbf,0x83) else (0x9f,0x71,0x19)
            x = fromIntegral xi
            y = fromIntegral yi
            t1 = Triangle (x,y,0) (x+1,y,0) (x,y+1,0)
            t2 = Triangle (x+1,y+1,0) (x,y+1,0) (x+1,y,0)
        return [Shape m t1, Shape m t2]
    cameraFocus = (4,4,1)
    cameraPos = (1,-2,3)
    cameraDir@(dx,dy,_) = traceShowId $ normalize $ cameraFocus - cameraPos
    cameraUp = traceShowId $ normalize $ cameraDir `cross3` traceShowId (-dy,dx,0)
    camera = Camera { cameraPointOfView  = Ray cameraPos cameraDir
                    , cameraUpDirection  = cameraUp
                    , cameraFocalLength  = 1
                    , cameraViewPortSize = (1.6,0.9)
                    , cameraResolution   = (320,180)
                    }
