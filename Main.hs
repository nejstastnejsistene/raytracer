import Codec.PPM
import Data.AdditiveGroup
import Data.Cross
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import Data.VectorSpace
import Data.Word

import Debug.Trace


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
    { materialColor   :: Pixel
    , materialOpacity :: Float
    }

data Geometry = Plane Ray
              | Triangle Vector Vector Vector

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
evaluateRay (Ray origin direction) t = origin ^+^ direction ^* t

-- Calculate where along the ray (if at all) it intersects a shape.
intersect :: Ray -> Shape -> Maybe Intersection
intersect r s@(Shape m geo) = case intersect' r geo of
    Just t -> Just (Intersection r t s)
    Nothing -> Nothing

intersect' :: Ray -> Geometry -> Maybe Float
intersect' r@(Ray p v) geo = case geo of
    Plane (Ray off n) -> if hit then Just t else Nothing
      where
        cos = v <.> n
        t = ((off ^-^ p) <.> n) / cos
        hit = abs cos > epsilon && t > -epsilon
    Triangle p1 p2 p3 -> case intersect' r (Plane (Ray p1 n)) of
        Nothing -> Nothing
        Just t -> if hit then Just t else Nothing
          where
            q = evaluateRay r t
            area = (e1 `cross3` e2) <.> n
            a = (p2^-^q) `cross3` (p3^-^q) <.> n
            b = (p3^-^q) `cross3` (p1^-^q) <.> n
            g = (p1^-^q) `cross3` (p2^-^q) <.> n
            hit = a > -epsilon && b > -epsilon && g > -epsilon
      where
        e1 = p2 ^-^ p1
        e2 = p3 ^-^ p1
        n = normalize (e1 `cross3` e2)

normalVector :: Geometry -> Vector
normalVector (Plane n) = rayDirection n
normalVector (Triangle p1 p2 p3) = undefined

traceRay :: Scene -> Ray -> Pixel
traceRay s@(Scene bg shapes) r@(Ray p v) = pixel
  where
    intersections = mapMaybe (intersect r) shapes
    sorted = sortBy (comparing intersectionTime) intersections
    pixel = computeShading sorted

    computeShading [] = bg
    computeShading (x:xs) = fg ^+^ bg
      where
        o = materialOpacity $ shapeMaterial $ intersectionShape x
        fg = o *^ computeShading' x
        bg = (1-o) *^ computeShading xs

    computeShading' = materialColor . shapeMaterial . intersectionShape

-- Compute a ray from the camera that goes through the (x,y)th pixel.
castRay :: Camera -> (Integer,Integer) -> Ray
castRay (Camera pov@(Ray p v) up fl (vw,vh) (imgW,imgH)) (x,y) = ray
  where
    -- Center of view port.
    center = evaluateRay pov fl
    -- Vectors down and to the right along the view port.
    down  = normalize (center ^+^ (-2) *^ up)
    right = normalize (v `cross3` up)
    -- Pixel size in terms of the coordinate system.
    pixelWidth = vw / fromIntegral imgW
    pixelHeight = vh / fromIntegral imgH
    -- Offsets from center of view port.
    hOff = (fromIntegral x + 0.5) * pixelWidth - vw / 2
    vOff = (fromIntegral y + 0.5) * pixelWidth - vh / 2
    -- Point in view port through which to cast the ray.
    p'= center ^+^ (down^*vOff) ^+^ (right^*hOff)
    ray = Ray p $ normalize (p' ^-^ p)

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
    -- A plane that functions as the floor of the scene.
    shape = Shape material plane
    material = Material (0,255,0) 0.5
    plane = Plane $ Ray (0,-1,0) $ normalize (0,1,0)
    shape' = Shape material' plane'
    material' = Material (255,0,0) 1
    plane' = Plane $ Ray (0,0,-1) $ normalize (0,0,1)
    shape'' = Shape material'' triangle
    material'' = Material (0,0,255) 1
    triangle = Triangle (0,-1,-1) (5,-1,1) (5,1,-1)
    scene = Scene { sceneBackground = (0,0,0)
                  , sceneShapes     = [shape, shape', shape'']
                  }
    -- Camera POV is behind the YZ-plane, with the Z-axis as up.
    camera = Camera { cameraPointOfView  = Ray (-1,0,0) (1,0,0)
                    , cameraUpDirection  = (0,0,1)
                    , cameraFocalLength  = 1
                    , cameraViewPortSize = (2,2)
                    , cameraResolution   = (200,200)
                    }
