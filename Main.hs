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

type Pixel = (Word8,Word8,Word8)

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
intersect (Ray p v) s@(Shape m geo) = case geo of
    Plane r@(Ray off n) -> case hit of
        True -> Just (Intersection r t s)
        False -> Nothing
      where
        cos = v <.> n
        t = ((off ^-^ p) <.> n) / cos
        hit = abs cos > epsilon && t > -epsilon
    Triangle p1 p2 p3 -> undefined

traceRay :: Scene -> Ray -> Pixel
traceRay (Scene bg shapes) r@(Ray p v) = pixel
  where
    intersections = mapMaybe (intersect r) shapes
    sorted = sortBy (comparing intersectionTime) intersections
    -- use nearest for now
    nearest = case sorted of
        [] -> Nothing
        (x:_) -> Just (intersectionShape x)
    pixel = maybe bg (materialColor . shapeMaterial) nearest

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
    pixelData = renderScene scene camera

main :: IO ()
main = renderImage "out.ppm" scene camera
  where
    -- A plane that functions as the floor of the scene.
    shape = Shape material plane
    material = Material (125,171,37) 0.5
    plane = Plane $ Ray (0,-1,0) $ normalize (0,1,0)
    shape' = Shape material' plane'
    material' = Material (135,206,235) 0.5
    plane' = Plane $ Ray (0,0,-1) $ normalize (0,0,1)
    scene = Scene { sceneBackground = (0,0,0)
                  , sceneShapes     = [shape, shape']
                  }
    -- Camera POV is behind the YZ-plane, with the Z-axis as up.
    camera = Camera { cameraPointOfView  = Ray (-1,0,0) (1,0,0)
                    , cameraUpDirection  = (0,0,1)
                    , cameraFocalLength  = 1
                    , cameraViewPortSize = (2,2)
                    , cameraResolution   = (200,200)
                    }
