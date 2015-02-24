import Codec.PPM
import Data.AdditiveGroup
import Data.Cross
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

data Shape = Plane Ray
           | Triangle Vector Vector Vector

data Scene = Scene [Shape]

data Camera = Camera
            { cameraPointOfView  :: Ray
            , cameraUpDirection  :: Vector
            , cameraFocalLength  :: Float
            , cameraViewPortSize :: (Float,Float)
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

-- Calculate where along the ray (if at all) it intersects the shape.
intersect :: Shape -> Ray -> Maybe Float
intersect (Plane (Ray off n)) (Ray p v) = if hit then Just t else Nothing
  where
    cos = v <.> n
    t = ((off ^-^ p) <.> n) / cos
    hit = abs cos > epsilon && t > -epsilon
--intersect (Triangle p1 p2 p3) r@(Ray p v) = case intersect (Plane normal) r of
--    Nothing -> Nothing
--    Just t -> -- check if inside barycentric coordinates
    

traceRay :: Scene -> Ray -> Pixel
traceRay (Scene [shape@(Plane (Ray _ n))]) r = maybe (0,0,0) (const (0,255,0)) (intersect shape r)

-- Compute a ray from the camera that goes through the (x,y)th pixel.
castRay :: Camera -> (Integer,Integer) -> (Integer,Integer) -> Ray
castRay (Camera pov@(Ray p v) up fl (vw,vh)) (x,y) (imgW,imgH) = ray
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
renderScene :: Scene -> Camera -> (Integer,Integer) -> [Pixel]
renderScene scene camera imgSize@(w,h) = do
    y <- [0..h-1]
    x <- [0..w-1]
    return $ traceRay scene $ castRay camera (x,y) imgSize

main :: IO ()
main = writePPM "out.ppm" imgSize (renderScene scene camera imgSize)
  where
    -- A plane that functions as the floor of the scene.
    scene = Scene [Plane (Ray (0,0,-1) (0,0,1))]
    -- Camera POV is behind the YZ-plane, with the Z-axis as up.
    camera = Camera { cameraPointOfView  = Ray (-1,0,0) (1,0,0)
                    , cameraUpDirection  = (0,0,1)
                    , cameraFocalLength  = 1
                    , cameraViewPortSize = (2, 2)
                    }
    -- Size of the image in pixels.
    imgSize = (200,200)
