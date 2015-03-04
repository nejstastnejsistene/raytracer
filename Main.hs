import Codec.PPM
import Data.AdditiveGroup
import Data.Cross
import Data.List (sortBy)
import Data.Maybe
import Data.Ord
import Data.VectorSpace
import Data.Word

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
    , materialSharpness      :: Maybe Integer
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
    , sceneLightSources :: [LightSource]
    , sceneShapes       :: [Shape]
    }

data LightSource = DirectionalLight Float Vector

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

-- Move a ray forward by a given amount.
moveRay :: Ray -> Float -> Ray
moveRay r@(Ray _ direction) t = Ray (evaluateRay r t) direction

-- Determine the normal vector of a surface at a point.
normalVector :: Geometry -> Vector -> Vector
normalVector (Plane n)           _ = normalize $ rayDirection n
normalVector (Triangle p1 p2 p3) _ = normalize $ (p2 - p1) `cross3` (p3 - p1)
normalVector (Sphere center _)   p = normalize (p - center)

-- Find all intersection of a ray and a list of shapes.
intersect :: Ray -> [Shape] -> [Intersection]
intersect r shapes = sortBy (comparing intersectionTime) intersections
  where intersections = concatMap (intersect' r) shapes

-- Intersection a ray with a single shape.
intersect' :: Ray -> Shape -> [Intersection]
intersect' r s@(Shape m g) = [ Intersection r t s | t <- intersect'' r g ]

-- Calculate the time at which a ray intersects a geometry.
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
        r2 = radius * radius -- Radius squared.
        dt = sqrt (r2 - d2) -- Distance from closest point on ray to sphere edge.

-- Trace a ray with default recursion depth.
traceRay :: Scene -> Ray -> Pixel
traceRay s r = traceRay' s r recursionDepth
  where recursionDepth = 5

-- Trace a ray with a specified recursion depth.
traceRay' :: Scene -> Ray -> Integer -> Pixel
traceRay' s r 0 = (0,0,0)
traceRay' s@(Scene bg ambient lights shapes) r depth = computeShading (intersect r shapes)
  where
    computeShading [] = bg
    computeShading (Intersection _ t s':_) = ambient*^(1,1,1) + illumination
      where
        illumination = sum (map calcShade lights) ^/ fromIntegral (length lights)
        Material color sharpness ref = shapeMaterial s'
        ip = evaluateRay r t
        calcShade (DirectionalLight intensity direction) = shade
          where
            l = normalize (-direction) -- Light starting at intersection point.
            n = normalize $ normalVector (shapeGeometry s') ip -- Normal vector.
            diffuse = color ^* (n `dotMin` l)
            v = - normalize (rayDirection r) -- Incident ray.
            r' = calcReflection n l
            specular = maybe (0,0,0) (\s -> (1,1,1) ^* (v `dotMin` r') ^ s) sharpness
            -- The light ray moved forward so it doesn't cast shadows on itself.
            shadowRay = moveRay (Ray ip l) (10 * epsilon)
            -- See if the light is blocked by anything, for shadows.
            i = case intersect shadowRay shapes of
                [] -> intensity
                _  -> 0
            -- Reflected viewing ray.
            reflection = calcReflection n v
            r'' = moveRay (Ray ip reflection) (10 * epsilon)
            recursive = traceRay' s r'' (depth - 1) -- Reflected image.
            shade = i *^ (1-ref) *^ (diffuse + specular) + ref *^ recursive
        -- Calculate the reflection of a ray about the normal vector.
        calcReflection n r = normalize $ 2 *^ (n `dotMin` r) *^ n - r
        -- Dot product that gets clamped at 0.
        dotMin x y = max 0 (x <.> y)

-- Compute a ray from the camera that goes through the (x,y)th pixel.
castRay :: Camera -> (Integer,Integer) -> Ray
castRay (Camera pov@(Ray p v) up fl (vw,vh) (imgW,imgH)) (x,y) = ray
  where
    -- Center of view port and right vector.
    center = evaluateRay pov fl
    right = normalize (v `cross3` up)
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
    return $ traceRay scene (castRay camera (x,y))

-- Output a scene from a camera into a ppm file.
renderImage :: String -> Scene -> Camera -> IO ()
renderImage path scene camera = writePPM path imgSize pixelData
  where
    imgSize = cameraResolution camera
    pixelData = map castPixel (renderScene scene camera)
    castPixel (r,g,b) = (cast r, cast g, cast b)
    cast = round . (255*) . clip
    clip x | x < 0 = 0
           | x > 1 = 1
           | otherwise = x

main :: IO ()
main = renderImage "out.ppm" scene camera
  where
    scene = Scene { sceneBackground = (0.9,1,1)
                  , sceneAmbientLight = 0
                  , sceneLightSources = [ DirectionalLight 1 (1,1,-1)
                                        , DirectionalLight 1 (0,1,-1) ]
                  , sceneShapes = (redBall : mirrorBall : chessBoard)
                  }
    -- Small red ball.
    redBall = Shape m $ Sphere (2,5,0.5) 0.5
      where m = Material (1,0,0) Nothing 0
    -- Highly reflective black ball.
    mirrorBall = Shape m $ Sphere (4,5,2) 1.5
      where m = Material (0,0,0) (Just 100) 0.9
    -- An 8x8, highly specular and partly reflective checkered board.
    chessBoard = concat $ do
        xi <- [0..7]
        yi <- [0..7]
        let c = if even xi /= even yi then (0.87,0.75,0.51) else (0.62,0.44,0.10)
            m = Material c (Just 10) 0.2
            x = fromIntegral xi
            y = fromIntegral yi
            t1 = Triangle (x,y,0) (x+1,y,0) (x,y+1,0)
            t2 = Triangle (x+1,y+1,0) (x,y+1,0) (x+1,y,0)
        return [Shape m t1, Shape m t2]
    -- Camera is pointed above center of chess board from off to the side..
    cameraFocus = (4,4,1)
    cameraPos = (1,-2,3)
    cameraDir@(dx,dy,_) = normalize $ cameraFocus - cameraPos
    -- Cross with left vector to get up vector.
    cameraUp = normalize $ cameraDir `cross3` (-dy,dx,0)
    camera = Camera { cameraPointOfView  = Ray cameraPos cameraDir
                    , cameraUpDirection  = cameraUp
                    , cameraFocalLength  = 1
                    , cameraViewPortSize = (1.6,0.9)
                    , cameraResolution   = (640,360)
                    }
