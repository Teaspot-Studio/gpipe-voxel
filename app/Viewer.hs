module Main where

import Control.Applicative (pure)
import Control.Monad (unless)
import Control.Monad.Exception (MonadException)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Data.Word (Word32)
import Graphics.GPipe
import Graphics.GPipe.Expr 

import qualified Graphics.GPipe.Context.GLFW as GLFW

data RaycastEnvironment = RaycastEnvironment {
    primitives :: PrimitiveArray Triangles (B2 Float)
  , winSize :: V2 Int
  , colorImage :: Image (Format RGBFloat)
  , depthImage :: Image (Format Depth)
  }

data FinalEnvironment = FinalEnvironment {
    primitives :: PrimitiveArray Triangles (B2 Float)
  , winSize :: V2 Int
  }

main :: IO ()
main =
  runContextT GLFW.defaultHandleConfig $ do
    win <- newWindow (WindowFormatColor RGB8) (GLFW.defaultWindowConfig "Viewer")

    -- Screen sized quad that we use in both shaders
    vertexBuffer :: Buffer os (B2 Float) <- newBuffer 4
    writeBuffer vertexBuffer 0 [V2 0 0, V2 1 0, V2 0 1, V2 1 1]

    -- Allocate textures for color and depth
    let raycastRes :: V2 Int = 64
    colorTex <- newTexture2D RGB8 raycastRes 1
    depthTex <- newTexture2D Depth16 raycastRes 1

    -- Setup uniform buffer for eye position 
    eyeBuffer :: Buffer os (Uniform (V3 (B Float))) <- newBuffer 1 
    let initEye = V3 (-0.6) 0.5 0.8
    writeBuffer eyeBuffer 0 [ initEye ]

    -- Setup uniform buffer for inverse projection and view matrices
    invMatsBuffer :: Buffer os (Uniform (M44 (B Float))) <- newBuffer 2
    let initProjMat = perspective (pi/3) 1 1 100
    let initViewMat = lookAt' initEye (V3 0 0 0) (V3 0 1 0)
    writeBuffer invMatsBuffer 0 [ inv44 initProjMat, inv44 initViewMat ]

    -- Setup uniform for updating light position each frame
    lightBuffer :: Buffer os (Uniform (V3 (B Float))) <- newBuffer 1 
    let initLight = V3 (-1.0) 1.2 (-1.0)
    writeBuffer lightBuffer 0 [ initLight ]
  
    -- First shader performs raycast in low resolution into texture
    shader1 :: CompiledShader os RaycastEnvironment <- compileShader $ do
      fragmentStream <- projectFullScreen (const raycastRes) (.primitives)
      invProjMat <- getUniform (const (invMatsBuffer, 0))
      invViewMat <- getUniform (const (invMatsBuffer, 1))
      eyePos <- getUniform (const (eyeBuffer, 0))
      lightPos <- getUniform (const (lightBuffer, 0))
      let spherePos = 0
          sphereRad = 0.2 
          sphereDiffuse = V3 0.8 0 0
          background = 0
          fragmentStream2 = fmap (lit lightPos . intersectSphere spherePos sphereRad sphereDiffuse background . Ray eyePos . unprojectRay invProjMat invViewMat) fragmentStream
          fragmentStream3 = withRasterizedInfo (\a r -> (a, (rasterizedFragCoord r).z)) fragmentStream2
      drawDepth (\s -> (NoBlending, depthImage s, DepthOption Less True)) fragmentStream3 $ \ a -> do
        drawColor (\ s -> (colorImage s, pure True, False)) a

    -- Second shader remaps lowres into full screen quad
    shader2 :: CompiledShader os FinalEnvironment <- compileShader $ do
      fragmentStream <- projectFullScreen (.winSize) (.primitives)

      let filter = SamplerFilter Nearest Nearest Nearest Nothing
          edge = (pure ClampToEdge, 0)
      samp <- newSampler2D (const (colorTex, filter, edge))
      let sampleTexture = sample2D samp SampleAuto Nothing Nothing
          fragmentStream2 = fmap ((\(V3 r g b) -> V3 r 0 g) . sampleTexture . (\(V2 x y) -> V2 x (1.0 - y))) fragmentStream
      drawWindowColor (const (win, ContextColorOption NoBlending (pure True))) fragmentStream2

    lightRef <- liftIO $ newIORef initLight
    let env = LoopEnv lightBuffer lightRef
    renderLoop win env [
      \winSize -> do
        vertexArray <- newVertexArray vertexBuffer
        let singleTriangle = takeVertices 3 vertexArray
        cImage <- getTexture2DImage colorTex 0
        dImage <- getTexture2DImage depthTex 0
        clearImageColor cImage 0
        clearImageDepth dImage 1
        shader1 $ RaycastEnvironment (toPrimitiveArray TriangleStrip vertexArray) winSize cImage dImage
      ,
      \winSize -> do
        clearWindowColor win 0.5
        vertexArray <- newVertexArray vertexBuffer
        shader2 $ FinalEnvironment (toPrimitiveArray TriangleStrip vertexArray) winSize
      ]

-- | Define ray as origin and direction
data Ray a = Ray {
    rayOrigin :: V3 a
  , rayDirection :: V3 a 
  }

-- | Showcase how to define intermediate results 
data IntersectResult a = IntersectResult {
  intersectSuccess :: !FBool 
, intersectHit     :: !(V3 a)
, intersectDiffuse :: !(V3 a)
, intersectNormal  :: !(V3 a)
}

-- Allows to use intersection result as result of if else statements
type instance BooleanOf (IntersectResult a) = FBool
instance ShaderType a F => IfB (IntersectResult a) where ifB = ifThenElse'

-- How to encode intersection result as basic expressions of shader
instance ShaderType a F => ShaderType (IntersectResult a) F where
    type ShaderBaseType (IntersectResult a) = (FBool, (ShaderBaseType (V3 a), (ShaderBaseType (V3 a), ShaderBaseType (V3 a))))
    toBase x ~(IntersectResult succ hit diffuse normal) = ShaderBaseProd (ShaderBaseBool succ) $ ShaderBaseProd (toBase x hit) $ ShaderBaseProd (toBase x diffuse) (toBase x normal)
    fromBase x (ShaderBaseProd succ (ShaderBaseProd hit (ShaderBaseProd diffuse normal))) = IntersectResult (fromBase x succ) (fromBase x hit) (fromBase x diffuse) (fromBase x normal)

-- | Calculate intersection with a sphere including normal vector
intersectSphere :: V3 FFloat -- ^ Sphere position
  -> FFloat -- ^ Sphere radius 
  -> V3 FFloat -- ^ Sphere color
  -> V3 FFloat -- ^ Miss color
  -> Ray FFloat -- ^ Ray
  -> IntersectResult FFloat -- ^ Diffuse Color and normal vector 
intersectSphere pos rad col missCol (Ray orig dir) = let 
  l = pos - orig -- direction from sphere center to ray origin
  tca = l `dot` dir -- project that direction to ray direction
  d2 = (l `dot` l) - tca * tca -- squared distance from center to projection
  r2 = rad * rad 
  thc = sqrt (r2 - d2) -- distance between projection and intersection
  t0 = tca - thc -- distance to the first point 
  t1 = tca + thc -- distance to the second point
  p = orig + (dir ^* t0) -- hit point
  normal = normalized (p - pos)
  miss = IntersectResult false 0 missCol 0 
  hit = IntersectResult true p col normal
  in ifB (d2 >* r2) miss $ ifB (t0 >* 0) hit miss

-- | Apply simple light to the result of intersection
lit :: V3 FFloat -- ^ Position of light 
  -> IntersectResult FFloat
  -> V3 FFloat 
lit lightPos (IntersectResult succ hit diffuse normal) = ifB succ lited diffuse 
  where 
    litFactor = normalized (hit - lightPos) `dot` normal 
    lited = litFactor *^ diffuse

-- | Transform fragment position to direction in world space. Origin is camera eye.
unprojectRay :: M44 FFloat -- ^ Inverse projection matrix
  -> M44 FFloat -- ^ Inverse view matrix
  -> V2 FFloat -- ^ Fragment coordinates
  -> V3 FFloat -- ^ World space coordinates
unprojectRay invProjMat invViewMat (V2 x y) = let 
  clipSpaceCoords = V4 (2.0 * x - 1.0) (2.0 * y - 1.0) (-1.0) 1.0 
  eyeSpaceCoords = invProjMat !* clipSpaceCoords;
  eyeSpaceDir = V4 eyeSpaceCoords.x eyeSpaceCoords.y (-1.0) 0.0 -- z = -1.0 here is always direction for camera forward, w = 0.0 means that it is now direction and not affected by offsets
  worldSpaceDir = normalized (invViewMat !* eyeSpaceDir)
  in worldSpaceDir.xyz

-- | Simplified version of 'normalize' from linear. We don't have Epsilon for the 'S F a'
normalized :: Metric f => f FFloat -> f FFloat
normalized v = (/ norm v) <$> v 

-- | Project primitive to the full size of the window without any transformations
projectFullScreen :: (VertexFormat a ~ V2 VFloat, VertexInput a) 
    => (s -> V2 Int) -- ^ Viewport size
    -> (s -> PrimitiveArray p a) -- ^ Selector of primitive from shader env
    -> Shader os s (FragmentStream (V2 FFloat))
projectFullScreen sSize sf = do 
    primitiveStream <- toPrimitiveStream sf
    let mappedStream = fmap (\pos2d -> (V4 (2 * pos2d.x - 1) (2 * pos2d.y - 1) 0 1, pos2d)) primitiveStream
    rasterize (\s -> (FrontAndBack, ViewPort (V2 0 0) (sSize s), DepthRange 0 1)) mappedStream

-- | Dynamic data that passes from frame to frame
data LoopEnv os = LoopEnv {
    lightBuffer :: Buffer os (Uniform (V3 (B Float)))
  , lightPosRef :: IORef (V3 Float)
  }

-- | Rotate light around up axis
updateLight :: MonadIO m => LoopEnv os -> ContextT GLFW.Handle os m ()
updateLight LoopEnv{..} = do 
  curLight <- liftIO $ readIORef lightPosRef 
  let quat = axisAngle (V3 0 1 0) 0.01
  let newLight = quat `rotate` curLight 
  liftIO $ writeIORef lightPosRef newLight
  writeBuffer lightBuffer 0 [ newLight ]

-- | Draw frame and do at again and again
renderLoop :: (Foldable t, MonadIO m, MonadException m) 
  => Window os c ds 
  -> LoopEnv os
  -> t (V2 Int -> Render os ()) -> ContextT GLFW.Handle os m ()
renderLoop win env renderings = do
  winSize <- maybe 0 (uncurry V2) <$> GLFW.getWindowSize win 
  updateLight env
  mapM_ (\sh -> render $ sh winSize) renderings
  swapWindowBuffers win
  closeRequested <- GLFW.windowShouldClose win
  unless (closeRequested == Just True) $
    renderLoop win env renderings

-- Copy of lookAt from linear with normalize replaced with signorm
lookAt' :: Floating a => V3 a -> V3 a -> V3 a -> V4 (V4 a)
lookAt' eye center up =
  V4 (V4 xa.x xa.y xa.z xd)
     (V4 ya.x ya.y ya.z yd)
     (V4 (-za.x) (-za.y) (-za.z) zd)
     (V4 0     0     0     1)
  where za = signorm $ center - eye
        xa = signorm $ cross za up
        ya = cross xa za
        xd = -dot xa eye
        yd = -dot ya eye
        zd = dot za eye
