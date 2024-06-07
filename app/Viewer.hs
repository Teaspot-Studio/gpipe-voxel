module Main where

import Prelude hiding ((<*))
import Control.Applicative (pure)
import Control.Monad (unless)
import Control.Monad.Exception (MonadException)
import Control.Monad.IO.Class (MonadIO(..))
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Monoid (mappend)
import Data.Word (Word8)
import Graphics.GPipe
import Graphics.GPipe.Expr 
import Graphics.GPipe.Voxel

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
    
    -- Make 3D texture with voxels
    let voxSize = 2
    let initGridSize = V3 voxSize voxSize voxSize
    tex <- newTexture3D RGB8 initGridSize 1
    let red :: V3 Word8 = V3 255 0 0 
    let green :: V3 Word8 = V3 0 255 0 
    let blue :: V3 Word8 = V3 0 0 255 
    let violet :: V3 Word8 = V3 255 0 255 
    let yellow :: V3 Word8 = V3 255 255 0 
    let blank :: V3 Word8 = V3 0 0 0
    -- writeTexture3D tex 0 0 initGridSize [ red, blank, green, blank, violet, yellow, blank, blank]
    writeTexture3D tex 0 0 initGridSize $ 
      [green, blank, red, blank] ++ [blue, blank, yellow, blank]
    -- Screen sized quad that we use in both shaders
    vertexBuffer :: Buffer os (B2 Float) <- newBuffer 4
    writeBuffer vertexBuffer 0 [V2 0 0, V2 1 0, V2 0 1, V2 1 1]

    -- Allocate textures for color and depth
    let raycastRes :: V2 Int = 256
    colorTex <- newTexture2D RGB8 raycastRes 1
    depthTex <- newTexture2D Depth16 raycastRes 1

    -- Setup uniform buffer for eye position 
    eyeBuffer :: Buffer os (Uniform (V3 (B Float))) <- newBuffer 1 
    let initEye = V3 (-1.0) 0 (-1.0)
    writeBuffer eyeBuffer 0 [ initEye ]

    -- Setup uniform buffer for inverse projection and view matrices
    invMatsBuffer :: Buffer os (Uniform (M44 (B Float))) <- newBuffer 2
    -- let initProjMat = ortho (-0.5) 0.5 (-0.5) 0.5 (-1.0) 10.0 
    let initProjMat = perspective (pi/3) 1 1 100
    let initViewMat = lookAt' initEye (V3 0 0 0) (V3 0 1 0)
    writeBuffer invMatsBuffer 0 [ inv44 initProjMat, inv44 initViewMat ]

    -- Setup uniform for updating light position each frame
    lightBuffer :: Buffer os (Uniform (V3 (B Float))) <- newBuffer 1 
    let initLight = V3 (-1.0) 1.2 (-1.0)
    writeBuffer lightBuffer 0 [ initLight ]
  
    -- Setup uniform for size of the voxel grid 
    gridSizeBuffer :: Buffer os (Uniform (V3 (B Float))) <- newBuffer 1
    writeBuffer gridSizeBuffer 0 [ fromIntegral <$> initGridSize ]

    -- First shader performs raycast in low resolution into texture
    shader1 :: CompiledShader os RaycastEnvironment <- compileShader $ do
      fragmentStream <- projectFullScreen (const raycastRes) (.primitives)
      invProjMat <- getUniform (const (invMatsBuffer, 0))
      invViewMat <- getUniform (const (invMatsBuffer, 1))
      eyePos <- getUniform (const (eyeBuffer, 0))
      lightPos <- getUniform (const (lightBuffer, 0))
      gridSize <- getUniform (const (gridSizeBuffer, 0))
      let filter = SamplerFilter Nearest Nearest Nearest Nothing 
          edge = (pure ClampToEdge, 0)
      samp <- newSampler3D (const (tex, filter, edge))
      let sampleVoxels = sample3D samp SampleAuto Nothing Nothing
          aabb = Aabb (-0.2) 0.2
          diffuse = V3 0.8 0 0
          background = 0
          raycast pos = let 
            dir = unprojectRay invProjMat invViewMat pos
            ray = Ray eyePos dir 
            intersected = intersectAabb aabb ray
            traversed = traverseGrid aabb gridSize sampleVoxels dir intersected 
            in lit lightPos background (traversed { traverseNormal = intersectNormal intersected})
          fragmentStream2 = withRasterizedInfo (\a r -> (a, (rasterizedFragCoord r).z)) $ fmap raycast fragmentStream
      drawDepth (\s -> (NoBlending, depthImage s, DepthOption Less True)) fragmentStream2 $ \ a -> do
        drawColor (\ s -> (colorImage s, pure True, False)) a

    -- Second shader remaps lowres into full screen quad
    shader2 :: CompiledShader os FinalEnvironment <- compileShader $ do
      fragmentStream <- projectFullScreen (.winSize) (.primitives)

      let filter = SamplerFilter Nearest Nearest Nearest Nothing
          edge = (pure ClampToEdge, 0)
      samp <- newSampler2D (const (colorTex, filter, edge))
      let sampleTexture = sample2D samp SampleAuto Nothing Nothing
          fragmentStream2 = fmap sampleTexture fragmentStream
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

-- | Apply simple light to the result of intersection
lit :: V3 FFloat -- ^ Position of light 
  -> V3 FFloat -- ^ Color of background
  -> TraverseResult FFloat
  -> V3 FFloat 
lit lightPos back (TraverseResult succ hit normal diffuse) = ifB succ normal back -- ifB succ lited back 
  where 
    litFactor = normalized (lightPos - hit) `dot` normal 
    lited = litFactor *^ diffuse

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
