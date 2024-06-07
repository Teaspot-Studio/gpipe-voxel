module Graphics.GPipe.Voxel(
    Ray(..)
  , Aabb(..)
  , IntersectResult(..)
  , TraverseResult(..)
  , intersectAabb
  , traverseGrid
  , unprojectRay
  , normalized
  , projectFullScreen
  , module Linear 
  , module Graphics.GPipe
  -- * Debug
  , DebugTrace(..)
  ) where 

import Linear hiding (trace)
import Graphics.GPipe hiding (trace)
import Prelude hiding ((<*))
import Data.Foldable1 (foldl1')
import Debug.Trace 
import GHC.Exts

import qualified Data.List.NonEmpty as NE 

-- | Define ray as origin and direction
data Ray a = Ray {
    rayOrigin :: V3 a
  , rayDirection :: V3 a 
  }

-- | Result of intersection algorithm
data IntersectResult a = IntersectResult {
  intersectSuccess :: !(BooleanOf a) 
, intersectHit     :: !(V3 a)
, intersectNormal  :: !(V3 a)
}

-- Allows to use intersection result as result of if else statements
type instance BooleanOf (IntersectResult a) = BooleanOf a
instance (ShaderType a F, S F Bool ~ BooleanOf a) => IfB (IntersectResult a) where ifB = ifThenElse'

-- How to encode intersection result as basic expressions of shader
instance (ShaderType a F, S F Bool ~ BooleanOf a) => ShaderType (IntersectResult a) F where
    type ShaderBaseType (IntersectResult a) = (BooleanOf a, (ShaderBaseType (V3 a), ShaderBaseType (V3 a)))
    toBase x ~(IntersectResult succ hit normal) = ShaderBaseProd (ShaderBaseBool succ) $ ShaderBaseProd (toBase x hit) (toBase x normal)
    fromBase x (ShaderBaseProd succ (ShaderBaseProd hit normal)) = IntersectResult (fromBase x succ) (fromBase x hit) (fromBase x normal)

-- | Axis aligned box defined by two corner points
data Aabb a = Aabb {
    minPoint :: !(V3 a) -- ^ Minimal coordinates point
  , maxPoint :: !(V3 a) -- ^ Maximum coordinates point
  } deriving (Show)

-- | Calculate intersection between AABB and a ray
intersectAabb :: Aabb FFloat -- ^ Box
  -> Ray FFloat -- ^ Ray 
  -> IntersectResult FFloat 
intersectAabb aabb@(Aabb minv maxv) (Ray origin dir) = let 
  dirInv = recip dir 
  t0s = (minv - origin) * dirInv 
  t1s = (maxv - origin) * dirInv
  eps = 1e20
  (tmin, (normal, hitPoint)) = maximumWithB [
      minWithB (t0s.x, (V3 (-1) 0 0, setX (minv.x - eps) $ origin + dir ^* t0s.x)) (t1s.x, (V3 1 0 0, setX (maxv.x + 0) $ origin + dir ^* t1s.x))
    , minWithB (t0s.y, (V3 0 (-1) 0, setY (minv.y - eps) $ origin + dir ^* t0s.y)) (t1s.y, (V3 0 1 0, setY (maxv.y + 0) $ origin + dir ^* t1s.y))
    , minWithB (t0s.z, (V3 0 0 (-1), setZ (minv.z - eps) $ origin + dir ^* t0s.z)) (t1s.z, (V3 0 0 1, setZ (maxv.z + 0) $ origin + dir ^* t1s.z)) ]
  tmax = minimumB [maxB t0s.x t1s.x, maxB t0s.y t1s.y, maxB t0s.z t1s.z]
  success = tmax >=* maxB tmin 0
  in ifB success (IntersectResult true hitPoint normal) (IntersectResult false 0 0) 

setX :: a -> V3 a -> V3 a 
setX x (V3 _ y z) = V3 x y z 

setY :: a -> V3 a -> V3 a 
setY y (V3 x _ z) = V3 x y z 

setZ :: a -> V3 a -> V3 a 
setZ z (V3 x y _) = V3 x y z 

maximumWithB :: (BooleanOf a ~ BooleanOf b, IfB a, IfB b, OrdB a) => [(a, b)] -> (a, b) 
maximumWithB [] = error "empty maximumB input"
maximumWithB ax = foldl1' (\(a, ab) (b, bb) -> ifB (a <* b) (b, bb) (a, ab)) $ NE.fromList ax 

maximumB :: (IfB a, OrdB a) => [a] -> a 
maximumB [] = error "empty maximumB input"
maximumB ax = foldl1' maxB $ NE.fromList ax 

minimumB :: (IfB a, OrdB a) => [a] -> a 
minimumB [] = error "empty minimumB input"
minimumB ax = foldl1' minB $ NE.fromList ax 

minWithB :: (BooleanOf a ~ BooleanOf b, IfB a, IfB b, OrdB a) => (a, b) -> (a, b) -> (a, b)
minWithB (a, ab) (b, bb) = ifB (a <* b) (a, ab) (b, bb)

-- | Result of traversal algorithm of voxel grid
data TraverseResult a = TraverseResult {
  traverseSuccess :: !(BooleanOf a) -- ^ If voxel hit or not
, traverseHit     :: !(V3 a) -- ^ Position of hit
, traverseNormal  :: !(V3 a) -- ^ Normal vector 
, traverseDiffuse :: !(V3 a) -- ^ Diffuse color
}

deriving instance (Show a, Show (BooleanOf a)) => Show (TraverseResult a)
deriving instance (Eq a, Eq (BooleanOf a)) => Eq (TraverseResult a)

-- Allows to use intersection result as result of if else statements
type instance BooleanOf (TraverseResult a) = BooleanOf a
instance IfB (TraverseResult FFloat) where ifB = ifThenElse'
instance IfB (TraverseResult Float) where ifB cond thenCond elseCond = if cond then thenCond else elseCond

-- How to encode intersection result as basic expressions of shader
instance (ShaderType a F, S F Bool ~ BooleanOf a) => ShaderType (TraverseResult a) F where
    type ShaderBaseType (TraverseResult a) = (BooleanOf a, (ShaderBaseType (V3 a), (ShaderBaseType (V3 a), ShaderBaseType (V3 a))))
    toBase x ~(TraverseResult succ hit normal diffuse) = ShaderBaseProd (ShaderBaseBool succ) $ ShaderBaseProd (toBase x hit) $ ShaderBaseProd (toBase x normal) (toBase x diffuse)
    fromBase x (ShaderBaseProd succ (ShaderBaseProd hit (ShaderBaseProd normal diffuse))) = TraverseResult (fromBase x succ) (fromBase x hit) (fromBase x normal) (fromBase x diffuse)

type instance BooleanOf (a,b,c,d,e,f,r) = BooleanOf a

instance (bool ~ BooleanOf p, bool ~ BooleanOf q, bool ~ BooleanOf r, bool ~ BooleanOf s, bool ~ BooleanOf t, bool ~ BooleanOf y, bool ~ BooleanOf u
         ,IfB p, IfB q, IfB r, IfB s, IfB t, IfB y, IfB u) => IfB (p,q,r,s,t,y,u) where
  ifB w (p,q,r,s,t,y,u) (p',q',r',s',t',y',u') =
    (ifB w p p', ifB w q q', ifB w r r', ifB w s s', ifB w t t', ifB w y y', ifB w u u')

class (IfB a, BooleanOf a ~ b) => While a b where 
  whileB :: (a -> b) -> (a -> a) -> a -> a

instance (IfB a, ShaderType a x, BooleanOf a ~ S x Bool) => While a (S x Bool) where 
  whileB = while 
instance (IfB a, BooleanOf a ~ Bool) => While a Bool where 
  whileB cond step = go 
    where 
      go a = if cond a then go $ step a else a 

class DebugTraceConstr a => DebugTrace a where 
  type DebugTraceConstr a :: Constraint 
  debugShow :: a -> String 

traceTag :: DebugTrace a => String -> a -> a
traceTag tag = traceWith (\a -> tag ++ ": " ++ debugShow a) 

traceShowTag :: DebugTrace a => String -> a -> b -> b 
traceShowTag tag a = trace (tag ++ debugShow a)

instance DebugTrace Float where 
  type DebugTraceConstr Float = Show Float
  debugShow = show

instance DebugTrace Bool where 
  type DebugTraceConstr Bool = Show Bool
  debugShow = show

instance DebugTrace (S x a) where 
  type DebugTraceConstr (S x a) = ()
  debugShow _ = ""

instance DebugTrace a => DebugTrace (V3 a) where 
  type DebugTraceConstr (V3 a) = DebugTraceConstr a
  debugShow (V3 a b c) = "V3 " ++ debugShow a ++ " " ++ debugShow b ++ " " ++ debugShow c

instance (DebugTrace a, DebugTrace b, DebugTrace c, DebugTrace d, DebugTrace e, DebugTrace f, DebugTrace g) => DebugTrace (a, b, c, d, e, f, g) where 
  type DebugTraceConstr (a, b, c, d, e, f, g) = (DebugTraceConstr a, DebugTraceConstr b, DebugTraceConstr c, DebugTraceConstr d, DebugTraceConstr e, DebugTraceConstr f, DebugTraceConstr g)
  debugShow (a, b, c, d, e, f, g) = "(" ++ debugShow a ++ ", " ++ debugShow b ++ ", " ++ debugShow c ++ ", " ++ debugShow d ++ ", " ++ debugShow e ++ ", " ++ debugShow f ++ ", " ++ debugShow g  ++ ")"

-- | Function to go through grid of voxels and select color
traverseGrid :: (Fractional a, Real' a, IfB a, EqB a, OrdB a, IfB (TraverseResult a), 
    While (BooleanOf a, BooleanOf a, V3 a, V3 a, V3 a, V3 a, V3 a) (BooleanOf a),
    DebugTrace a, DebugTrace (BooleanOf a)
  )
  => Aabb a -- ^ Bounding box of the voxel volume
  -> V3 a -- ^ Size of the grid 
  -> (V3 a -> V3 a) -- ^ Sampler of voxels
  -> V3 a -- ^ Direction of the ray
  -> IntersectResult a -- ^ Entry point of ray
  -> TraverseResult a -- ^ Color of the voxel or background
traverseGrid box@(Aabb minCorner maxCorner) gridSize sample dir (IntersectResult entrySuccess start entryNormal) = let 
  missed = TraverseResult false 0 0 0
  boxSize = maxCorner - minCorner 
  entry = traceTag "entry" $ (start - minCorner) / boxSize * gridSize -- remaps ray entry point to [0 .. gridSize] range
  stepDir = traceTag "stepDir" $ signum dir -- Step direction by each axis
  dt = traceTag "dt" $ abs <$> recip dir -- "Time" to reach a new voxel by each axis (always non negative)
  floorDown a = let -- Special floor that decreases by 1 if we are on higher edge
    fa = floor' a 
    in ifB (fa ==* a) (a - 1.0) fa 
  ceilingUp a = let -- Special ceiling that increases by 1 if we are on lower edge 
    ca = ceiling' a 
    in ifB (ca ==* a) (a + 1.0) ca  
  calcTmax (s, e, d) = let 
    de = ifB (s <* 0) (traceTag "e" e - traceTag "floorDown e" (floorDown e)) (traceTag "ceilingUp e" (ceilingUp e) - traceTag "e" e)
    in de / abs d
  tmax0 = traceTag "tmax0" $ calcTmax <$> zip3V3 stepDir entry dir -- Initial value of tmax
  calcJustOut (s, n) = ifB (s >* 0) (n-1) 0
  justOut = calcJustOut <$> zipV3 stepDir gridSize -- Keep index that terminates the ray
  normals = V3 (V3 (-stepDir.x) 0 0) (V3 0 (-stepDir.y) 0) (V3 0 0 (-stepDir.z))
  stepLoop datum@(_, _, pos, tmax, _, normal, _) = traceShowTag "datum" datum $ let -- The core loop of traversal
    voxelIndex = (\(p, s) -> ifB (s <* 0) (floorDown p) (floor' p)) <$> zipV3 pos stepDir
    voxel = sample $ traceTag "sample at" voxelIndex
    notHit = voxel ==* 0
    stepX = traceTag "stepX" (notHit, voxelIndex.x /=* justOut.x, entry + dir ^* tmax.x, tmax + V3 dt.x 0 0, pos, ifB notHit normals.x normal, voxel)
    stepY = traceTag "stepY" (notHit, voxelIndex.y /=* justOut.y, entry + dir ^* tmax.y, tmax + V3 0 dt.y 0, pos, ifB notHit normals.y normal, voxel)
    stepZ = traceTag "stepZ" (notHit, voxelIndex.z /=* justOut.z, entry + dir ^* tmax.z, tmax + V3 0 0 dt.z, pos, ifB notHit normals.z normal, voxel)
    in traceTag "loopres" $ ifB (tmax.x <* tmax.y) (ifB (tmax.x <* tmax.z) stepX stepZ) (ifB (tmax.y <* tmax.z) stepY stepZ)
  (notHit, isInside, _, _, hitGrid, normal, diffuse) = whileB (\(notHit, isInside, _, _, _, _, _) -> notHit &&* isInside) stepLoop (true, true, entry, tmax0, 0, entryNormal, 0)
  hitWorld = hitGrid / gridSize * boxSize + minCorner -- hit is defined in voxel grid coordinates, recalculate it back to the world coords
  success = ifB (notB notHit) (TraverseResult true hitWorld normal diffuse) missed
  in ifB entrySuccess success missed

zipV3 :: V3 a -> V3 b -> V3 (a, b)
zipV3 (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1, x2) (y1, y2) (z1, z2)

zip3V3 :: V3 a -> V3 b -> V3 c -> V3 (a, b, c)
zip3V3 (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x3 y3 z3) = V3 (x1, x2, x3) (y1, y2, y3) (z1, z2, z3)

-- | Transform fragment position to direction in world space. Origin is camera eye.
unprojectRay :: Floating a
  => M44 a -- ^ Inverse projection matrix
  -> M44 a -- ^ Inverse view matrix
  -> V2 a -- ^ Fragment coordinates
  -> V3 a -- ^ World space coordinates
unprojectRay invProjMat invViewMat (V2 x y) = let 
  clipSpaceCoords = V4 (2.0 * x - 1.0) (2.0 * y - 1.0) (-1.0) 1.0 
  eyeSpaceCoords = invProjMat !* clipSpaceCoords;
  eyeSpaceDir = V4 eyeSpaceCoords.x eyeSpaceCoords.y (-1.0) 0.0 -- z = -1.0 here is always direction for camera forward, w = 0.0 means that it is now direction and not affected by offsets
  worldSpaceDir = normalized (invViewMat !* eyeSpaceDir)
  in worldSpaceDir.xyz

-- | Simplified version of 'normalize' from linear. We don't have Epsilon for the 'S F a'
normalized :: (Metric f, Floating a) => f a -> f a
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