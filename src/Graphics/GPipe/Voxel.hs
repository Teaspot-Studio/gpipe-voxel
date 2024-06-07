module Graphics.GPipe.Voxel(
    Aabb(..)
  , IntersectResult(..)
  , TraverseResult(..)
  , traverseGrid
  , module Linear 
  , module Graphics.GPipe
  ) where 

import Linear 
import Graphics.GPipe
import Prelude hiding ((<*))

-- | Result of intersection algorithm
data IntersectResult a = IntersectResult {
  intersectSuccess :: !(BooleanOf a) 
, intersectHit     :: !(V3 a)
}

-- Allows to use intersection result as result of if else statements
type instance BooleanOf (IntersectResult a) = BooleanOf a
instance (ShaderType a F, S F Bool ~ BooleanOf a) => IfB (IntersectResult a) where ifB = ifThenElse'

-- How to encode intersection result as basic expressions of shader
instance (ShaderType a F, S F Bool ~ BooleanOf a) => ShaderType (IntersectResult a) F where
    type ShaderBaseType (IntersectResult a) = (BooleanOf a, ShaderBaseType (V3 a))
    toBase x ~(IntersectResult succ hit) = ShaderBaseProd (ShaderBaseBool succ) $ toBase x hit
    fromBase x (ShaderBaseProd succ hit) = IntersectResult (fromBase x succ) (fromBase x hit) 

-- | Axis aligned box defined by two corner points
data Aabb a = Aabb {
    minPoint :: !(V3 a) -- ^ Minimal coordinates point
  , maxPoint :: !(V3 a) -- ^ Maximum coordinates point
  } deriving (Show)

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

-- | Function to go through grid of voxels and select color
traverseGrid :: (Fractional a, Real' a, IfB a, EqB a, OrdB a, IfB (TraverseResult a), While
                      (BooleanOf a, BooleanOf a, V3 a, V3 a, V3 a, V3 a, V3 a)
                      (BooleanOf a))
  => Aabb a -- ^ Bounding box of the voxel volume
  -> V3 a -- ^ Size of the grid 
  -> (V3 a -> V3 a) -- ^ Sampler of voxels
  -> V3 a -- ^ Direction of the ray
  -> IntersectResult a -- ^ Entry point of ray
  -> TraverseResult a -- ^ Color of the voxel or background
traverseGrid box@(Aabb minCorner maxCorner) gridSize sample dir (IntersectResult entrySuccess start) = let 
  missed = TraverseResult false 0 0 0
  boxSize = maxCorner - minCorner 
  entry = (start - minCorner) / boxSize * gridSize -- remaps ray entry point to [0 .. gridSize] range
  step = signum dir -- Step direction by each axis
  dt = abs <$> recip dir -- "Time" to reach a new voxel by each axis (always non negative)
  calcTmax (s, e, t) = let 
    de = ifB (s <* 0) (e - floor' e) (ceiling' e - e)
    in de / t
  tmax0 = calcTmax <$> zip3V3 step entry dt -- Initial value of tmax
  calcJustOut (s, n) = ifB (s >* 0) (n-1) 0
  justOut = calcJustOut <$> zipV3 step gridSize -- Keep index that terminates the ray
  normals = V3 (V3 step.x 0 0) (V3 0 step.y 0) (V3 0 0 step.z)
  stepLoop (_, _, pos, tmax, _, _, _) = let -- The core loop of traversal
    voxel = sample pos
    notHit = voxel /=* 0
    stepX = (notHit, floor' pos.x /=* justOut.x, entry + V3 step.x 0 0, tmax + V3 dt.x 0 0, pos, normals.x, voxel)
    stepY = (notHit, floor' pos.y /=* justOut.y, entry + V3 0 step.y 0, tmax + V3 0 dt.y 0, pos, normals.y, voxel)
    stepZ = (notHit, floor' pos.z /=* justOut.z, entry + V3 0 0 step.z, tmax + V3 0 0 dt.z, pos, normals.z, voxel)
    in ifB (tmax.x <* tmax.y) (ifB (tmax.x <* tmax.z) stepX stepZ) (ifB (tmax.y <* tmax.z) stepY stepZ)
  (notHit, isInside, _, _, hit, normal, diffuse) = whileB (\(notHit, isInside, _, _, _, _, _) -> notHit &&* isInside) stepLoop (true, true, entry, tmax0, 0, 0, 0)
  success = TraverseResult (notB notHit &&* isInside) hit normal diffuse
  in ifB entrySuccess success missed

zipV3 :: V3 a -> V3 b -> V3 (a, b)
zipV3 (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1, x2) (y1, y2) (z1, z2)

zip3V3 :: V3 a -> V3 b -> V3 c -> V3 (a, b, c)
zip3V3 (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x3 y3 z3) = V3 (x1, x2, x3) (y1, y2, y3) (z1, z2, z3)
