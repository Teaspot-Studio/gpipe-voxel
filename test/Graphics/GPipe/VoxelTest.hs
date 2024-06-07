module Graphics.GPipe.VoxelTest where 

import Linear
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Util

import Graphics.GPipe.Voxel

unit_traverse_test :: IO ()
unit_traverse_test = do 
  let box :: Aabb Float = Aabb 0 1 
      size = 2
      sampler = const $ V3 1 0 0
      dir = V3 1 0 0
      intersection = IntersectResult True (V3 0 0.1 0.1)
      traversed = traverseGrid box size sampler dir intersection
  TraverseResult True (V3 0 0.1 0.1) (V3 (-1) 0 0) (V3 1 0 0) @=? traversed