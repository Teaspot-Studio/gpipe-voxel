module Graphics.GPipe.VoxelTest where 

import Linear
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Util

import Graphics.GPipe.Voxel

testTraverseFull :: V3 Float -> V3 Float -> V3 Float -> Assertion
testTraverseFull dir hit normal = do 
  let box :: Aabb Float = Aabb 0 1 
      size = 2
      color = V3 1 0 0
      sampler = const color
      intersection = IntersectResult True hit normal
      traversed = traverseGrid box size sampler dir intersection
  TraverseResult True hit normal color @=? traversed

unit_traverse_full_axis_x0_y0_z0 :: Assertion
unit_traverse_full_axis_x0_y0_z0 = do 
  testTraverseFull (V3 1 0 0) (V3 0 0.1 0.1) (V3 (-1) 0 0)
  testTraverseFull (V3 (-1) 0 0) (V3 1.0 0.1 0.1) (V3 1 0 0)
  testTraverseFull (V3 0 1 0) (V3 0.1 0.0 0.1) (V3 0 (-1) 0)
  testTraverseFull (V3 0 (-1) 0) (V3 0.1 1.0 0.1) (V3 0 1 0)
  testTraverseFull (V3 0 0 1) (V3 0.1 0.1 0) (V3 0 0 (-1))
  testTraverseFull (V3 0 0 (-1)) (V3 0.1 0.1 1.0) (V3 0 0 1)

unit_traverse_full_axis_x1_y0_z0 :: Assertion
unit_traverse_full_axis_x1_y0_z0 = do 
  testTraverseFull (V3 0 1 0) (V3 1.1 0.0 0.1) (V3 0 (-1) 0)
  testTraverseFull (V3 0 (-1) 0) (V3 1.1 1.0 0.1) (V3 0 1 0)
  testTraverseFull (V3 0 0 1) (V3 1.1 0.1 0) (V3 0 0 (-1))
  testTraverseFull (V3 0 0 (-1)) (V3 1.1 0.1 1.0) (V3 0 0 1)

unit_traverse_full_axis_x0_y1_z0 :: Assertion
unit_traverse_full_axis_x0_y1_z0 = do 
  testTraverseFull (V3 1 0 0) (V3 0 1.1 0.1) (V3 (-1) 0 0)
  testTraverseFull (V3 (-1) 0 0) (V3 1.0 1.1 0.1) (V3 1 0 0)
  testTraverseFull (V3 0 0 1) (V3 0.1 1.1 0) (V3 0 0 (-1))
  testTraverseFull (V3 0 0 (-1)) (V3 0.1 1.1 1.0) (V3 0 0 1)

testTraverseXRay :: V3 Float -> Assertion
testTraverseXRay hit = do 
  let box :: Aabb Float = Aabb 0 1 
      size = 2
      color1 = V3 1 0 0
      color2 = V3 0 1 0 
      dir = V3 1 0 0
      normal = V3 (-1) 0 0
      sampler p = case floor' p of 
        V3 0 0 0 -> color1 
        V3 1 0 0 -> color2 
        _ -> 0 
      intersection = IntersectResult True hit normal
      traversed = traverseGrid box size sampler dir intersection
  TraverseResult True hit normal color1 @=? traversed

unit_traverse_see_through_x :: Assertion 
unit_traverse_see_through_x = do 
  testTraverseXRay (V3 0.00000000000000000001 0.45 0.447)

testTraverseEmpty :: V3 Float -> V3 Float -> Assertion
testTraverseEmpty dir hit = do 
  let box :: Aabb Float = Aabb 0 1 
      size = 2
      sampler = const 0
      intersection = IntersectResult True hit 0
      traversed = traverseGrid box size sampler dir intersection
  TraverseResult False 0 0 0 @=? traversed

unit_traverse_empty :: Assertion
unit_traverse_empty = do 
  testTraverseEmpty (V3 1 0 0) (V3 0 0.1 0.1)
  testTraverseEmpty (V3 (-1) 0 0) (V3 1.0 0.1 0.1)
  testTraverseEmpty (V3 0 1 0) (V3 0.1 0 0.1)
  testTraverseEmpty (V3 0 (-1) 0) (V3 0.1 1.0 0.1)
  testTraverseEmpty (V3 0 0 1) (V3 0.1 0.1 0)
  testTraverseEmpty (V3 0 0 (-1)) (V3 0.1 0.1 1.0)

unit_traverse_angle_x0_y0_z0 :: Assertion
unit_traverse_angle_x0_y0_z0 = do 
  let box :: Aabb Float = Aabb 0 1 
      size = 2
      sampler p = if fmap floor p == (0 :: V3 Int) then V3 1 0 0 else 0
      dir = normalize $ V3 0.25 (-0.5) 0
      intersection = IntersectResult True (V3 0 0.75 0.1) (V3 (-1.0) 0 0)
      traversed = traverseGrid box size sampler dir intersection
  TraverseResult True (V3 0.125 0.5 0.1) (V3 0 1 0) (V3 1 0 0) @=? traversed

unit_traverse_angle_x1_y0_z1 :: Assertion
unit_traverse_angle_x1_y0_z1 = do 
  let box :: Aabb Float = Aabb 0 1 
      size = 2
      sampler p = if fmap floor p == (V3 1 0 1 :: V3 Int) then V3 1 0 0 else 0
      start = V3 0.75 0.75 0
      target = V3 0.625 0.5 0.625
      dir = normalize $ target - start
      intersection = IntersectResult True start (V3 0 0 (-1))
      traversed = traverseGrid box size sampler dir intersection
  TraverseResult True target (V3 0 1 0) (V3 1 0 0) @=? traversed