package org.boidflow.pool

import simplex3d.math.{ConstVec3i, Vec3i}
import javax.naming.OperationNotSupportedException

/**
 * Some volume filled with a fluid.
 */
class Pool(val size: ConstVec3i) {

  val yMult = size.x
  val zMult = size.x * size.y
  val totalSize = size.x * size.y * size.z

  val velocity     = new PoolBuffer3(size)
  val prevVelocity = new PoolBuffer3(size)

  val density      = new PoolBuffer(size)
  val prevDensity  = new PoolBuffer(size)

  val sources      = new PoolBuffer(size)

  val boundaries: PoolBoundaries = null

  def densityStep(density1: PoolBuffer, density0: PoolBuffer, velocity: PoolBuffer3, diff: Float, timeStep: Float, advectCellsPerSecond = 1f) {
    density1.scaleAdd(density0, timeStep)
    density0.diffuse(density1, boundaries.densityBoundary, diff, timeStep)
    density1.advect(density0, boundaries.densityBoundary, velocity, timeStep, advectCellsPerSecond)
  }

  def velocityStep(velocity1: PoolBuffer3, velocity0: PoolBuffer3, viscosity: Float, timeStep: Float, advectCellsPerSecond = 1f) {

    velocity1.scaleAdd(velocity0, timeStep)

    velocity0.diffuse(velocity1, boundaries.velocityBoundary, viscosity, timeStep)

    // ptoject

    velocity1.advect(velocity0, boundaries.velocityBoundary, velocity0, timeStep, advectCellsPerSecond)

    // ptoject
  }
  

}