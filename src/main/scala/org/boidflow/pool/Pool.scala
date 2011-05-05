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

  


}