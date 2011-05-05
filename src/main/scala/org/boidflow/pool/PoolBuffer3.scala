package org.boidflow.pool

import simplex3d.math.{Vec3i, ConstVec3i}
import simplex3d.math.float.functions._
import simplex3d.math.float._

/**
 * 
 */

final class PoolBuffer3(size: ConstVec3i) {

  val yMult = size.x
  val zMult = size.x * size.y
  val totalSize = size.x * size.y * size.z

  val bufferX = new Array[Float](totalSize)
  val bufferY = new Array[Float](totalSize)
  val bufferZ = new Array[Float](totalSize)

  def set(pos: Vec3i, value: inVec3) {
    val index = pos.x + pos.y * yMult + pos.z * zMult
    bufferX(index) = value.x
    bufferY(index) = value.y
    bufferZ(index) = value.z
  }

  def get(pos: Vec3i, valueOut: outVec3) {
    val index = pos.x + pos.y * yMult + pos.z * zMult
    valueOut.x = bufferX(index)
    valueOut.y = bufferY(index)
    valueOut.z = bufferZ(index)
  }

}