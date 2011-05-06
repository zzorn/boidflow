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

  val x = new PoolBuffer(size)
  val y = new PoolBuffer(size)
  val z = new PoolBuffer(size)

  def set(pos: Vec3i, value: inVec3) {
    val index = pos.x + pos.y * yMult + pos.z * zMult
    x.buffer(index) = value.x
    y.buffer(index) = value.y
    z.buffer(index) = value.z
  }

  def get(pos: Vec3i, valueOut: outVec3) {
    get(pos.x + pos.y * yMult + pos.z * zMult, valueOut)
  }

  def get(index: Int, valueOut: outVec3) {
    valueOut.x = x.buffer(index)
    valueOut.y = y.buffer(index)
    valueOut.z = z.buffer(index)
  }

  def scaleAdd(source: PoolBuffer3, scale: Float) {
    x.scaleAdd(source.x, scale)
    y.scaleAdd(source.y, scale)
    z.scaleAdd(source.z, scale)
  }

  def diffuse(previous: PoolBuffer3, boundary: Boundary3, diff: Float, timeStep: Float) {
    x.diffuse(previous.x, boundary.x, diff, timeStep)
    y.diffuse(previous.y, boundary.y, diff, timeStep)
    z.diffuse(previous.z, boundary.z, diff, timeStep)
  }

  /**
   * Does a linear backtrace using velocity vector to move the buffer contents from previous buffer to this.
   */
  def advect(previous: PoolBuffer3, boundary: Boundary3, velocity: PoolBuffer3, timeStep: Float, advectCellsPerSecond: Float = 1f) {
    x.advect(previous.x, boundary.x, velocity, timeStep, advectCellsPerSecond)
    y.advect(previous.y, boundary.y, velocity, timeStep, advectCellsPerSecond)
    z.advect(previous.z, boundary.z, velocity, timeStep, advectCellsPerSecond)
  }


  def project() {
    
  }


}