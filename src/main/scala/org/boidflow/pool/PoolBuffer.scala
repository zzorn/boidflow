package org.boidflow.pool

import simplex3d.math._

/**
 * 
 */
final class PoolBuffer(val size: ConstVec3i) {

  val yMult = size.x
  val zMult = size.x * size.y

  private val nextX =  1
  private val prevX = -1
  private val nextY =  yMult
  private val prevY = -yMult
  private val nextZ =  zMult
  private val prevZ = -zMult

  val totalSize = size.x * size.y * size.z

  val internalSize = size - Vec3i(2, 2, 2) // Without boundaries

  val totalInternalSize = internalSize.x * internalSize.y * internalSize.z

  val buffer = new Array[Float](totalSize)

  def set(pos: Vec3i, value: Float) {buffer(pos.x + pos.y * yMult + pos.z * zMult) = value}
  def get(pos: Vec3i): Float = buffer(pos.x + pos.y * yMult + pos.z * zMult)

  def scaleAdd(source: PoolBuffer, scale: Float) {
    val s = source.buffer
    var i = 0
    while (i < totalSize) {
      buffer(i) += s(i) * scale
      i += 1
    }
  }


  def diffuse(previous: PoolBuffer,
              setBoundaries: (PoolBuffer) => Unit,
              diff: Float,
              timeStep: Float) {
    val a = timeStep * diff * totalInternalSize

    val s = previous.buffer
    val d = buffer

    val xSize = internalSize.x
    val ySize = internalSize.y
    val zSize = internalSize.z

    // Gauss-Seidel relaxation solver
    val relaxationSteps = 20
    var k = 0
    while (k < relaxationSteps) {

      var x = 1
      while (x <= xSize) {

        var y = 1
        while (y <= ySize) {

          var z = 1
          while (z <= zSize) {
            // Optimize by extracting index to variable from inner loop
            // TODO: Do we really want to use destination as the source?
            val i = x + y * yMult + z * zMult
            d(i) = (s(i) + a * (d(i+prevX) + d(i+nextX) +
                                d(i+prevY) + d(i+nextY) +
                                d(i+prevZ) + d(i+nextZ))) /
                   (1f + 6f * a)

            z += 1
          }

          y += 1
        }

        x += 1
      }

      setBoundaries(this)

      k += 1
    }
  }

}