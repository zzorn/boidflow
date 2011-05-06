package org.boidflow.pool

import simplex3d.math._
import simplex3d.math.float.functions._
import simplex3d.math.float._

/**
 * 
 */
final class PoolBuffer(val size: ConstVec3i) {

  val xMult = 1
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
  def get(x: Int, y: Int, z: Int): Float = buffer(x + y * yMult + z * zMult)

  def getInterpolated(pos: Vec3): Float = getInterpolated(pos.x, pos.y, pos.z)
  def getInterpolated(x: Float, y: Float, z: Float): Float = {

    // Clamp to boundaries
    val cx = clamp(x, 0.5f, internalSize.x + 0.5f)
    val cy = clamp(y, 0.5f, internalSize.y + 0.5f)
    val cz = clamp(z, 0.5f, internalSize.z + 0.5f)

    // Calculate base index and the interpolation values
    val ix = cx.toInt
    val iy = cy.toInt
    val iz = cz.toInt
    val ex = cx - ix
    val ey = cy - iy
    val ez = cz - iz
    val sx = 1f - ex
    val sy = 1f - ey
    val sz = 1f - ez
    val baseIndex = xMult * ix + yMult * iy + zMult * iz

    // Interpolate x and y separately for z planes 0 and 1
    val xy0 = sy * (sx * buffer(baseIndex                ) + ex * buffer(baseIndex + xMult                )) +
              ey * (sx * buffer(baseIndex + yMult        ) + ex * buffer(baseIndex + xMult + yMult        ))
    val xy1 = sy * (sx * buffer(baseIndex + zMult        ) + ex * buffer(baseIndex + xMult + zMult        )) +
              ey * (sx * buffer(baseIndex + yMult + zMult) + ex * buffer(baseIndex + xMult + yMult + zMult))

    // Interpolate the z planes and return result
    sz * xy0 + ez * xy1
  }

  def scaleAdd(source: PoolBuffer, scale: Float) {
    val s = source.buffer
    var i = 0
    while (i < totalSize) {
      buffer(i) += s(i) * scale
      i += 1
    }
  }


  def diffuse(previous: PoolBuffer,
              boundary: Boundary,
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
            // Optimize by extracting index calculation to variable from innermost loop
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

      boundary.applyTo(this)

      k += 1
    }
  }

  /**
   * Does a linear backtrace using velocity vector to move the buffer contents from previous buffer to this.
   */
  def advect(previous: PoolBuffer,
             boundary: Boundary,
             velocity: PoolBuffer3,
             timeStep: Float,
             advectCellsPerSecond: Float = 1f) {

    val backStep = -timeStep * Vec3(advectCellsPerSecond, advectCellsPerSecond, advectCellsPerSecond)

    val sourcePos = Vec3(0f, 0f, 0f)

    var pos = Vec3i(0,0,0)
    pos.x = 1
    while (pos.x <= internalSize.x) {

      pos.y = 1
      while (pos.y <= internalSize.y) {

        pos.z = 1
        while (pos.z <= internalSize.z) {
          // Get index
          val index = pos.x + pos.y * yMult + pos.z * zMult

          // Get velocity to sourcePos
          velocity.get(index, sourcePos)

          // Backtrack
          sourcePos *= backStep
          sourcePos += pos

          // Interpolate from source
          buffer(index) = previous.getInterpolated(sourcePos)

          pos.z += 1
        }

        pos.y += 1
      }

      pos.x += 1
    }

    boundary.applyTo(this)
  }

}