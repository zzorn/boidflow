package org.boidflow.pool

/**
 * 
 */

class PoolBoundaries {

  // TODO

  def densityBoundary: Boundary = null
  def velocityBoundary: Boundary3 = null

}

class Boundary {

  // TODO: Have pool call boundary with coordinate values instead -> boundary can just be environment function

  def applyTo(pool: PoolBuffer) = null
}

case class Boundary3(x: Boundary, y: Boundary, z: Boundary)

