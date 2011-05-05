import sbt._
import com.github.retronym.OneJarProject

class Project(info: ProjectInfo) extends DefaultProject(info) with IdeaProject with OneJarProject { // with AkkaProject

//  override def mainClass = Some("org.foo.MainObj")

  // Scala unit testing
  val scalatest = "org.scalatest" % "scalatest" % "1.3"

  // Store all JME3 jar files in own directory under lib/
  def jme3Jars = descendents("lib" / "jme3", "*.jar")

  // Simplex libs
  def simplex3dJars = descendents("lib" / "simplex3d", "*.jar")

  override def unmanagedClasspath = super.unmanagedClasspath +++ jme3Jars +++ simplex3dJars

/*
  // Akka libraries, uncomment if / as needed. 
  val akkaStm = akkaModule("stm")
  val akkaTypedActor = akkaModule("typed-actor")
  val akkaRemote = akkaModule("remote")
  val akkaHttp = akkaModule("http")
  val akkaAmqp = akkaModule("amqp")
  val akkaCamel = akkaModule("camel")
  val akkaSpring = akkaModule("spring")
  val akkaJta = akkaModule("jta")
  val akkaCassandra = akkaModule("persistence-cassandra")
  val akkaMongo = akkaModule("persistence-mongo")
  val akkaRedis = akkaModule("persistence-redis")
*/
  
}
