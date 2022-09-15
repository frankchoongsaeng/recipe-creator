import mill._, scalalib._

object specifier extends ScalaModule {
  def scalaVersion = "2.12.12"

  override def ivyDeps = Agg(
    ivy"io.circe::circe-core:0.14.1",
    ivy"io.circe::circe-generic:0.14.1",
    ivy"io.circe::circe-parser:0.14.1",
    ivy"com.lihaoyi::cask:0.8.3",
  )
}

object test extends TestModule {
  override def testFrameworks = ???
}