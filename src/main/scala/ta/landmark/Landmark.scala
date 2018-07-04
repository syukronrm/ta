package ta.landmark

trait Landmark {
  val distance: Double
  val edgeId: Option[Int]
  val objId: Int
}