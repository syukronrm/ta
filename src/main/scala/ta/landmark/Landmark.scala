package ta.landmark

trait Landmark {
  val distance: Double // Distance to Node S
  val edgeId: Option[Int]
  val objId: Int

  override def toString: String = {
    val edge = if (edgeId.isDefined) edgeId.get.toString else "None"
    "edgeId " + edge + " distance " + distance.toString + " obj " + objId
  }
}