package ta.landmark

class LandmarkRight(_distance: Double, _edgeId: Option[Int], _objId: Int) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Option[Int] = _edgeId
  override val objId: Int = _objId

  override def toString: String = {
    val edge = if (edgeId.isDefined) edgeId.get.toString else "None"
    "LandmarkRight in edgeId " + edge + " between objId " + objId + " in location " + distance
  }
}