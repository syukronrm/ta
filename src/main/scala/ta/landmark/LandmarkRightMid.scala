package ta.landmark

class LandmarkRightMid(_distance: Double, _edgeId: Option[Int], _objId: Int, _objDominatedId: Int) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Option[Int] = _edgeId
  override val objId: Int = _objId
  val objDominatedId: Int = _objDominatedId
}