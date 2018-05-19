package ta.stream

case class ExpiredObject(id: Int) extends Stream {
  override def getId: Int = id
}
