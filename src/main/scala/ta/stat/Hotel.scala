package ta.stat

object Hotel {
  var hotel: List[Int] = List()

  def add(i: Int) = {
    hotel = hotel :+ i
  }

  def flush() = {
    hotel = List()
  }

  def average() = {
    hotel.sum.toDouble / hotel.size
  }

  override def toString: String = hotel.toString
}
