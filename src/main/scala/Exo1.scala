object Exo1 extends App {



  val linesTest = List("100", "10 5", "1 0")
  /**
    * - si vous terminez dans le top 100, vous gagnez 1 000 euros ;
    * - si vous terminez le marathon, vous gagnez 100 euros ;
    * - si vous ne terminez pas le marathon, vous aurez juste perdu votre crédibilité.
    */
  val lines: List[String] = io.Source.stdin.getLines.toList

  val placeDepart: Int = lines.head.toInt

  val dataByKm: List[(Int, Int)] = lines.tail.map{ s =>
    val Array(gain, perte) = s.split(" ").map(_.toInt)
    (gain, perte)
  }

  val finalPlace: Int = placeDepart - dataByKm.map{case (gain, perte) => gain-perte}.sum

  def montant(place: Int): String = {
    if(place <= 100 ) "1000"
    else if (place <= 10000) "100"
    else "KO"
  }

  println(montant(finalPlace))


}
