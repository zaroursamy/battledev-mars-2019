object Exo2 extends App{


  val linesTest = List("4", "50","51","30","1")

  val lines: List[String] = linesTest//io.Source.stdin.getLines.toList

  val nbCarton = lines.head.toInt

  val maxWeight = 100

  val weights: List[Int] = lines.tail.map(_.toInt)

  Seq(List(50), List(51,30,1))

  val res = weights.foldLeft(Seq.empty[List[Int]]){
    case (acc, newWeight) =>
      if(acc.lastOption.getOrElse(Nil).sum + newWeight <= 100) {
        if(acc.reverse.nonEmpty) acc.reverse.tail.reverse ++ Seq(acc.last ::: newWeight :: Nil) else Seq(acc.lastOption.getOrElse(Nil) ::: newWeight :: Nil)
      }
      else acc ++ Seq(List(newWeight))
  }

  println(res.size)

}
