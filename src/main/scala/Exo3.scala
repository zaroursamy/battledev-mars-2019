object Exo3 extends App {


  /**
    * 4
    * ....
    * o.*o
    * ....
    * ....
    */
  val input1 = List("4", "....","o.*o","....","....")

  val lines: List[String] = input1//io.Source.stdin.getLines.toList

  val size: Int = lines.head.toInt

  val matrix: List[String] = lines.tail

  def get(i: Int, j: Int)(aMatrix: List[String]): Char = aMatrix.map(_.apply(j)).apply(i)

  def getEmpl(whatIWant: Char)(data: List[String]) = (for {
    i <- 0 until size
    j <- 0 until size
    if get(i,j)(data) == whatIWant
  } yield (i,j)).toList

  def getPieceEmpl(data: List[String]): List[(Int, Int)] = getEmpl('o')(data)

  def getMultEmpl(data: List[String]): List[(Int, Int)] = getEmpl('*')(data)


  def parkourPieceLineI(i: Int)(aMatrix: List[String]) = {
    if(i%2==0) parkourPairPiece(i)(aMatrix)
    else parkourImpairPiece(i)(aMatrix)
  }

  def parkourPairPiece(i: Int)(aMatrix: List[String]): String = {
    val pieceEmplacement: List[Int] = getPieceEmpl(aMatrix).filter(_._1 == i).map(_._2)

    var arr = Array.fill[String](size)(">")
    pieceEmplacement.foreach{j =>
      arr(j) = "x>"
    }

    arr = arr.mkString("").toCharArray.map(_.toString)
    arr(arr.length-1)="v"

    arr.mkString("")
  }



  def parkourImpairMult(i: Int)(aMatrix: List[String]) = {
    val multEmplacement = getMultEmpl(aMatrix).filter(_._1 == i).map(_._2)
    var arr = Array.fill[String](size)(">")
    multEmplacement.foreach{j =>
      arr(j) = "x>"
    }

    arr = arr.mkString("").toCharArray.map(_.toString)
    arr(arr.length-1)="^"

    arr.mkString("")
  }

  def parkourImpairPiece(i: Int)(aMatrix: List[String]): String = {

    val pieceEmplacement: List[Int] = getPieceEmpl(aMatrix).filter(_._1 == i).map(_._2)
    var arr =  Array.fill[String](size)("<")

    pieceEmplacement.foreach{j =>
      arr(j) = "<x"
    }

    arr = arr.mkString("").toCharArray.map(_.toString)
    arr(0)="v"
    arr.mkString("").reverse
  }

  def parkourPairMult(i: Int)(aMatrix: List[String]) = {
    val multEmplacement: List[Int] = getMultEmpl(aMatrix).filter(_._1 == i).map(_._2)
    var arr =  Array.fill[String](size)("<")

    multEmplacement.foreach{j =>
      arr(j) = "<x"
    }

    arr = arr.mkString("").toCharArray.map(_.toString)
    arr(0)="^"
    arr.mkString("").reverse
  }

  def parkourMultLineI(i: Int)(aMatrix: List[String]) = {
    if(i%2 == 0) parkourPairMult(i)(aMatrix)
    else parkourImpairMult(i)(aMatrix)
  }

  def concatParkour(aMatrix: List[String]): String = {
    val resPiece = (0 until size).map{i => parkourPieceLineI(i)(aMatrix)}.toList

    val newResPiece = resPiece.dropRight(1) ::: resPiece(size-1).dropRight(1) :: Nil

    val resMult = (0 until size).map{i => parkourMultLineI(i)(aMatrix)}.reverse.toList
    val newResMult = resMult.dropRight(1) ::: resMult(size-1).dropRight(1) :: Nil

    newResPiece.mkString("")+newResMult.mkString("")

  }

  println(concatParkour(matrix))

}
