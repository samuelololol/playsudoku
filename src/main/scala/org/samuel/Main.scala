package org.samuel
import scala.BigInt

class Cell (var x:Int, var y:Int, var confirm:Int = 0) {
  var predict = Set(1,2,3,4,5,6,7,8,9)
  var zone = calcZone(x, y)


  private[this] def calcZone(x:Int, y:Int): Int = {
    var xr = (BigInt(x) /% 3)._1.toInt
    var yr = (BigInt(y) /% 3)._1.toInt
    return xr * 3 + yr
  }

  private[this] def checkConfirm: Unit = {
    if (predict.size == 1) {
      confirm = predict.iterator.next
    }
  }

  private[this] def removePossible(ele: Int):Unit = {
    predict -= ele
    checkConfirm
  }

  private[this] def removePossible(ele: Array[Int]):Unit = {
    ele.foreach{ predict -= _}
    checkConfirm
  }

  def checkRow(matrix: Array[Array[Cell]]):Unit = {
    matrix(this.x).filter(_.confirm != 0).foreach {
      i => removePossible(i.confirm)
    }
  }

  def checkCol(matrix: Array[Array[Cell]]):Unit = {
    matrix.flatten.filter(_.y == this.y).filter(_.confirm != 0).foreach {
      i => removePossible(i.confirm)
    }
  }

  def checkCircle(matrix: Array[Array[Cell]]):Unit = {
    matrix.flatten.filter(_.zone == this.zone).filter(_.confirm != 0).foreach {
      x => removePossible(x.confirm)
    }
  }
}

object print_matrix(matrix: Array[Array[Cell]]):Unit = {
  (0 to 8).foreach{ x=>
    println("x: " + x)
  }
}

object Main extends App {
  if (args.length == 0) {
    println("please enter numbers 9x9, 81 numbers")
    System.exit(1)
  }

  val number_string = args.toList.map(_.toInt)
  val matrix = Array.ofDim[Cell](9, 9)

  number_string.grouped(9).toList.zipWithIndex.foreach{ case(x, xi) =>
      x.zipWithIndex.foreach { case (y, yi) =>
          matrix(xi)(yi) = new Cell(xi, yi)
      }
  }

  //display
  (0 to 8).foreach { x =>
    (0 to 8).foreach{ y =>
      println(s"(${matrix(x)(y).x}, ${matrix(x)(y).y}) = ${matrix(x)(y).zone}")
    }
  }

  (0 to 8).foreach { x=>
    println(s"zone ${x}")
    matrix.flatten.filter(_.zone == x).foreach { i =>
      println(s"(${matrix(i.x)(i.y).x}, ${matrix(i.x)(i.y).y}) = ${matrix(i.x)(i.y).zone}")
    }
  }
}
