package fr.ybo.battle

import java.util.concurrent.TimeUnit
import collection.parallel.ParSeq


class Pion(val x: Int, val y: Int, var poid:BigInt=1) {

  def play(size: Int): Seq[Pion] = {

    var result = Seq.empty[Pion]
    if (x < size - 1) {
      result = Seq(new Pion(x + 1, y, poid))
    }
    if (y < size - 1) {
      result :+= new Pion(x, y + 1, poid)
    }

    result
  }

  def isSame(other:Pion):Boolean = {
    (other.x == this.x && other.y == this.y)
  }

}


object Exercice4 {

  def calculate(size: Int): BigInt = {

    var allPion = Seq(new Pion(0, 0))

    val nbCoups = size + size - 2
    var pourcentage = 0
    var oldPourcentage = 0
    for (index <- 1 to nbCoups) {

      pourcentage = (index * 10/ nbCoups)
      if (pourcentage != oldPourcentage) {
        println("Avancement : " + (pourcentage*10) + "%")
        oldPourcentage = pourcentage
      }

      allPion = allPion.flatMap(pion => pion.play(size)).groupBy(pion => {
        (pion.x, pion.y)
      }).map(pions => {
        new Pion(pions._1._1, pions._1._2, pions._2.map(_.poid).sum)
      }).toSeq
    }

    allPion.map(_.poid).sum
  }

  def displayASolution(size: Int) {
    println(size.toString + 'x' + size.toString)
    val startTime = System.nanoTime
    println(calculate(size))
    val elapsedTime = System.nanoTime - startTime
    println("Temps de calcul : " + TimeUnit.NANOSECONDS.toMillis(elapsedTime) + "ms")

  }

  def main(args: Array[String]) {
    displayASolution(2)
    displayASolution(3)
    displayASolution(4)
    displayASolution(5)
    displayASolution(7)
    displayASolution(10)
    displayASolution(10000)

  }


}
