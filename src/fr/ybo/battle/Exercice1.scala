package fr.ybo.battle

import java.util.concurrent.TimeUnit

class Hanoi(val tours: Seq[Seq[Int]],
            val history: Seq[Hanoi]) {

  def isFinished: Boolean = {
    tours.zipWithIndex.filter(_._2 != tours.size - 1).filter(!_._1.isEmpty).isEmpty
  }

  def displayState() {
    println(tours.map(tour => {
      "{" + tour.sorted.mkString(",") + "}"
    }).mkString(" "))
  }

  def displaySolution() {
    println("Nombre de coups : " + history.size)
    history.foreach(_.displayState())
    this.displayState()
  }

  def play: Seq[Hanoi] = {

    var toursCanPlay = tours.zipWithIndex
    var otherTours = Seq.empty[(Seq[Int], Int)]

    var result = Seq.empty[Hanoi]

    while (toursCanPlay.size > 1 && toursCanPlay.find(!_._1.isEmpty).isDefined) {

      val minTour = toursCanPlay.minBy(_._1.reduceOption(_.min(_)).getOrElse(99))

      toursCanPlay = toursCanPlay.filter(_._2 != minTour._2)
      val min = minTour._1.min
      val newHistory = history.union(Seq(this))
      val minTourWithoutMin = (minTour._1.filter(_ != min), minTour._2)
      val otherToursWithMin = otherTours.union(Seq(minTourWithoutMin))

      result = result.union(toursCanPlay.map(tourCanPlay => {
        val toursCanPlayWithoutCurrentTour = toursCanPlay.filter(_._2 != tourCanPlay._2)
        val tourCanPlayWithMin = (tourCanPlay._1.union(Seq(min)), tourCanPlay._2)
        new Hanoi(otherToursWithMin.union(toursCanPlayWithoutCurrentTour).union(Seq(tourCanPlayWithMin)).sortBy(_._2).map(_._1)
          , newHistory)
      }))

      otherTours :+= minTour
    }

    result
  }
}

object Exercice1 {

  def findSolutionForANumber(number: Int): Hanoi = {
    var allHanoi = Seq(new Hanoi(Seq(Seq.range(1, number + 1), Seq(), Seq()), Seq()))

    while (!allHanoi.find(hanoi => hanoi.isFinished).isDefined) {
      allHanoi = allHanoi.flatMap(hanoi => hanoi.play).groupBy(hanoi => {
        hanoi.tours
      }).map(allSameHanoi => {
        allSameHanoi._2.minBy(_.history.size)
      }).toSeq
    }

    allHanoi.find(hanoi => hanoi.isFinished).get
  }

  def timer[A](f: => A) = {
    val startTime = System.nanoTime
    val result = f
    println("Time : " + TimeUnit.NANOSECONDS.toMillis(System.nanoTime - startTime) + "ms")
    result
  }

  def main(args: Array[String]) {
    println("Recherche de solution pour 7")
    val result7 = timer(findSolutionForANumber(7))
    result7.displaySolution()
    assert(result7.history.size == 127)

    println("Recherche de solution pour 8")
    val result8 = timer(findSolutionForANumber(8))
    result8.displaySolution()
    assert(result8.history.size == 255)
  }
}
