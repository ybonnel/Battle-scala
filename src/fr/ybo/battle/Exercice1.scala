package fr.ybo.battle

class Hanoi(val pile1: Seq[Int],
            val pile2: Seq[Int],
            val pile3: Seq[Int],
            val history: Seq[Hanoi]) {

  def isFinished: Boolean = {
    pile1.isEmpty && pile2.isEmpty
  }

  def displayState {
    println("Tour 1 : " + pile1.sorted)
    println("Tour 2 : " + pile2.sorted)
    println("Tour 3 : " + pile3.sorted)
  }

  def isSame(newHanoi: Hanoi): Boolean = {
    return (newHanoi.pile1.sorted == this.pile1.sorted
      && newHanoi.pile2.sorted == this.pile2.sorted
      && newHanoi.pile3.sorted == this.pile3.sorted)
  }

  def displaySolution {
    println("Nombre de coups : " + history.size)
    history.foreach(_.displayState)
    this.displayState
  }

  def play: Seq[Hanoi] = {
    val minPile1 = pile1.reduceOption(_.min(_)).getOrElse(99)
    val minPile2 = pile2.reduceOption(_.min(_)).getOrElse(99)
    val minPile3 = pile3.reduceOption(_.min(_)).getOrElse(99)

    var result: Seq[Hanoi] = Seq()

    if (minPile1 < minPile2) {
      // Pile1 => Pile2
      val newHanoi = new Hanoi(pile1.filter(_ != minPile1),
        pile2.union(Seq(minPile1)),
        pile3,
        history.union(Seq(this)))

      if (!history.find(hanoi => hanoi.isSame(newHanoi)).isDefined) {
        result = result.union(Seq(newHanoi))
      }

    } else if (minPile2 < minPile1) {
      // Pile2 => Pile1
      val newHanoi = new Hanoi(
        pile1.union(Seq(minPile2)),
        pile2.filter(_ != minPile2),
        pile3,
        history.union(Seq(this)))

      if (!history.find(hanoi => hanoi.isSame(newHanoi)).isDefined) {
        result = result.union(Seq(newHanoi))
      }
    }
    if (minPile1 < minPile3) {
      // Pile1 => Pile3
      val newHanoi = new Hanoi(
        pile1.filter(_ != minPile1),
        pile2,
        pile3.union(Seq(minPile1)),
        history.union(Seq(this)))

      if (!history.find(hanoi => hanoi.isSame(newHanoi)).isDefined) {
        result = result.union(Seq(newHanoi))
      }
    } else if (minPile3 < minPile1) {
      // Pile3 => Pile1
      val newHanoi = new Hanoi(
        pile1.union(Seq(minPile3)),
        pile2,
        pile3.filter(_ != minPile3),
        history.union(Seq(this)))

      if (!history.find(hanoi => hanoi.isSame(newHanoi)).isDefined) {
        result = result.union(Seq(newHanoi))
      }
    }

    if (minPile2 < minPile3) {
      // Pile2 => Pile3
      val newHanoi = new Hanoi(
        pile1,
        pile2.filter(_ != minPile2),
        pile3.union(Seq(minPile2)),
        history.union(Seq(this)))

      if (!history.find(hanoi => hanoi.isSame(newHanoi)).isDefined) {
        result = result.union(Seq(newHanoi))
      }
    } else if (minPile3 < minPile2) {
      // Pile3 => Pile2
      val newHanoi = new Hanoi(
        pile1,
        pile2.union(Seq(minPile3)),
        pile3.filter(_ != minPile3),
        history.union(Seq(this)))

      if (!history.find(hanoi => hanoi.isSame(newHanoi)).isDefined) {
        result = result.union(Seq(newHanoi))
      }
    }

    return result
  }
}

object Exercice1 {

  def findSolutionForANumber(number:Int) {
    var allHanoi = Seq(new Hanoi(Seq.range(1, number + 1), Seq(), Seq(), Seq()))

    while (!allHanoi.find(hanoi => hanoi.isFinished).isDefined) {
      allHanoi = allHanoi.flatMap(hanoi => hanoi.play)

      var newHanois = Seq.empty[Hanoi]
      allHanoi.filter(hanoi => {
        val oldHanoi = newHanois.find(_.isSame(hanoi))
        if (oldHanoi.isDefined) {
          if (oldHanoi.get.history.size > hanoi.history.size) {
            newHanois = newHanois.filter(_.isSame(hanoi)).union(Seq(hanoi))
          }
          false
        } else {
          newHanois :+= hanoi
          true
        }
      })
      allHanoi = newHanois
    }

    val solution = allHanoi.find(hanoi => hanoi.isFinished).get

    solution.displaySolution

  }

  def main(args: Array[String]) {

    println("Recherche de solution pour 6")
    findSolutionForANumber(6)

    println("Recherche de solution pour 7")
    findSolutionForANumber(7)

    println("Recherche de solution pour 8")
    findSolutionForANumber(8)
  }
}
