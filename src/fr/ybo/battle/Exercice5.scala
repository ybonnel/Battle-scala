package fr.ybo.battle

class Player(val sequence:Seq[Int]) {
  var position = 0
  var currentPlay = 0

  def play {
    if (currentPlay == sequence.size) {
      currentPlay = 0
    }

    position += sequence.apply(currentPlay)

    position = position match {
      case 19 => 17
      case 32 => 33
      case 51 => 48
      case _ => position
    }

    currentPlay += 1
  }
}

object Exercice5 {

  def calculateBestPlayer(players:Seq[Player]) {
    while (players.filter(_.position >= 56).isEmpty) {
      players.foreach(_.play)
    }
    players.zipWithIndex.foreach(playerWithIndex => {
      println("Player " + (playerWithIndex._2 +1) + ", case : " + playerWithIndex._1.position)
    })
  }

  def main(args:Array[String]) {
    calculateBestPlayer(Seq(new Player(Seq(2, 2, 4)), new Player(Seq(1, 1, 6))))
  }



}
