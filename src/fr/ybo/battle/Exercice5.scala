package fr.ybo.battle

class Player(val sequence:Seq[Int]) {
  var position = 0
  var nbCoups = 0
  var history = Seq.empty[(Int, Int)]

  def newPosition(currentPlay:Int):Int = {
    (position + currentPlay) match {
      case 19 => 17
      case 32 => 33
      case 51 => 48
      case _ => position + currentPlay
    }
  }

  def playToTheEnd() {
    nbCoups = Stream.continually(sequence.toStream).flatten.takeWhile(currentPlay => {
      position = newPosition(currentPlay)
      history :+= (currentPlay, position)
      position < 56
    }).size
  }
}

object Exercice5 {

  def calculateBestPlayer(players:Seq[Player]) {
    players.foreach(_.playToTheEnd())
    players.zipWithIndex.foreach(playerWithIndex => {
      println("Player " + (playerWithIndex._2 +1) + ", case : " + playerWithIndex._1.position + ", nbCoups : " + playerWithIndex._1.nbCoups)
      println("History : ")
      println(playerWithIndex._1.history.map(unCoup => {
        "Coup : " + unCoup._1 + ", position : " + unCoup._2
      }).mkString("\n"))
    })
  }

  def main(args:Array[String]) {
    calculateBestPlayer(Seq(new Player(Seq(2, 2, 4)), new Player(Seq(1, 1, 6))))
  }



}
