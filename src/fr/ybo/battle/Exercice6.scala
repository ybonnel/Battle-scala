package fr.ybo.battle


object Exercice6 {

  def main(args:Array[String]) {
    println(
    (0 to 59).flatMap( minute => {
      (0 to 23).map(heure => {
        heure.toString +  minute.toString
      })
    }).map( affichageReveil => {
      (affichageReveil, affichageReveil.map(_.toString.toInt).sum)
    }
    ).maxBy(_._2))
  }

}
