package fr.ybo.battle

import collection.mutable


object Exercice7 {

  def main(args: Array[String]) {

    val sommes:mutable.HashMap[Int, Int] = mutable.HashMap.empty

    (0 to 59).flatMap(minute => {
      (0 to 23).map(heure => {
        heure.toString + minute.toString
      })
    }).map(affichageReveil => {
      affichageReveil.map(_.toString.toInt).sum
    }).foreach( somme => {
      sommes.put(somme, sommes.getOrElse(somme, 0) + 1)
    })

    println(sommes.maxBy(_._2))
  }
}
