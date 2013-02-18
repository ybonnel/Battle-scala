package fr.ybo.battle


object Exercice3 {

  def calculate(feuille:String):Int = {

    // decomposition en frames
    var countFrame = 0
    var inFrame = false
    feuille.foreach( oneCoup => {
      oneCoup match {
        case 'X' => {
          countFrame+=1
          inFrame=false
        }
        case _ => {
          if (inFrame) {
            inFrame = false
            countFrame += 1
          } else {
            inFrame = true
          }
        }
      }
    })
    if (inFrame) {
      countFrame += 1
    }

    val moreThan10Frames = countFrame > 10


    ("00" + feuille).sliding(3).zipWithIndex.map(
       oneCoup => {
         val lastCoup = oneCoup._1.charAt(2)
         val score = (lastCoup match {
           case 'X' => 10
           case '/' => 10 - oneCoup._1.charAt(1).toString.toInt
           case '-' => 0
           case _ => lastCoup.toString.toInt
         })

         var multiple = 1
         if (oneCoup._1.charAt(0) == 'X' && (oneCoup._2  < feuille.length - 1 || !moreThan10Frames)) {
           multiple += 1
         }
         if ((oneCoup._1.charAt(1) == '/' || oneCoup._1.charAt(1) == 'X') && (oneCoup._2  < feuille.length - 2 || !moreThan10Frames)) {
           multiple += 1
         }

         score * multiple
       }
    ).sum

  }

  def main(args:Array[String]) {
    assert(calculate("12121212121212121212") == 30)
    assert(calculate("------------------32") == 5)
    assert(calculate("XXXXXXXXXXXX") == 300)
    assert(calculate("5/5/5/5/5/5/5/5/5/5/5") == 150)
    assert(calculate("XXX728/17X7/9-XX8") == 180)
    assert(calculate("XXX728/17X7/9-X2/") == 172)
    assert(calculate("2/2-5/6/-99-X----XX1") == 89)
    assert(calculate("122/X12121212123--3") == 57)
    assert(calculate("122/X12121212123--XX1") == 75)
  }
}
