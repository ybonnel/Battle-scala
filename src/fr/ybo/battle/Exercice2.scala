package fr.ybo.battle


object Exercice2 {

  def specificSepRegExp = "\\[([^\\[\\]]+)]".r

  def defaultSep = Seq("\n", ",")

  def add(valeur: String): Int = {

    if (valeur.isEmpty) {
      return 0
    }

    val myValue = valeur.split("\n").filter(!_.startsWith("//")).mkString("\n")

    val seps = defaultSep.union(specificSepRegExp.findAllIn(valeur.split("\n").apply(0)).map(oneSep => {
      oneSep.substring(1, oneSep.length - 1)
    }).toSeq)

    val regexpSep = seps.map("(" + _.flatMap("\\" + _) + ")").mkString("|")

    try {
      myValue.split(regexpSep).map(_.toInt).filter(_ <= 1000).sum
    } catch {
      case ignore: NumberFormatException => 9999
    }
  }


  def main(args: Array[String]) {
    assert(add("") == 0)
    assert(add("10") == 10)
    assert(add("10,2") == 12)
    assert(add("10,-20,1") == -9)
    assert(add("1,aa") == 9999)
    assert(add("1\n10,2") == 13)
    assert(add("//[;]\n1;2") == 3)
    assert(add("//[;]\n1;1002") == 1)
    assert(add("//[;][%]\n1;2%1;2") == 6)
    assert(add("//[;][%%][.]\n1;2\n1%%2,-1.4.9.5%%1001,1") == 24)
    assert(add("//[;][%%][.][|]\n1;2\n1%%2|-1.4.9.5%%1001|0") == 23)
  }

}
