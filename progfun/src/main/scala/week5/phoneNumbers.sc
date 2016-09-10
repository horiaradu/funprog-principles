object phoneNumbers {
  val words = List("JAVA", "Home", "Horia", "Oana", "Arci", "jaub")

  val mnemonics = Map(2 -> "ABC", 3 -> "DEF", 4 -> "GHI", 5 -> "JKL",
    6 -> "MNO", 7 -> "PQRS", 8 -> "TUV", 9 -> "WXYZ")

  val charCode: Map[Char, Char] =
    for {
      (code, chars) <- mnemonics
      char <- chars
    } yield char -> (code + '0').toChar


  def wordCode(word: String) = word.toUpperCase.map(charCode)

  val wordsForNum: Map[String, Seq[String]] = words.groupBy(wordCode).withDefaultValue(Seq())

  def encode(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet

  def translate(number: String): Set[String] =
    encode(number).map(_.mkString(" "))

  translate("528246742")
}