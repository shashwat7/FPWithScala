package interview.indix.tautologyverifier

/**
  * Created by shashwat on 18/4/17.
  */
object ExecuteTautologyVerifier {

  def main(args: Array[String]): Unit = {

    // - Assumptions:
    // - Parsing assumes that all input values are correct propositional statement.
    // - In case you are using any binary operator, please enclose it within brackets, otherwise it will throw array index out of bound!

    val inputValues: Array[String] = Array(
      "a",
      "(a & b)",
      "(a & (b | c))",
      "(!a & !b)",
      "(a | !a)",
      "((a & (!b | b)) | (!a & (!b | b)))",
      "(!a | (a & a))",
      "(!a | (b & !a))",
      "(!a | a)",
      "((a & (!b | b)) | (!a & (!b | b)))"
    )

    inputValues.foreach{ i =>
      try{
        val stmt = Statement(i)
        println("Input String: " + i +
          "\nParsed Statement: " + stmt +
          "\nSolved Statement: " + Statement.solve(stmt) +
          "\nIs Tautology: " + Statement.isTautology(stmt) +
          "\n-------------"
        )
      } catch {
        case e: StringIndexOutOfBoundsException =>
          println("Error in parsing: " + i)
          println("Please check your input string! " +
            "\nMake sure you follow all assumptions. "+
            "\nI guess, it is missing some brackets!" +
            "\n-------------"
          )
      }
    }
  }
}
