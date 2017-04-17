import interview.indix.tautologyverifier.Statement

val testValues = Array(
  "a",
  "0",
  "1",
  "(a & b)",
  "(a & !a)",
  "(a & a)",
  "(a | b)",
  "(a | !a)",
  "(a | a)",
  "!(a | !a)",
  "((a | !a) & (a | a))",
  "((a | !a) | (!a & a))",
  "((a | !a) & (!a & a))",
  "((a & !a) | (!a | a))",
  "((a & !a) & (!a | a))",
  "((a & !a) | (!a & a))",
  "((a | !a) & (!a | a))"
)

testValues.foreach{ i =>
  try{
    val stmt = Statement(i)
    println("Input String: " + i +
      "\nParsed Statement: " + stmt +
      "\nSolved Statement: " + Statement.solve(stmt) +
      "\n-------------"
    )
  } catch {
    case e: StringIndexOutOfBoundsException =>
      println("Error in parsing: " + i)
  }
}


