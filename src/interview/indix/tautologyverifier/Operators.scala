package interview.indix.tautologyverifier

/**
  * Created by shashwat on 15/4/17.
  * Operator is the parent trait for all operators.
  * It operates on one or more statements. And, the operation is executed by calling operate on the Operator.
  */
sealed trait Operator{
  def operate: Statement
}

case class |(op1: Statement, op2: Statement) extends Operator{
  def operate: Statement = op1 | op2
}
case class &(op1: Statement, op2: Statement) extends Operator {
  def operate: Statement = op1 & op2
}
case class !(op1: Statement) extends Operator {
  def operate: Statement = !op1
}