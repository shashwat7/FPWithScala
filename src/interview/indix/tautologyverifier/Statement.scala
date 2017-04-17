package interview.indix.tautologyverifier

import scala.collection.mutable

/**
  * Created by shashwat on 15/4/17.
  * Statement is the parent trait for a Propositional Statement.
  * Statements are made from further sub-statements, connected via Operators.
  * Their smallest form being
  * Variables - a,b,c...z ; !a,!b,!c...!z
  * Constants - 0 and 1
  */
sealed trait Statement{
  def |(that: Statement): Statement
  def &(that: Statement): Statement
  def unary_! :Statement
}

/**
  * Variables are simple statements like:
  * a,b,c...z OR !a,!b,!c...!z
  * */
case class Variable(name: Char, negated: Boolean = false) extends Statement {
  def |(that: Statement): Statement = that match{
    case x: Constant => x | this // 1 | a = 1, 0 | a = a
    case y: Variable if y.equals(this) => this // a | a = a
    case y: Variable if y.equals(!this) => One // a | !a = 1
    case y: Variable => ComplexStatement( new |(this, y) ) // a | b
    case y: Statement => ComplexStatement( new |(this, y) ) // a | (a & b)
  }

  def &(that: Statement): Statement = that match{
    case x: Constant => x & this // 1 & a = a, 0 & a = 0
    case y: Variable if y.equals(this) => this // a & a = a
    case y: Variable if y.equals(!this) => Zero // a & !a = 0
    case y: Variable => ComplexStatement( new &(this, y) ) // a & b
    case y: Statement => ComplexStatement( new &(this, y) ) // a & (a | b)
  }

  def unary_! :Statement = Variable(name, !negated)

  override def toString: String = if(negated) "!" + name else name.toString
}


/**
  * Constant in propositional statements are of two types: Zeroes(0) and Ones(1)
  * */
trait Constant extends Statement

case object One extends Constant {
  def |(that: Statement): Statement = One
  def &(that: Statement): Statement = that
  def unary_! :Statement = Zero
  override def toString: String = "1"
}

case object Zero extends Constant{
  def |(that: Statement): Statement = that
  def &(that: Statement): Statement = Zero
  def unary_! :Statement = One
  override def toString: String = "0"
}

/**
  * Complex Statements involves more than one Variable and/or Constants
  * Like: a & b; a | !b.
  * They are saved as a Tree where root is an Operator.
  * */
case class ComplexStatement(root: Operator) extends Statement{
  def |(that: Statement): Statement = that match{
    case y: Constant => y | this // 1 | (a & b) = 1, 0 | (a & b) = (a & b)
    case y: ComplexStatement if y.equals(this) => this // (a & b) | (a & b) = (a & b)
    case y: ComplexStatement if y.equals(! this) => One // (a & b) | !(a & b) = One
    case y: Statement => ComplexStatement( new |(this, y) ) // (a & b) | (b & c)
  }
  def &(that: Statement): Statement = that match{
    case y: Constant => y & this // 1 & (a & b) = 1, 0 & (a & b) = (a & b)
    case y: ComplexStatement if y.equals(this) => this // (a & b) & (a & b) = (a & b)
    case y: ComplexStatement if y.equals(! this) => Zero // (a & b) & !(a & b) = Zero
    case y: Statement => ComplexStatement( new &(this, y) ) // (a & b) | (b & c)
  }
  def unary_! :Statement = ComplexStatement( new !(this) )
}

object Statement{
  val ONE: Statement = One
  val ZERO: Statement = Zero
  def variable(c: Char, n: Boolean): Statement = Variable(c,n)
  def solve(stmt: Statement): Statement = stmt match{
    case c: Constant => c
    case v: Variable => v
    case complex: ComplexStatement => solve(complex.root.operate)
  }
  def isTautology(stmt: Statement): Boolean = solve(stmt).equals(ONE)

  def apply(input: String): Statement = {
    def isVariable(s: Char): Boolean = s.toLower >= 'a' && s.toLower <= 'z'
    def isOne(s: Char): Boolean = s == '1'
    def isZero(s: Char): Boolean = s == '0'
    def isUnaryOperator(s: Char): Boolean = s == '!'
    def isBinaryOperator(s: Char): Boolean = s == '|' || s == '&' || s == '!'
    def isOpeningBracket(s: Char): Boolean = s == '('
    def isClosingBracket(s: Char): Boolean = s == ')'
    val stackPreparedStatements = new mutable.Stack[Any]()
    //  ((a & (!b | b)) | (!a & (!b | b)))
    //  ((a & (!b | b)) | (!a & (!b | b)))
    input.zipWithIndex.foreach{ case (c,idx) =>
      if(isOpeningBracket(c)) stackPreparedStatements.push(c)
      if(isOne(c)) stackPreparedStatements.push(ONE)
      if(isZero(c)) stackPreparedStatements.push(ZERO)
      if(isVariable(c)) stackPreparedStatements.push(Variable(c))




      if(isClosingBracket(c)){
        var top: Char = stack.pop()
        var preparedStatement: Statement = _
        do{
          preparedStatement = top match{
            case c if isOne(c) => ONE
            case c if isZero(c) => ZERO
            case c if isVariable(c)=> Variable(c)
            case c if isUnaryOperator(c) => ComplexStatement( new !(preparedStatement) )
            case c if isBinaryOperator(c) =>
              val op1 = stack.pop() // This could be ) , a/b/c ,
          }
        } while (!isOpeningBracket(top))
      }
      if(c != ' ') stack.push(c)
    }
  }
}
