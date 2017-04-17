package interview.indix.tautologyverifier

import scala.collection.mutable

/**
  * Created by shashwat on 15/4/17.
  * Statement is the parent trait for a Propositional Statement.
  * Statements are made from further sub-statements(ComplexStatements), connected via Operators.
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

  override def toString: String = root match{
    case &(op1,op2) => "(" + op1.toString + " & " + op2.toString + ")"
    case |(op1,op2) => "(" + op1.toString + " | " + op2.toString + ")"
    case !(op1) => "!(" + op1.toString + ")"
  }
}

object Statement{
  val ONE: Statement = One
  val ZERO: Statement = Zero

  /**
    * Solve will reduce the statement to it's simplest form.
    * Example:
    * (a | !a) ==> 1
    * (a | b) & (a | !a) ==> (a | b)
    * @param stmt is the statement to be solved
    * @return solved statement
    * */
  def solve(stmt: Statement): Statement = stmt match{
    case c: Constant => c
    case v: Variable => v
    case complex: ComplexStatement =>
      complex.root match{
        case &(op1,op2) =>
          val solvedLeftOperator = solve(op1)
          val solvedRightOperator = solve(op2)
          solvedLeftOperator & solvedRightOperator
        case |(op1,op2) =>
          val solvedLeftOperator = solve(op1)
          val solvedRightOperator = solve(op2)
          solvedLeftOperator | solvedRightOperator
        case !(op1) =>
          val solvedOperator = solve(op1)
          solvedOperator.unary_!
      }
  }

  /***
    * Checks whether stmt is a tautology or not.
    * @param stmt statement to be checked
    * @return true if it is a tautology else false
    */
  def isTautology(stmt: Statement): Boolean = solve(stmt).equals(ONE)

  /**
    * This function parses a given string to a Statement.
    * Assumptions:
    * 1. Parsing assumes that all input values are correct propositional statement.
    * 2. In case you are using any binary operator, please enclose it within brackets, otherwise it will throw String index out of bound!
    * @param input String to be parsed.
    * @return A propositional Statement.
    * */
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
    var idx = 0
    while(idx < input.length){
      val c = input.charAt(idx)
      if(c != ' '){
        if(isOpeningBracket(c)) stackPreparedStatements.push(c)
        else if(isOne(c)) stackPreparedStatements.push(ONE)
        else if(isZero(c)) stackPreparedStatements.push(ZERO)
        else if(isVariable(c)) stackPreparedStatements.push(Variable(c))
        else if(isUnaryOperator(c)){
          val nextChar = input.charAt(idx+1)
          if(isVariable(nextChar)) {stackPreparedStatements.push(Variable(nextChar,negated = true)); idx = idx + 1}
          else if(isOpeningBracket(nextChar)){
            var j = idx + 2
            while(!isClosingBracket(input.charAt(j))) j = j + 1
            stackPreparedStatements.push(ComplexStatement(new !(Statement(input.substring(idx + 1, j+1)))))
            idx = j + 1
          }
        }
        else if(isBinaryOperator(c)){
          val opr1 = stackPreparedStatements.pop().asInstanceOf[Statement]
          var j = idx + 1
          while(!isClosingBracket(input.charAt(j))) j = j + 1
          val opr2 = Statement(input.substring(idx+1, j+1))
          if(c == '&') stackPreparedStatements.push(ComplexStatement(&(opr1,opr2)))
          else if(c == '|') stackPreparedStatements.push(ComplexStatement(|(opr1,opr2)))
          idx = j+1
        }
        else if(isClosingBracket(c)){
          val stmt = stackPreparedStatements.pop().asInstanceOf[Statement]
          if(stackPreparedStatements.nonEmpty) stackPreparedStatements.pop() // remove opening brackets
          stackPreparedStatements.push(stmt)
        }
      }
      idx = idx + 1
    }
    stackPreparedStatements.pop().asInstanceOf[Statement]
  }
}
