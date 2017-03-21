package chapter6

/**
  * Created by srastogi on 21-Mar-17.
  */
sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine{
  type MachineState = State[Machine, (Int, Int)]

  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) => Machine(locked = true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State{
    machineState: Machine  => {
      val machineEndState = inputs.foldLeft(machineState){case (ms,i) => update(i)(ms)}
      ((machineEndState.candies, machineEndState.coins), machineEndState)
    }
  }

}