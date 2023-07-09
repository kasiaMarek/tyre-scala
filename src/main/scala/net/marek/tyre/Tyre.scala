// Tyre
sealed trait Tyre[R]
case class Pred(f: Char => Boolean) extends Tyre[Char]
case class Or[R1, R2](right: Tyre[R1], left: Tyre[R2]) extends Tyre[Either[R1, R2]]
case class And[R1, R2](right: Tyre[R1], left: Tyre[R2]) extends Tyre[(R1, R2)]
case class Star[R](re: Tyre[R]) extends Tyre[List[R]]
case object Epsilon extends Tyre[Unit]
case class Conv[R1, R2](tyre: Tyre[R1], f: R1 => R2) extends Tyre[R2]
