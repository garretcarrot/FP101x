import scala.language.postfixOps

abstract sealed class Action
case class Atom(atom: Unit => Action) extends Action {
  override def toString() = "atom"
}
case class Fork(a1: Action, a2: Action) extends Action {
  override def toString = s"fork ${a1 toString} ${a2 toString}"
}
case class Stop() extends Action {
  override def toString = "stop"
}

class Concurrent[A](val func: (A => Action) => Action) {

  import Concurrent.roundRobin

  def andThen[B](after: Concurrent[B]): Concurrent[B] = flatMap(_ => after)

  def action(): Action = func(a => Stop())
  def fork(): Concurrent[Unit] = Concurrent(
    (cont: Unit => Action) => Fork(action(), cont(()))
  )
  def flatMap[B](mapper: A => Concurrent[B]): Concurrent[B] = Concurrent(
    cont => func(a => mapper(a).action())
  )

  def run(): () => Unit = roundRobin(List[Action](action))
}

object Concurrent {
  def apply[A](func: (A => Action) => Action) = new Concurrent[A](func)
  def of[A](a: A) = Concurrent((cont: A => Action) => cont(a))
  
  def stop[A](): Concurrent[A] = Concurrent(cont => Stop())
  def atom[A](ioA: Unit => A): Concurrent[A] = Concurrent(cont => Atom(_ => cont(ioA(()))))
  def par[A](c1: Concurrent[A], c2: Concurrent[A]): Concurrent[A] = Concurrent(
    (cont: A => Action) => Fork(c1.action(), c2.action())
  )

  private def roundRobin(list: List[Action]): () => Unit = 
    list match {
      case Nil => () => ()
      case Stop() :: actions => roundRobin(actions)
      case Atom(atom) :: actions => roundRobin(actions :+ atom(()))
      case Fork(a1, a2) :: actions => roundRobin(actions ++ List(a1, a2))
    }
}
