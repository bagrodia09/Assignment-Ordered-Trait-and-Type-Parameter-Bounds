trait Set[A <: Ordered[A]] {
  def incl(x: A): Set[A]
  def contains(x: A): Boolean
}
class EmptySet[A <: Ordered[A]] extends Set[A] {
  def contains(x: A): Boolean = false
  def incl(x: A): Set[A] = new NonEmptySet(x, new EmptySet[A], new EmptySet[A])
}
class NonEmptySet[A <: Ordered[A]](elem: A, left: Set[A], right: Set[A]) extends Set[A] {
  def contains(x: A): Boolean =
    if (x < elem) {
      left contains x
    }
    else if (x > elem) {
      right contains x
    }
    else {
      true
    }
  def incl(x: A): Set[A] =
    if (x < elem) {
      new NonEmptySet(elem, left incl x, right)
    }
    else if (x > elem) {
      new NonEmptySet(elem, left, right incl x)
    }
    else {
      this
    }
}
case class SetValue(value: Int) extends Ordered[SetValue] {
  def compare(that: SetValue): Int =
    if (this.value < that.value) {
      -1
    }
    else if (this.value > that.value){
      1
    }
    else 0
}