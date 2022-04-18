import org.scalatest.flatspec.AnyFlatSpec

class SetTest extends AnyFlatSpec {
  //Test cases of Empty set
  val set1 = new EmptySet[SetValue]
  // Test case of incl method
  "incl method" should "include element in EmptySet" in {
    val set2 = set1.incl(SetValue(10)).incl(SetValue(20))
    assert(set2.contains(SetValue(10)))
  }
  // Test case of incl method
  "Contain method" should "check weather the element is present(true) or not(false) " in {
    val set3 = set1.incl(SetValue(10)).incl(SetValue(20)).incl(SetValue(40)).incl(SetValue(50))
    assert(!set3.contains(SetValue(30)))
    assert(set3.contains(SetValue(40)))
  }
  // Test cases of NonEmptySet
  val set3= new EmptySet[SetValue]
  val set4 = new NonEmptySet[SetValue](SetValue(3),set3.incl(SetValue(7)),set3.incl(SetValue(10)))
  // Test case of incl method
  "incl method " should "include element in NonEmptySet" in {
    val set5 = set4.incl(SetValue(12)).incl(SetValue(20))
    assert(set5.contains(SetValue(12)))
  }
  //Test case of contain method
  "Contain method" should "check weather the element is present(true) or not(false) in NonEmptySet" in {
    assert(set4.contains(SetValue(10)))
    assert(!set4.contains(SetValue(2)))
  }
}