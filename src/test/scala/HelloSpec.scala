import org.scalatest._
import org.scalatest.matchers.ShouldMatchers

// Tests obviously not implemented yet :)
class HelloSpec extends FlatSpec with ShouldMatchers {
  "Hello" should "have tests" in {
    true should be === true
  }
}
