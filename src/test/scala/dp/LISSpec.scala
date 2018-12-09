package dp

import org.scalatest._

class LISSpec extends FlatSpec with MustMatchers with LIS {
    
    "generate" should "LIS" in {
        generate(Seq(5, 7, 4, -3, 9, 1, 10, 4, 5, 8, 9, 3)) must be (6)
    }
}
