package dp

import org.scalatest._
import Algorithms._

class AlgorithmsSpec extends FlatSpec with MustMatchers {
    "fibonacci" should "return fib(n) where n = [0,7]" in {
        val tests: Map[Int, Long] = Map(0 -> 1L, 1 -> 1L, 2 -> 2L, 3 -> 3L, 4 -> 5L, 5 -> 8L, 6 -> 13L, 7 -> 21L)
        tests.foreach{
            case (k,v) => fibonacci(k) must be (v)
        }
    }

    "lis" should "return longest increasing sub sequence" in {
        lis(Seq(5, 7, 4, -3, 9, 1, 10, 4, 5, 8, 9, 3)) must be (6)
    }

    "lcs" should "return longest common sub sequence" in {
        lcs(
            Seq('B', 'C', 'D', 'B', 'C', 'D', 'A'), 
            Seq('A', 'B', 'E', 'C', 'B', 'A', 'B')) must be (4)
    }
}
