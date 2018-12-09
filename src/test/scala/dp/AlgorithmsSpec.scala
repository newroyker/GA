package dp

import org.scalatest._
import Algorithms._

class AlgorithmsSpec extends FlatSpec with MustMatchers {
    "fibonacci" should "return fib(n) where n = [0,7]" in {
        val tests: Map[Int, Long] = Map(0 -> 0L, 1 -> 1L, 2 -> 1L, 3 -> 2L, 4 -> 3L, 5 -> 5L, 6 -> 8L, 7 -> 13L)
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
            Seq('A', 'B', 'E', 'C', 'B', 'A')) must be (4)

        lcs(
            Seq('A', 'B', 'E', 'C', 'B', 'A'),
            Seq('B', 'C', 'D', 'B', 'C', 'D', 'A')) must be (4)
    }

    "csms" should "return max sum of contig subseq" in {
        csms(Seq(5, 15, -30, 10, -5, 40, 10)) must be (55)
    }
}
