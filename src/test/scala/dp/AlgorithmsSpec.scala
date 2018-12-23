package dp

import org.scalatest._
import Algorithms._

class AlgorithmsSpec extends FlatSpec with MustMatchers {
  "fibonacci" should "return fib(n) where n = [0,7]" in {
    val tests: Map[Int, Long] = Map(0 -> 0L, 1 -> 1L, 2 -> 1L, 3 -> 2L, 4 -> 3L, 5 -> 5L, 6 -> 8L, 7 -> 13L)
    tests.foreach {
      case (k, v) => fibonacci(k) must be(v)
    }
  }

  "lis" should "return longest increasing sub sequence" in {
    lis(Seq(5, 7, 4, -3, 9, 1, 10, 4, 5, 8, 9, 3)) must be(6)
  }

  "lcs" should "return longest common sub sequence" in {
    lcs(
      Seq('B', 'C', 'D', 'B', 'C', 'D', 'A'),
      Seq('A', 'B', 'E', 'C', 'B', 'A')) must be(4)

    lcs(
      Seq('A', 'B', 'E', 'C', 'B', 'A'),
      Seq('B', 'C', 'D', 'B', 'C', 'D', 'A')) must be(4)
  }

  "csms" should "return max sum of contiguous sub sequence" in {
    csms(Seq(5, 15, -30, 10, -5, 40, 10)) must be(55)
  }

  "sow" should "return if a string is made up of words" in {
    def dict(cs: Seq[Char]): Boolean =
      Set("i", "a", "it", "was", "the", "best", "of", "times").map(_.toSeq).contains(cs)

    sow("itwasthebestoftimes", dict) must be(true)

    sow("itwasthebedtoftimes", dict) must be(false)
  }

  "lcss" should "return longest common sub string / contiguous sub sequence" in {
    lccs("abcdxyz".toSeq, "xyzabcd".toSeq) must be(4)

    lccs("zxabcdezy".toSeq, "yzabcdezx".toSeq) must be(6)

    lccs("abcdaf".toSeq, "zbcdf".toSeq) must be(3)
  }

  "ksnr" should "return max value" in {
    ksnr(Seq(15,12,10,5), Seq(15,10,8,1), 22) must be (18)
  }

  "ksr" should "return max value" in {
    ksr(Seq(15,12,10,5), Seq(15,10,8,1), 22) must be (18)
  }
}
