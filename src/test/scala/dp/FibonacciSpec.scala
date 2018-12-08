package dp

import org.scalatest._

class FibonacciSpec extends FlatSpec with MustMatchers with Fibonacci {
    val tests: Map[Int, Long] = Map(0 -> 1L, 1 -> 1L, 2 -> 2L, 3 -> 3L, 4 -> 5L, 5 -> 8L, 6 -> 13L, 7 -> 21L)
    
    "generate" should "return fib(n)" in {
        tests.foreach{
            case (k,v) =>
                generate(k) must be (v)
        }
    }
}
