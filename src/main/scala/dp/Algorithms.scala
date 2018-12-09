package dp

import scala.collection.mutable.ArrayBuffer

object Algorithms {

   /**
    *   Subproblem: f(i) is the i-th Fibonacci
    *   Recurrence: f(i) = f(i-1) + f(i-2)
    *   O(n)
    */
    def fibonacci(n: Int): Long = 
        if (n == 0) 0L
        else if (n == 1) 1L
        else {
            var fi_2 = 0L
            var fi_1 = 1L
            for(i <- 2 to n){
                val temp = fi_1
                fi_1 = fi_1 + fi_2
                fi_2 = temp
            }
            fi_1
        }

   /**
    *   Subproblem: l(i) is the lis including a(i)
    *   Recurrence: l(i) = 1 + max[ l(j) where j < i and a(j) < a(i) ]
    *   O(n^2)
    */
    def lis(as: Seq[Int]): Int = {
        val lis: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
        for(i <- as.indices){
            var max = 1
            for(j <- 0 to i){
                if(as(j) < as(i) && lis(j) + 1 > max) 
                    max = 1 + lis(j)
            }
            lis += max
        }
        lis.max
    }

   /**
    *   Subproblem: l(i,j) is the lcs in first i of x and first j in y
    *   Recurrence: l(i,j) = 
    *                           1 + l(i-1, j-1), if x(i) == y(j)
    *                           max(l(i-1,j) l(i,j-1)), if x(i) != y(j) 
    *   O(n^2)
    */
    def lcs(xs: Seq[Char], ys: Seq[Char]): Int = {
        val ls: ArrayBuffer[ArrayBuffer[Int]] = ArrayBuffer.fill(xs.size+1)(ArrayBuffer.fill(ys.size+1)(0))
    
        for(i <- 1 to xs.size){
            for(j <- 1 to ys.size){
                if(xs(i-1) == ys(j-1))
                    ls(i)(j) = 1+ ls(i-1)(j-1)
                else
                    ls(i)(j) = math.max(ls(i-1)(j),ls(i)(j-1)  )
            }
        }

        ls(xs.size)(ys.size)
    }
}
