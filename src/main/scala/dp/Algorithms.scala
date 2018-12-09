package dp

object Algorithms {
    def fibonacci(n: Int): Long = if(n < 0) 1L else 
        (1 until n)
            .foldLeft((1L,1L)){case ((li_2, li_1), _) => (li_1, li_1 + li_2)}
            ._2

    def lis(as: Seq[Int]): Int = {
        val ls = as.indices.foldLeft(Seq.empty[Int]){ case (lis, i) => 
            val max: Int = (0 to i).foldLeft(1){ case (m, j) => 
                if(as(j) < as(i) && m < (lis(j)+1)) lis(j)+1 else m
            }
            lis :+ max
        }
        ls.max
    }

    def lcs(xs: Seq[Char], ys: Seq[Char]): Int = ???
}
