package dp

trait Fibonacci {
    def generate(n: Int): Long = if(n < 0) 1L else 
        (1 until n)
            .foldLeft((1L,1L)){case ((li_2, li_1), _) => (li_1, li_1 + li_2)}
            ._2
}