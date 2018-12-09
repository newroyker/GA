package dp

trait LIS {
    def generate(as: Seq[Int]): Int = {
        as.indices.foldLeft(Seq.empty[Int]){ case (lis, i) => 
            val max: Int = (0 to i).foldLeft(1){ case (m, j) => 
                if(as(j) < as(i) && m < (lis(j)+1)) lis(j)+1 else m
            }
            lis :+ max
        }
        as.max
    }
}
