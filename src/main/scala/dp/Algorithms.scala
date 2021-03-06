package dp

import common.{MutableRow, MutableTable}

object Algorithms {

  /**
    * Sub-problem: f(i) is the i-th Fibonacci
    * Recurrence: f(i) = f(i-1) + f(i-2)
    * O(n)
    */
  def fibonacci(n: Int): Long =
    if (n == 0) 0L
    else if (n == 1) 1L
    else {
      val fs = MutableRow[Long](n+1)
      fs.set(0, 0L)
      fs.set(1, 1L)
      for (i <- 2 to n) {
        fs.set(i, fs(i-1) + fs(i-2))
      }
      fs(n)
    }

  /**
    * Sub-problem: l(i) is the lis including a(i)
    * Recurrence: l(i) = 1 + max[ l(j) where j < i and a(j) < a(i) ]
    * O(nˆ2)
    */
  def lis(as: Seq[Int]): Int = {
    val lis = MutableRow[Int](as.size)
    for (i <- as.indices) {
      var max = 1
      for (j <- 0 to i) {
        if (as(j) < as(i) && lis(j) + 1 > max)
          max = 1 + lis(j)
      }
      lis.set(i,max)
    }
    lis.max
  }

  /**
    * Sub-problem: l(i,j) is the lcs in first i of x and first j in y
    * Recurrence: l(i,j) =
    *     1 + l(i-1, j-1), if x(i) == y(j)
    *     max(l(i-1,j) l(i,j-1)), if x(i) != y(j)
    * O(nˆ2)
    */
  def lcs(xs: Seq[Char], ys: Seq[Char]): Int = {
    val ls = MutableTable[Int](xs.size + 1, ys.size + 1)
    ls.fillCol(0, 0)
    ls.fillRow(0, 0)

    for (i <- 1 to xs.size) {
      for (j <- 1 to ys.size) {
        if (xs(i - 1) == ys(j - 1))
          ls.set(i, j, 1 + ls(i - 1, j - 1))
        else
          ls.set(i, j, math.max(ls(i - 1, j), ls(i, j - 1)) )
      }
    }

    ls(xs.size, ys.size)
  }

  /**
    * Sub-problem: m(i) is the max sum of contiguous sub sequence including a(i)
    * Recurrence: m(i) = a(i) + max( m(i-1) , 0 )
    * O(n)
    */
  def csms(as: Seq[Int]): Int = {
    val mis = MutableRow[Int](as.size)
    for (i <- as.indices) {
      if (i == 0)
        mis.set(i, as(i))
      else
        mis.set(i, as(i) + math.max(mis(i - 1), 0))
    }
    mis.max
  }

  /**
    * Sub-problem: s(i) is true if characters from 0 to i (inclusive) can be split into words
    * Recurrence: s(i) =
    *     true, if exists a 0 <= j < i so that s(j-1) = true AND characters from j -to- i is a word
    *     false, otherwise
    * O(nˆ2)
    */
  def sow(cs: Seq[Char], dict: Seq[Char] => Boolean): Boolean = {
    val sis = MutableRow[Boolean](cs.size)

    for (i <- cs.indices) {
      if (i == 0)
        sis.set(0, dict(Seq(cs.head)) )
      else {
        var exists = false
        for (j <- 0 until i) {
          val word = cs.slice(j, i + 1)
          if (j == 0)
            exists = dict(word)
          else {
            if (sis(j - 1) && dict(word)) exists = true
          }
        }
        sis.set(i,exists)
      }
    }
    sis(cs.size-1)
  }

  /**
    * Sub-problem: l(i,j) is the lccs in first i of x and first j in y
    * Recurrence: l(i,j) =
    *     1 + l(i-1, j-1), if x(i) == y(j)
    *     0, if x(i) != y(j)
    * O(nˆ2)
    */
  def lccs(xs: Seq[Char], ys: Seq[Char]): Int = {
    val ls = MutableTable[Int](xs.size + 1, ys.size + 1)
    ls.fillCol(0,0)
    ls.fillRow(0,0)

    for (i <- 1 to xs.size) {
      for (j <- 1 to ys.size) {
        if (xs(i - 1) == ys(j - 1))
          ls.set(i, j, 1 + ls(i - 1, j - 1))
        else
          ls.set(i, j, 0)
      }
    }

    ls.max
  }

  /**
    * Sub-problem: k(i,b) = max value using i objects and total weight <= b
    * Recurrence: k(i,b) =
    *     max{ k(i-1, b-w_i) + v_i, k(i-1, b)}, if w_i <= b
    *     k(i-1, b), else
    * O(n*B)
    */
  def ksnr(ws: Seq[Int], vs: Seq[Int], bound: Int): Int = {
    val ks = MutableTable[Int](ws.size + 1, bound + 1)
    ks.fillCol(0,0)
    ks.fillRow(0,0)

    for(i <- 1 to ws.size){
      for(b <- 1 to bound){
        ks.set(i, b,
          if(ws(i-1) <= b)
            math.max(ks(i-1, b-ws(i-1)) + vs(i-1), ks(i-1, b))
          else
            ks(i-1, b)
        )
      }
    }
    ks(ws.size, bound)
  }

  /**
    * Sub-problem: k(b) = max value using objects and total weight <= b
    * Recurrence: k(b) =
    *     max{ v_i + k(b - w_i)}, if w_i <= b for all i
    *     0, else
    * O(n*B)
    */
  def ksr(ws: Seq[Int], vs: Seq[Int], bound: Int): Int = {
    val ks = MutableRow[Int](bound+1)
    ks.fill(0)

    for(b <- 0 to bound){
      for(i <- ws.indices){
        if(ws(i) <= b)
          ks.set(b, math.max(ks(b), vs(i) + ks(b - ws(i))))
      }
    }
    ks(bound)
  }
}
