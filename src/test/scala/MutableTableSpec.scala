import org.scalatest.{FlatSpec, MustMatchers}

class MutableTableSpec extends FlatSpec with MustMatchers {

  "MutableRow" should "be empty on creation" in {
    val r = MutableRow[Int](5)

    r.toString must be("( , , , , )")

  }

  it should "allow setting values" in {
    val r = MutableRow[Int](5)
    r.set(2, 7)

    r.toString must be("( , ,7, , )")

    r.get(2) must be(Some(7))

    r.get(0) must be(None)

  }

  it should "allow filling" in {
    val s = 5
    val r = MutableRow[Int](s)
    r.fill(-1)

    r.toString must be("(-1,-1,-1,-1,-1)")

    (0 until s).foreach {
      i => r.get(i) must be(Some(-1))
    }

  }

  "MutableTable" should "be empty on creation" in {
    val t = MutableTable[Int](4, 5)

    t.toString must be(
      """|[( , , , , )
         | ( , , , , )
         | ( , , , , )
         | ( , , , , )]""".stripMargin)

  }

  it should "allow setting values" in {
    val t = MutableTable[Int](4, 5)

    t.set(1, 2, -1)

    t.toString must be(
      """|[( , , , , )
         | ( , ,-1, , )
         | ( , , , , )
         | ( , , , , )]""".stripMargin)

    t.get(1, 2) must be(Some(-1))

    t.get(1, 1) must be(None)

  }

  it should "allow filling a row" in {
    val t = MutableTable[Int](4, 5)

    t.fillRow(1, -1)

    t.toString must be(
      """|[( , , , , )
         | (-1,-1,-1,-1,-1)
         | ( , , , , )
         | ( , , , , )]""".stripMargin)

    (0 until 5).foreach {
      c => t.get(1, c) must be(Some(-1))
    }

    (0 until 5).foreach {
      c => t.get(2, c) must be(None)
    }

  }

  it should "allow filling a col" in {
    val t = MutableTable[Int](4, 5)

    t.fillCol(1, -1)

    t.toString must be(
      """|[( ,-1, , , )
         | ( ,-1, , , )
         | ( ,-1, , , )
         | ( ,-1, , , )]""".stripMargin)

    (0 until 4).foreach {
      r => t.get(r, 1) must be(Some(-1))
    }

    (0 until 4).foreach {
      r => t.get(r, 0) must be(None)
    }
  }

  it should "allow filling a row and col" in {
    val t = MutableTable[Int](4, 5)

    t.fillCol(0, 0)
    t.fillRow(0, 0)

    t.toString must be(
      """|[(0,0,0,0,0)
         | (0, , , , )
         | (0, , , , )
         | (0, , , , )]""".stripMargin)

    (0 until 4).foreach {
      r => t.get(r, 0) must be(Some(0))
    }

    (0 until 5).foreach {
      c => t.get(0, c) must be(Some(0))
    }
  }
}
