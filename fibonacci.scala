object Math {

  def fibonacci (n: Int): Int = {

    @annotation.tailrec
    def go (n: Int, a: Int): Int =
      if (n == 1) a + 1
      else go(n-1, a + n)

    go(n, 0)

  }

}

println(Math fibonacci 10000000)