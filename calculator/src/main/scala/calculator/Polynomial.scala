package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(
      a: Signal[Double],
      b: Signal[Double],
      c: Signal[Double]
  ): Signal[Double] =
    Signal {
      val aVal: Double = a()
      val bVal: Double = b()
      val cVal: Double = c()
      bVal * bVal - 4 * aVal * cVal
    }

  def computeSolutions(
      a: Signal[Double],
      b: Signal[Double],
      c: Signal[Double],
      delta: Signal[Double]
  ): Signal[Set[Double]] =
    Signal {
      if delta() < 0 then Set()
      else
        val bMinus = -b()
        val aTwice = 2 * a()
        val deltaSqrt: Double = math.sqrt(delta())
        Set((bMinus - deltaSqrt) / aTwice, (bMinus + deltaSqrt) / aTwice)
    }
