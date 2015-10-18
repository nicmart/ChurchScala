/*
 * (c) 2015 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.church

import CBoolean._
import CPair._

/**
 * Class Description
 */
trait CNat {
  def apply[T](s: T => T)(z: T): T
  override def toString = CNat.church(this).toString
}

object CNat {
  val zero: CNat = new CNat {
    def apply[T](s: T => T)(z: T): T = z
  }

  val one = succ(zero)

  def iszero(n: CNat): CBoolean = n[CBoolean](_ => fls)(tru)

  def succ(n: CNat): CNat = new CNat {
    def apply[T](s: T => T)(z: T): T = s(n(s)(z))
  }

  def pred(n: CNat): CNat = {
      second(n[CPair[CNat,CNat]](p => CPair(succ(first(p)), first(p)))(CPair(zero, zero)))
  }

  def plus(n: CNat)(m: CNat): CNat = new CNat {
    def apply[T](s: T => T)(z: T): T = n(s)(m(s)(z))
  }

  def plus2(n: CNat)(m: CNat): CNat = n(succ)(m)

  def minus(n: CNat)(m: CNat): CNat = m(pred)(n)

  def times(n: CNat)(m: CNat): CNat = n(plus(m))(zero)

  def exp(base: CNat)(exp: CNat): CNat = exp(times(base))(one)

  def church(n: CNat): Int = n[Int](_ + 1)(0)

  def unchurch(n: Int): CNat = n match {
    case 0 => zero
    case _ => succ(unchurch(n - 1))
  }
}
