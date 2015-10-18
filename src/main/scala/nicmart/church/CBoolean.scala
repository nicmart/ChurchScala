/*
 * (c) 2015 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.church

/**
 * Class Description
 */
trait CBoolean {
  def apply[T](x: T)(y: T): T
}

object CBoolean {

  type Bool[T] = T => T => T

  val tru = new CBoolean {
    def apply[T](x: T)(y: T) = x
  }

  val fls = new CBoolean {
    def apply[T](x: T)(y: T): T = y
  }

  def and(x: CBoolean)(y: CBoolean): CBoolean = x(y)(fls)
  def or(x: CBoolean)(y: CBoolean): CBoolean = x(tru)(y)
  def not(x: CBoolean): CBoolean = x(fls)(tru)
  def xor(x: CBoolean)(y: CBoolean): CBoolean = x(not(y))(y)

  def ifthenelse[T](b: CBoolean)(x: => T)(y: => T) = b(x)(y)

  def church(x: Boolean): CBoolean = if (x) tru else fls
  def unchurch(x: CBoolean): Boolean = x(true)(false)
}
