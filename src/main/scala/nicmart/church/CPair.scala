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
 * Trait Description
 */
trait CPair[X, Y] {
  def apply[T](f: X => Y => T): T
  override def toString = this(x => y => s"($x, $y)")
}

object CPair {
  def apply[X, Y](x: X, y: Y) = new CPair[X, Y] {
    def apply[T](f: X => Y => T) = f(x)(y)
  }

  def first[X, Y](p: CPair[X, Y]) = p[X]{ x: X => y: Y => x }
  def second[X, Y](p: CPair[X, Y]) = p[Y]{ x: X => y: Y => y }

  def church[X, Y](p: (X, Y)): CPair[X, Y] = CPair(p._1, p._2)
  def unchurch[X, Y](p: CPair[X, Y]): (X, Y) = p(x => y => (x, y))
}
