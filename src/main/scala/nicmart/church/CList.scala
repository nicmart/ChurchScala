/*
 * (c) 2015 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.church

import CPair._
import CBoolean._

/**
 * Trait Description
 */
trait CList[X] {
  def apply[R](f: X => R => R)(z: R): R

  override def toString = "(" + first(this{
    x: X => pair: CPair[String, String] => CPair(x + second(pair) + first(pair), ", ")
  }(CPair("", ""))) + ")"
}

object CList {
  def nil[X] = new CList[X] {
    def apply[R](f: X => R => R)(z: R): R = z
  }

  def isNil[X](list: CList[X]): CBoolean = list[CBoolean](_ => _ => fls)(tru)

  def cons[X](head: X)(tail: CList[X]) = new CList[X] {
    def apply[R](f: X => R => R)(z: R): R = f(head)(tail(f)(z))
  }

  def head[X](list: CList[X]): X = list[Unit => X](head => tail => _ => head)(_ => ???)(())

  def tail[X](list: CList[X]): CList[X] = {
    second(list[CPair[CList[X], CList[X]]] {
      x => pair => CPair[CList[X], CList[X]](cons[X](x)(first(pair)), first(pair))
    } (CPair[CList[X], CList[X]](nil, nil)))
  }

  def append[X](left: CList[X])(right: CList[X]): CList[X] = {
    left[CList[X]](hd => tail => cons(hd)(tail))(right)
  }

  def map[X, Y](f: X => Y)(list: CList[X]): CList[Y] = {
    list[CList[Y]](head => tail => cons(f(head))(tail))(nil[Y])
  }

  def flatMap[X, Y](f: X => CList[Y])(list: CList[X]): CList[Y] = {
    list[CList[Y]](head => tail => append(f(head))(tail))(nil[Y])
  }

  def church[X](list: List[X]): CList[X] = list.foldRight[CList[X]](nil[X]){
    case (head, tail) => cons(head)(tail)
  }

  def unchurch[X](list: CList[X]): List[X] = list[List[X]](head => tail => head :: tail)(Nil)
}
