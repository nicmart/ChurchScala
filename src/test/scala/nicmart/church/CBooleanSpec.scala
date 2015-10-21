/*
 * (c) 2015 Nicolò Martini
 *
 * http://nicolo.martini.io
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */

package nicmart.church

import org.scalatest._

/**
 * Class Description
 */
class CBooleanSpec extends WordSpec
{
  import CBoolean._

  "True" should {
   "return the first argument" when {
     "used as a function" in {
       assert(tru(1)(2) === 1)
     }
   }
  }
  "False" should {
    "return the second argument" when {
      "used as a function" in {
        assert(fls(1)(2) === 2)
      }
    }
  }

  "and" should {
    "return true" when {
      "both arguments are true" in {
        assert(and(tru)(tru)(true)(false))
      }
    }
    "return false" when {
      "one argument is false" in {
        assert(and(fls)(tru)(false)(true))
        assert(and(tru)(fls)(false)(true))
      }
      "both arguments are false" in {
        assert(and(fls)(fls)(false)(true))
      }
    }
  }
}
