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

  "or" should {
    "return false" when {
      "both arguments are false" in {
        assert(or(fls)(fls)(false)(true))
      }
    }
    "return true" when {
      "one argument is true" in {
        assert(or(fls)(tru)(true)(false))
        assert(or(tru)(fls)(true)(false))
      }
      "both arguments are true" in {
        assert(or(tru)(tru)(true)(false))
      }
    }
  }

  "xor" should {
    "return false" when {
      "both arguments are false" in {
        assert(xor(fls)(fls)(false)(true))
      }
      "both arguments are true" in {
        assert(xor(tru)(tru)(false)(true))
      }
    }
    "return true" when {
      "exactly one argument is true" in {
        assert(xor(fls)(tru)(true)(false))
        assert(xor(tru)(fls)(true)(false))
      }
    }
  }

  "not" should {
    "return true" when {
      "false" in {
        assert(not(fls)(true)(false))
      }
    }
    "return false" when {
      "true" in {
        assert(not(tru)(false)(true))
      }
    }
  }

  "ifthenelse" should {
    "return the first block" when {
      "the argument is true" in {
        assert(ifthenelse(tru)(1)(2) === 1)
      }
    }
    "return the second block" when {
      "the argument is false" in {
        assert(ifthenelse(fls)(1)(2) === 2)
      }
    }
  }

  "unchurch" should {
    "return a native true" when {
      "a Church true is given" in {
        assert(unchurch(tru))
      }
    }
    "return a native false" when {
      "a Church false is given" in {
        assert(!unchurch(fls))
      }
    }
  }

  "church" should {
    "return church true" when {
      "a native true is given" in {
        assert(church(true)(true)(false))
      }
    }
    "return church false" when {
      "a native false is given" in {
        assert(!church(false)(true)(false))
      }
    }
  }
}
