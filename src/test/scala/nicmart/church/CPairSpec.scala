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
class CPairSpec extends WordSpec
{
  import CPair._

  val firstElement = 123
  val secondElement = "Second"
  val pair = CPair(firstElement, secondElement)

  "A Church Pair" when {
    "passed to the first function" should {
      "return the first element of the pair" in {
        assert(first(pair) === firstElement)
      }
    }
    "passed to the second function" should {
      "return the second element of the pair" in {
        assert(second(pair) === secondElement)
      }
    }
    "passed to the unchurch function" should {
      "be converted to a native scala Pair" in {
        assert(unchurch(pair) == (firstElement, secondElement))
      }
    }
  }

  "A scala Pair" when {
    "passed to the curch function" should {
      "be converted to a Church Pair" in {
        church((firstElement, secondElement)) {
          x => y => assert(x == firstElement && y == secondElement)
        }
      }
    }
  }
}
