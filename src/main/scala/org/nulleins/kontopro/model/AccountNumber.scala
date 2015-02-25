package org.nulleins.kontopro.model
import scalaz._

trait AccountNumber {
  val noise = """[\s\.,_/:-]""".r

  /** @return a Validation of the supplied `code` with non-significant characters removed */
  def normalize(code: String, min: Int): Validation[String,String] = {
    if(code == null) {
      Failure("Code may not be null")
    } else {
      val result = noise.replaceAllIn(code, "")
      if (result.length >= min) Success(result) else Failure(s"""Length of "$result" less than $min""")
    }
  }

  private val stars: Int => String = Memo.mutableHashMapMemo[Int, String] { num =>
    (for(_ <- 1 to num) yield "*").mkString
  }

  /** @return a copy of the supplied account number, with all but the
    *         specified initial and final portion beclouded by stars
    * @param value of the account number
    * @param first length to be displayed in clear
    * @param last length to be displayed in clear */
  def obfuscate( value: String, first: Int, last: Int) = {
    val prefix = if(first >= 0 && first < value.length) first else 1
    val suffix = if(last >= 0 && last < value.length) last else 1
    val obstars = stars(math.min(value.length - prefix - suffix,value.length))
    s"${value take prefix}$obstars${value drop value.length-suffix}"
  }
}
