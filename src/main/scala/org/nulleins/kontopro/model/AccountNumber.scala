package org.nulleins.kontopro.model

object AccountNumber {
  val stars = "******************************"
  val noise = """[\s\.,_/:-]""".r

  /** @return an Option of the supplied `code` with non-significant characters removed */
  def normalize(code: String, min: Int) = {
    if(code == null) {
      None
    } else {
      val result = noise.replaceAllIn(code, "")
      if (result.length >= min) Some(result) else None
    }
  }

  /** @return a version of the supplied account number, with all but
    *         the specified initial and final portion replaced by stars
    * @param value of the account number
    * @param initial length to be displayed in clear
    * @param end length to be displayed in clear */
  def obsusticate( value: String, initial: Int, end: Int) = {
    val length = value.length
    val newInitial = if(initial < length) initial else 1
    val newEnd = if(end < length) end else 1
    val oblen = length - newInitial - newEnd
    s"${value.substring(0,newInitial)}${stars.substring(0,math.min(oblen,length))}${value.substring(length - end)}"
  }
}
