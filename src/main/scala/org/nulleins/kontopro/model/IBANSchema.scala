package org.nulleins.kontopro.model

import com.typesafe.config._

case class IBANScheme(bank: String, branch: String, account: String) {
  val pattern = s"""([A-Z]{2})([0-9]{2})(${map(bank)})(${map(branch)})(${map(account)})""".r
  def valid(value: String) = pattern.findFirstIn(value)
  def countryCode(value: String) = value match { case pattern(cc, _*) => ISO3166(cc) }
  def bankCode(value: String) = value match { case pattern(_, _, a, _*) => a }
  def branchCode(value: String) = value match { case pattern(_, _, _, b, _) => b }
  def accountNumber(value: String) = value match { case pattern(_, _, _, _, c) => c }

  lazy val length = Seq(bank, branch, account).foldLeft(4) { (a, s) => {
    s match {
      case specPattern(n, _) => a + n.toInt
      case _ => 0
    }
  }
  }

  private lazy val specPattern = """(\d+)([anc])""".r
  private lazy val typeMap = Map("a" -> "A-Za-z", "n" -> "\\d", "c" -> "A-Za-z0-9")
  private def map(spec: String) = {
    spec match {
      case specPattern(n, t) => s"""[${typeMap(t)}]{$n}"""
      case _ => ""
    }
  }
}

object IBANScheme {
  import scala.collection.JavaConversions._

  private lazy val configRoot = ConfigFactory.load("iban-schemes").getObject("iban-schemes")
  private lazy val schemes = configRoot.unwrapped.toMap.map {
    case (k: String, v: java.util.Map[String, String]) =>
      ISO3166(k) -> IBANScheme(v.getOrElse("bank", ""), v.getOrElse("branch", ""), v.getOrElse("account", ""))
  }

  def lookupScheme(countryCode: ISO3166): Option[IBANScheme] = schemes.get(countryCode)
  def lookupScheme(value: String): Option[IBANScheme] = lookupScheme(ISO3166(value take 2))

  def valid(value: String) = (for {
    normal <- AccountNumber.normalize(value)
    scheme <- IBANScheme.lookupScheme(normal)
    ok <- scheme.valid(normal)
  } yield IBANChecker.checksumValid(ok)).getOrElse(false)

  def create(value: String): IBAN = {
    val countryCode = ISO3166(value.substring(0, 2))
    IBAN(value, countryCode, schemes(countryCode))
  }
}

object IBANChecker {
  def checksumValid(code: String)
  = (BigInt(translateChars(code.drop(4) + code.take(4))) mod 97) == BigInt(1)

  /** Translate letters to numbers, also ignoring non-alphanumeric characters */
  private def translateChars(code: String) =
    code.map(cc => if (cc.isDigit) cc.toInt - '0' else cc - 'A' + 10).mkString
}
