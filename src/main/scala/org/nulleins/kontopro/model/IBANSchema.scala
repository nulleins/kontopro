package org.nulleins.kontopro.model

import com.typesafe.config._

case class IBANScheme(bank: String, branch: String, account: String) {
  val pattern = s"""^([A-Z]{2})([0-9]{2})${map(bank)}${map(branch)}${map(account)}$$""".r

  def valid(value: String) = pattern.findFirstIn(value)
  def countryCode(value: String) = value match { case pattern(cc, _*) => ISO3166(cc) }
  def bankCode(value: String) = value match { case pattern(_, _, a, _*) => a }
  def branchCode(value: String) = value match { case pattern(_, _, _, b, _) => b }
  def accountNumber(value: String) = value match { case pattern(_, _, _, _, c) => c }

  lazy val length = Seq(bank, branch, account).foldLeft(4) ( (acc, part) => {
    part match { case specPattern(n, _) => acc + n.toInt; case _ => 0 } })

  private lazy val specPattern = """(\d+)([anc])""".r
  private lazy val typeMap = Map("a" -> "A-Za-z", "n" -> "\\d", "c" -> "A-Za-z0-9")
  private def map(spec: String) = {
    spec match {
      case specPattern(n, t) => s"""([${typeMap(t)}]{$n})"""
      case _ => ""
    }
  }
}

object IBANScheme {
  import scala.collection.JavaConversions._

  private lazy val configRoot = ConfigFactory.load("iban-schemes").getObject("iban-schemes")
  private lazy val schemes = configRoot.unwrapped.toMap.map {
    case (k: String, v: java.util.Map[String, String]) =>
      ISO3166(k) -> IBANScheme(v.getOrElse("bank", "0n"), v.getOrElse("branch", "0n"), v.getOrElse("account", "0n"))
  }

  def schemeFor(countryCode: ISO3166): Option[IBANScheme] = schemes.get(countryCode)

  def valid(value: String) = (for {
    normal <- AccountNumber.normalize(value,min=2)
    scheme <- IBANScheme.schemeFor(ISO3166(normal take 2))
    ok <- scheme.valid(normal)
  } yield IBANScheme.checksumValid(ok)).getOrElse(false)

  def create(value: String): IBAN = {
    val countryCode = ISO3166(value.substring(0, 2))
    IBAN(value, countryCode, schemes(countryCode))
  }

  def generateChecksum(cc: ISO3166, bban: String) = 98 - mod97of(s"${cc}00$bban")
  def checksumValid(code: String) = mod97of(code) == BigInt(1)
  private def mod97of(code: String) = BigInt(((code drop 4) + (code take 4)).map{cc =>
    if (cc isDigit) cc.toInt - '0' else cc - 'A' + 10 }.mkString) mod 97
}
