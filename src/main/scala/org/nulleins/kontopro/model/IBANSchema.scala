package com.citibank.citift.sim.model

import java.math.BigInteger

import com.typesafe.config._
import oracle.net.aso.r

case class IBANScheme(bank: String, branch: String, account: String) {
  val pattern = s"""([A-Z]{2})([0-9]{2})(${map(bank)})(${map(branch)})(${map(account)})""".r
  def valid(value: String) = pattern.findFirstIn(value)
  def getCountryCode(value: String) = value match { case pattern(cc, _*) => ISO3166(cc) }
  def getBankCode(value: String) = value match { case pattern(_, _, a, _*) => a }
  def getBranchCode(value: String) = value match { case pattern(_, _, _, b, _) => b }
  def getAccountNumber(value: String) = value match { case pattern(_, _, _, _, c) => c }

  val length = Seq(bank, branch, account).foldLeft(4) { (a, s) => {
    s match {
      case specPattern(n, _) => a + n.toInt
      case _ => 0
    }
  }
  }

  override def toString = s"IBANScheme(len=$length, pat=$pattern)"

  private lazy val specPattern = """(\d+)([anc])""".r
  private lazy val typeMap = Map("a" -> "A-Za-z", "n" -> "\\d", "c" -> "A-Za-z0-9")
  private def map(spec: String) = { spec match {
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
      ISO3166(k) -> IBANScheme(
        v.getOrElse("bank", ""),
        v.getOrElse("branch", ""),
        v.getOrElse("account", ""))
  }

  def lookupScheme(countryCode: ISO3166): Option[IBANScheme] = schemes.get(countryCode)
  def lookupScheme(value: String): Option[IBANScheme] = lookupScheme(ISO3166(value.take(2)))

  def valid(value: String) = (for {
      scheme <- IBANScheme.lookupScheme(AccountNumber.sanitize(value))
      ok <- scheme.valid(value)
    } yield IBANChecker.checksumValid(ok)).getOrElse(false)

  def create(value: String, countryCode: ISO3166): IBAN = {
    require(schemes.contains(countryCode))
    IBAN(value,countryCode,schemes(countryCode))
  }

  def create(countryCode: ISO3166, bankCode: String, branchCode: String, accountNumber: String): IBAN =
    IBANScheme.create(bankCode+branchCode+accountNumber,countryCode)

  def create(value: String): IBAN = {
    require(value.length >= 5)
    IBANScheme.create(value,ISO3166(value.substring(0,2)))
  }

}

object IBANChecker {
  private val BI_97 = new BigInteger ( "97")
  private val BI_98 = BI_97.add(BigInteger.ONE)

  def checksumValid(code: String)
    = (BigInt(translateChars ( code.drop(4) + code.take(4))) mod 97) == BigInt(1)

  /** Translate letters to numbers, also ignoring non-alphanumeric characters */
  private def translateChars ( bban: String) =
    bban.map(cc => if (cc.isDigit) cc.toInt - '0' else cc - 'A' + 10).mkString

}
