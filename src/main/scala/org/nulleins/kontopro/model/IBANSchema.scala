package org.nulleins.kontopro.model

import com.typesafe.config._
import scalaz.{Success, Failure, Validation}
import scalaz.syntax.std.option._

case class IBANScheme(countryCode: ISO3166, bankPic: String, branchPic: String, accountPic: String) {
  val pattern = s"^([A-Z]{2})([0-9]{2})${map(bankPic)}${map(branchPic)}${map(accountPic)}$$".r

  def matches(value: String) = pattern.findFirstIn(value).toSuccess(s"$value does not match pattern")
  def countryCode(value: String) = ISO3166(value take 2)
  def bankCode(value: String) = value match { case pattern(_, _, a, _*) => a }
  def branchCode(value: String) = value match { case pattern(_, _, _, b, _) => b }
  def accountNumber(value: String) = value match { case pattern(_, _, _, _, c) => c }

  lazy val length = Seq(bankPic, branchPic, accountPic).foldLeft(4)((acc, part) => {
    part match { case specPattern(n, _) => acc + n.toInt; case _ => 0} })

  private lazy val specPattern = "(\\d+)([anc])".r
  private lazy val typeMap = Map("a" -> "A-Za-z", "n" -> "\\d", "c" -> "A-Za-z\\d")
  private def map(spec: String) = {
    spec match { case specPattern(n, t) => s"([${typeMap(t)}]{$n})"; case _ => ""}}
}

object IBANScheme extends AccountNumber {
  import scala.collection.JavaConversions._

  /* load country IBAN scheme definitions from configuration file on class path */
  private lazy val configRoot = ConfigFactory.load("iban-schemes").getObject("iban-schemes")
  private lazy val schemes = configRoot.unwrapped.toMap.map {
    case (k: String, v: java.util.Map[String, String]) => val key = ISO3166(k)
      key -> IBANScheme(key, v.getOrElse("bank", "0n"), v.getOrElse("branch", "0n"), v.getOrElse("account", "0n"))
  }
  def schemeFor(country: ISO3166) = schemes.get(country).toSuccess(s"Scheme not registered for $country")

  /** @return `Success` wrapping the normalized input string, if: it has a scheme defined, it
    *         matches the scheme pattern and the checksum is valid; else a `Failure` wrapping
    *         the failure message detailing the (first) reason it was not considered valid */
  def parse(value: String): Validation[String,String] = for {
    normal <- normalize(value,5)
    scheme <- schemeFor(ISO3166(normal take 2))
    result <- scheme.matches(normal)
    _ <- validateChecksum(normal)
  } yield result

  def valid(value: String) = parse(value).isSuccess

  def create(value: String): IBAN = IBAN(value, schemes(ISO3166(value take 2)))

  def validateChecksum(code: String) = if ( checksumValid(code)) Success(code) else Failure("Invalid checksum")
  def generateChecksum(cc: ISO3166, bban: String) = 98 - mod97of(s"${cc}00$bban")
  def checksumValid(code: String) = mod97of(code) == BigInt(1)
  private def mod97of(code: String) = BigInt(((code drop 4) + (code take 4)).map { cc =>
    if (cc isDigit) cc.toInt - '0' else cc - 'A' + 10 }.mkString) mod 97
}
