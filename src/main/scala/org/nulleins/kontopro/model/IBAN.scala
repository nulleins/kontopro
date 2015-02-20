package org.nulleins.kontopro.model

/** Representation of an International Bank Account Number (IBAN)
  * <pre>
  * +-------------------------------------------------------+
  * | IBAN                                                  |
  * | +---------+----+------------------------------------+ |
  * | | ISO3166 | CS | BBAN                               | |
  * | |         |    | +------+--------+----------------+ | |
  * | |    AA   | 99 | | BANK | BRANCH | ACCOUNT NUMBER | | |
  * | |         |    | +------+--------+----------------+ | |
  * | +---------+----+------------------------------------+ |
  * +-------------------------------------------------------+</pre>
  * The IBAN consists of a ISO 3166-1 alpha-2 country code, followed by two check digits,
  * and up to thirty alpha-numeric characters for the domestic bank account number, BBAN
  * (Basic Bank Account Number), which itself may be composed of a bank code, branch code
  * and account number, as specified by the national scheme */
case class IBAN(value: String, countryCode: ISO3166, private val scheme: IBANScheme) {
  require(value != null, "IBAN string cannot be null")
  require(countryCode != null, "ISO3166 cannot be null")
  require(scheme != null, "IBANScheme cannot be null")
  require(scheme.matches(value).isDefined, s"IBAN string must be valid for scheme ($value)")

  /** @return the string representation of this IBAN, obfuscated */
  override lazy val toString = AccountNumber.obsusticate(value, 5, 2)

  /** @return the BBAN (Basic Bank Account Number) segment of this IBAN */
  lazy val bban = new BBAN(value drop 4)

  /** @return the BankCode segment of this IBAN */
  lazy val bankCode: String = scheme.bankCode(value)

  /** @return the BranchCode segment of this IBAN */
  lazy val branchCode: String =  scheme.branchCode(value)

  /** @return the AccountNumber segment of this IBAN */
  lazy val accountNumber: String = scheme.accountNumber(value)
}

object IBAN {
  /** Create an IBAN from the supplied string
    * @param value of the IBAN, may contain punctuation
    * @throws RuntimeException if the supplied value does
    *                               not represent a valid IBAN code */
  def apply(value: String): IBAN = {
    val result = for {
      iban <- AccountNumber.normalize(value,5)
      country <- Some(ISO3166(iban take 2))
      scheme <- IBANScheme.schemeFor(country)
      _ <- scheme.matches(iban)
      if IBANScheme.valid(iban)
    } yield new IBAN(iban,country,scheme)
    assert(result.isDefined, s"invalid IBAN string [$value]")
    result.get
  }
}
