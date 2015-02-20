apackage com.citibank.citift.sim.model


/** Representation of an International Bank Account Number (IBAN)
  * <p/>
  * <pre>
  * +--------------------------------------------------+
  * | IBAN                                             |
  * | +---------+------------------------------------+ |
  * | | ISO3166 | BBAN                               | |
  * | |         | +------+--------+----------------+ | |
  * | |         | | BANK | BRANCH | ACCOUNT NUMBER | | |
  * | |         | +------+--------+----------------+ | |
  * | +---------+------------------------------------+ |
  * +--------------------------------------------------+
  * </pre>
  * The IBAN consists of a ISO 3166-1 alpha-2 country code, followed by two check digits,
  * and up to thirty alphanumeric characters for the domestic bank account number, the
  * BBAN (Basic Bank Account Number), which itself may be composed of bank code, branch
  * code and account number, as specified by the national scheme */
case class IBAN(value: String, countryCode: ISO3166, private val scheme: IBANScheme) {
  require(value != null, "IBAN string cannot be null")
  require(scheme.valid(value).isDefined, s"IBAN string must be valid for scheme ($value)")

  /** @return the string representation of this IBAN, obfuscated */
  override def toString = AccountNumber.obsusticate(value, 5, 2)

  /** @return the BBAN (Basic Bank Account Number) segment of this IBAN */
  def bban = new BBAN(value.substring(4))

  /** @return the BankCode segment of this IBAN */
  def bankCode: String = scheme.getBankCode(value)

  /** @return the BranchCode segment of this IBAN */
  def branchCode: String =  scheme.getBranchCode(value)

  /** @return the AccountNumber segment of this IBAN */
  def accountNumber: String = scheme.getAccountNumber(value)

}

object IBAN {

  /** Create an IBAN from the supplied string
    *
    * @param value of the IBAN, may contain punctuation
    * @throws InvalidIBANException if the supplied value does
    *                               not represent a valid IBAN code */
  def apply(value: String): IBAN = {
    require(value != null, "IBAN string cannot be null")

    val iban = AccountNumber.sanitize(value) // strip any insignificant punctuation before processing
    require(iban.length() >= 5,"IBAN string must be at least 5 characters long")
    val countryCode = ISO3166(iban.substring(0, 2))
    val scheme = IBANScheme.lookupScheme(countryCode)

    if (!scheme.isDefined) {
      throw new InvalidIBANException(countryCode, "no scheme defined for country", value)
    }
    if (iban.length() != scheme.get.length) {
      throw new InvalidIBANException(countryCode, s"IBAN string must be ${scheme.get.length} characters in length", value)
    }
    new IBAN(value,countryCode,scheme.get)
  }

  /** Create an IBAN from its component parts */
  def apply(countryCode: ISO3166, bankCode: String, branchCode: String, accountNumber: String) =
    IBANScheme.create(countryCode, bankCode, branchCode, accountNumber)

}

class InvalidIBANException(country: ISO3166, reason: String, errorPart: String)
  extends RuntimeException(s"Invalid IBAN for $country: $reason ($errorPart)")
  }
