import com.citibank.citift.sim.model._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class IBANTest extends FunSuite {

  val testIbanCz = "CZ1955000000001041041022"
  val testIbanPl = "PL19114011240000540026001002"
  val testChecksum = "GB82WEST12345698765432"

  def blackCases = """AT611904300235473201
                     |GB82TEST12345698765432
                     |GB81WEST12345698765432""".stripMargin

  def whiteCases = """AD1200012030200359100100
                     |AE26 0211 0000 0023 0064 016
                     |AL47 2121 1009 0000 0002 3569 8741
                     |AO06000600000100037131174
                     |AZ21NABZ00000000137010001944
                     |BA391290079401028494
                     |BE68539007547034
                     |BF1030134020015400945000643
                     |BG80BNBG96611020345678
                     |BH29BMAG1299123456BH00
                     |BI43201011067444
                     |BJ11B00610100400271101192591
                     |BR9700360305000010009795493P1
                     |CG5230011000202151234567890
                     |CH9300762011623852957
                     |CI05A00060174100178530011852
                     |CM2110003001000500000605306
                     |CR0515202001026284066
                     |CV64000300004547069110176
                     |CY17002001280000001200527600
                     |CZ6508000000192000145399
                     |DE89370400440532013000
                     |DK5000400440116243
                     |DO28BAGR00000001212453611324
                     |DZ4000400174401001050486
                     |EE382200221020145685
                     |EG1100006001880800100014553
                     |ES9121000418450200051332
                     |FI2112345600000785
                     |FO1464600009692713
                     |FR1420041010050500013M02606
                     |FR7630007000110009970004942
                     |GA2140002000055602673300064
                     |GB29NWBK60161331926819
                     |GE29NB0000000101904917
                     |GI75NWBK000000007099453
                     |GL8964710001000206
                     |GR1601101250000000012300695
                     |GT82TRAJ01020000001210029690
                     |HR1210010051863000160
                     |HU42117730161111101800000000
                     |IE29AIBK93115212345678
                     |IL620108000000099999999
                     |IR580540105180021273113007
                     |IS140159260076545510730339
                     |IT60X0542811101000000123456
                     |JO94CBJO0010000000000131000302
                     |KW74NBOK0000000000001000372151
                     |KZ176010251000042993
                     |LB30099900000001001925579115
                     |LI21088100002324013AA
                     |LT121000011101001000
                     |LU280019400644750000
                     |LV80BANK0000435195001
                     |MC5813488000010051108001292
                     |MD24AG000225100013104168
                     |ME25505000012345678951
                     |MG4600005030010101914016056
                     |MK07300000000042425
                     |ML03D00890170001002120000447
                     |MR1300012000010000002037372
                     |MT84MALT011000012345MTLCAST001S
                     |MU17BOMM0101101030300200000MUR
                     |MZ59000100000011834194157
                     |NL91ABNA0417164300
                     |NL81 TRIO 0212 4710 66
                     |NO9386011117947
                     |PK24SCBL0000001171495101
                     |PL27114020040000300201355387
                     |PS92PALS000000000400123456702
                     |PT50000200000163099310355
                     |PT50000201231234567890154
                     |QA58 DOHB 0000 1234 5678 90AB CDEF G
                     |RO49 AAAA 1B31 0075 9384 0000
                     |RS35260005601001611379
                     |SA0380000000608010167519
                     |SE3550000000054910000003
                     |SI56191000000123438
                     |SK3112000000198742637541
                     |SM86U0322509800000000270100
                     |SN12K00100152000025690007542
                     |TN5914207207100707129648
                     |TR330006100519786457841326
                     |UA57 3543 4700 0676 2462 0549 2502 6
                     |VG96 VPVG 0000 0123 4567 8901
                     |GB82 WEST 1234 5698 7654 32
                     |SA03 8000 0000 6080 1016 7519
                     |CH93 0076 2011 6238 5295 7""".stripMargin

  test("can read schema config") {
    val ieIbanScheme = IBANScheme.schemeFor(ISO3166("IE"))
    assert(ieIbanScheme.isSuccess)
    val scheme = ieIbanScheme.toList.head
    assert(scheme.length === 22)

    val testIbanIe = "IE64IRCE92050112345678"
    assert(scheme.countryCode(testIbanIe) === ISO3166("IE"))
    assert(scheme.bankCode(testIbanIe) === "IRCE")
    assert(scheme.branchCode(testIbanIe) === "920501")
    assert(scheme.accountNumber(testIbanIe) == "12345678")
    assert(scheme.matches(testIbanIe).isSuccess)
  }

  test("can create IBAN from string") {
    val iban = IBAN(testIbanCz)
    assert(iban.toString === "CZ195*****************22")
    assert(iban.value === "CZ1955000000001041041022")
    assert(iban.bban.id === "55000000001041041022")
    assert(iban.formatted == "CZ19 5500 0000 0010 4104 1022")
  }

  test("valid ibans") {
    whiteCases.lines foreach(iban => assert(IBANScheme.valid(iban)))
  }

  test("invalid ibans") {
    blackCases.lines foreach(iban => assert(!IBANScheme.valid(iban)))
  }

  test("validate iban: positive") {
    assert(IBANScheme.valid("IE64IRCE92050112345678"))
  }

  test("validate iban: negative") {
    assert(!IBANScheme.valid("PLAA999999999999999999999999"))
  }

  test("validate checksum: positive") {
    assert(IBANScheme.checksumValid(testChecksum))
  }

  test("validate checksum: negative") {
    assert(!IBANScheme.checksumValid("PLAA999999999999999999999999"))
  }

  test("checksum generation") {
    val bban = "WEST12345698765432"
    assert(IBANScheme.generateChecksum(ISO3166("GB"),bban) === 82)
  }

  test("bad construction: null supplied") {
    val thrown = intercept[RuntimeException] {
      val iban = IBAN(null)
    }
    assert(thrown.getMessage === "Code may not be null")
  }

  test("bad construction: string too short") {
    val thrown = intercept[RuntimeException] {
      val iban = IBAN("IE23")
    }
    assert(thrown.getMessage === """Length of "IE23" less than 5""")
  }

  test("bad construction: unregistered schema (Kyrgyzstan)") {
    val thrown = intercept[RuntimeException] {
      val iban = IBAN("KG580540105180021273113007")
    }
    assert(thrown.getMessage === "Scheme not registered for KG")
  }

  test("bad construction: pattern match failure") {
    val thrown = intercept[RuntimeException] {
      val iban = IBAN("PLAA999999999999999999999999")
    }
    assert(thrown.getMessage === "PLAA999999999999999999999999 does not match pattern")
  }

  test("bad construction: incorrect-checksum") {
    val thrown = intercept[RuntimeException] {
      val iban = IBAN("CH9300762011623852999")
    }
    assert(thrown.getMessage === "Invalid checksum")
  }

}
