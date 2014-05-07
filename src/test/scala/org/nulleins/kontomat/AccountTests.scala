package org.nulleins.kontomat

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.nulleins.kontomat.model._
import org.joda.money.{Money, CurrencyUnit}

@RunWith(classOf[JUnitRunner])
class AccountTests extends FunSuite {

  test("account is created") {
    val createEvent = AccountCreated ( "123456", "IE29AIBK93115212345678", "CURRAC", "CUST001", CurrencyUnit.EUR)
    val openAccount = Account("123456", List(createEvent))
    println(openAccount)

    val credited = openAccount.credit(Money.of(CurrencyUnit.EUR, 100))
    println(credited)
  }
}