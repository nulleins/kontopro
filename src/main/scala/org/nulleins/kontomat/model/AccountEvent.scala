package org.nulleins.kontomat.model

import org.joda.money.{CurrencyUnit, Money}

/** What do we know about accounts?
  * They have a type (e.g., eWallet), they can be internal or external, they have an owner,
  * they probably have an account currency<p/>
  * They are 'instances' of an Account product, that may supply some of the processing rules,
  * such as limits, but some limits will be instance-specific Accounts have a life-cycle, from
  * created, thru open, suspended, resumed, closed, deleted Accounts can have detail updated
  * (perhaps limits changed)<p/>
  * Transfers would seem to be the main business activity involving accounts: value is transferred and
  * and out, and these transactions would be the main usage scenarios
  * @author phillipsr@gmail.com */
sealed abstract class AccountEvent(val accountId: AccountId) extends DomainEvent

// Account life-cycle events
case class AccountCreated ( 
    override val accountId: AccountId,
    accountNumber: AccountNumber,
    productId: ProductId,
    holderId: PartyId,
    currency: CurrencyUnit)
  extends AccountEvent(accountId)

case class AccountSuspended ( override val accountId: AccountId, reason: String)
  extends AccountEvent(accountId)

case class AccountOpened ( override val accountId: AccountId)
  extends AccountEvent(accountId)

case class AccountClosed ( override val accountId: AccountId, reason: String)
  extends AccountEvent(accountId)

case class AccountDeleted ( override val accountId: AccountId, reason: String)
  extends AccountEvent(accountId)

// payments, etc.
case class AccountDebit ( override val accountId: AccountId, amount: Money)
  extends AccountEvent(accountId)

case class AccountCredit ( override val accountId: AccountId, amount: Money)
  extends AccountEvent(accountId)

