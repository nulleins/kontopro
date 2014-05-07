package org.nulleins.kontopro.model

import java.util.Currency

import scalaz._
import org.joda.money.{CurrencyUnit, Money}

/**
 * Aggregate definition for account
 * <p/>
 * Responsibilities include representation of the current state of an account,
 * implementing business rules/logic on receipt of change events, publishing
 * any state changes to interested parties (pub/sub) and recreating itself 
 * from a sequence of historical events (projection)
 * 
 * What do we know about accounts?
 * They have a type (e.g., eWallet), they can be internal or external, they have an owner,
 * they probably have an account currency
 * They are 'instances' of an Account product, that may supply some of the processing rules,
 * such as limits, but some limits will be instance-specific
 * Accounts have a life-cycle, from created, thru open, suspended, resumed, closed, deleted
 * Accounts can have detail updated (perhaps limits changed)
 * Transfers would seem to be the main business activity involving accounts: value is transfered and
 * and out, and these transactions would be the main usage scenarios
 * 
 * @author phillipsr@gmail.com
 *
 */
sealed class Account ( val id: AccountId) {
  /** @return a state change event to open this account,
   		or an error event, if requested change is not valid */
  def openAccount = Seq ( this match {
    case acc: OpenAccount => ErrorEvent.warning ( "Cannot open account if already Open", Some(id))
    case acc: DeletedAccount => ErrorEvent.warning ("Cannot open a deleted account", Some(id))
    case _ => AccountOpened(id)
  })
  
  /** @return a state change event to close this account,
   		or an error event, if requested change is not valid */
  def closeAccount(reason: String) = Seq ( this match {
    case acc: ClosedAccount => ErrorEvent.warning ( "Cannot close account if already Closed", Some(id))
    case _ => AccountClosed(id,reason)
  })

  /** @return a state change event to suspend this account,
   		or an error event, if requested change is not valid */
  def suspendAccount(reason: String) = Seq ( this match {
    case acc: OpenAccount => AccountSuspended(id, reason)
    case _ => ErrorEvent.warning ( "Cannot suspend account if not Open", Some(id))
  })
  
  /** @return a state change event to delete this account,
   		or an error event, if requested change is not valid */
  def deleteAccount(reason: String) = Seq ( this match {
    case acc: ClosedAccount => acc.deleteAccountIfZero(reason)
    case _ => ErrorEvent.warning ( "Cannot delete account if not Closed", Some(id))
  })

  /** @return a state change event to delete this account,
   		or an error event, if requested change is not valid */
  def credit(amount: Money) = Seq ( this match {
    case acc: OpenAccount => acc.creditAccount(amount)
    case _ => ErrorEvent.warning ( "Cannot credit account if not Open", Some(id))
  })

  override def toString = s"Account($id)"
}

case class OpenAccount (  
    override val id: AccountId,
    accountNumber: AccountNumber,
    productId: ProductId,
    holderId: PartyId,
    currency: CurrencyUnit,
    balance: Money)
  extends Account ( id)
{
  /** @return Successful validation if the amount specified is in the same currency as
      	this account,otherwise, return a failure wrapping an appropriate error event */
  private[this] def assertCorrectCurrency(amount: Money) =
    amount.getCurrencyUnit match {
      case `currency` => Success()
      case _ => Failure(ErrorEvent.error (
          f"Transaction currency (${amount.getCurrencyUnit}) not account currency ($currency)", Some(id)))
    }     
  
  /** @return a debit event for this account, if the business rules allow */
  def debitAccount ( amount: Money) = Seq ((
    for {
      _ <- assertCorrectCurrency ( amount) 
      event <- Success(AccountDebit ( id, amount)) 
    } yield event) match {
      case Success(debitEvent) => debitEvent
      case Failure(errorEvent) => errorEvent})

  /** @return a credit event for this account, if the business rules allow */  
  def creditAccount ( amount: Money) = Seq ((
    for {
      _ <- assertCorrectCurrency ( amount) 
      event <- Success(AccountCredit ( id, amount)) 
    } yield event) match {
      case Success(creditEvent) => creditEvent
      case Failure(errorEvent) => errorEvent})
  
  /** @return a copy of this account object, with the balance reduced by `amount` */
  protected[model] def subBalance ( amount: Money) = copy(balance=balance.minus(amount))
  
  /** @return a copy of this account object, with the balance increased by `amount` */
  protected[model] def addBalance ( amount: Money) = copy(balance=balance.plus(amount))
}

case class ClosedAccount (
    override val id: AccountId,
    accountNumber: AccountNumber,
    productId: ProductId,
    holderId: PartyId,
    currency: CurrencyUnit,
    balance: Money,
    reason: String)
  extends Account ( id)
{
  /** @return an AccountDeleted event if the balance is zero, otherwise, an ErrorEvent */
  protected[model] def deleteAccountIfZero(reason: String) = balance.isZero match {
    case true => AccountDeleted(id,reason)
    case false => ErrorEvent.warning ( "Cannot delete account if balance not zero", Some(id))
  }  
}

case class DeletedAccount (  
    override val id: AccountId,
    accountNumber: AccountNumber,
    productId: ProductId,
    holderId: PartyId,
    currency: CurrencyUnit,
    reason: String) extends Account ( id)

object Account
{
  /**
   * Apply a list of domain events to the domain object, yielding the current state of that
   * object (projected); this method will sort the supplied event sequence to ensure the newest
   * events are applied last
   * @return a Account whose state reflects the events received
   */
  def apply ( id: AccountId, events: Seq[AccountEvent]) =
    project ( id, events sortWith ( (ev1, ev2) => ev1.timestamp < ev2.timestamp))

  /**
   * Apply a list of domain events to the domain object, yielding the current state of that
   * object (projected); the events will be applied in the order supplied (either already sorted
   * or use the `apply` to ensure correct event sequencing)
   * @return a Account whose state reflects the events received
   */  
  def project ( accountId: AccountId, orderedEvents: Seq[AccountEvent]) =
    orderedEvents.foldLeft(new Account(id=accountId))((result,item) => applyEvent(result,item))

  /** Factory that will @return a new Account (sub-class) that results from applying the
   *  supplied `event' to `account'
   *  @throws AssertionException if the supplied event does not refer to the supplied account */
  private[this] def applyEvent ( account: Account, event: AccountEvent): Account = {
    require ( event.accountId == account.id, "event applies to supplied account")
    event match {
      case AccountCreated(accountId,anAccNumber,aProductId,aHolderId,aCurrency) => 
        new OpenAccount ( accountId, anAccNumber, aProductId, aHolderId, aCurrency, Money.of(aCurrency,0))
      
      case openEvent: AccountOpened => account match {
         case ClosedAccount(accountId,anAccountNumber,aProductId,aHolderId,aCurrency,aBalance,_) => 
           new OpenAccount ( accountId, anAccountNumber, aProductId, aHolderId, aCurrency, aBalance)
         case _ => throw new IllegalStateException ( "Attempt to open non-Closed account")
      }      
      case debitEvent: AccountDebit => account match {
         case openAcc: OpenAccount => openAcc.subBalance(debitEvent.amount)
         case _ => throw new IllegalStateException ( "Attempt to debit non-Open account")
      }
      case creditEvent: AccountCredit => account match {  
         case openAcc: OpenAccount => openAcc.addBalance(creditEvent.amount)
         case _ => throw new IllegalStateException ( "Attempt to credit non-Open account")
      }
      case suspendEvent: AccountSuspended => account match {
         case OpenAccount(accountId,anAccountNumber,aProductId,aHolderId,aCurrency,aBalance) => 
           new ClosedAccount ( accountId, anAccountNumber, aProductId, aHolderId, aCurrency, aBalance, suspendEvent.reason)
         case _ => throw new IllegalStateException ( "Attempt to suspend non-Open account")
      }
      case closeEvent: AccountClosed => account match {
         case OpenAccount(accountId,anAccountNumber,aProductId,aHolderId,aCurrency,aBalance) => 
           new ClosedAccount ( accountId, anAccountNumber, aProductId, aHolderId, aCurrency, aBalance, closeEvent.reason)
         case _ => throw new IllegalStateException ( "Attempt to close non-Open account")
      }
      case deleteEvent: AccountDeleted => account match {
         case OpenAccount(accountId,anAccountNumber,aProductId,aHolderId,aCurrency,aBalance) => 
           require ( aBalance.isZero, "Account balance is zero on deletion")
           new DeletedAccount ( accountId, anAccountNumber, aProductId, aHolderId, aCurrency, deleteEvent.reason)
         case _ => throw new IllegalStateException ( "Attempt to delete non-Open account")
      }
    }
  }
}