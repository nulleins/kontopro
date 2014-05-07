package org.nulleins.kontomat

import java.util.{Currency, UUID}

/**
 * Module context services
 * Services defined in here are implicitly available to the module, such as aggregates, domain and application services
 * While this makes set-up a lot simpler (and therefore less error-prone), please do resist the temptation to introduce
 * state (global data) into this object, ok?
 * 
 * @author phillipsr@gmail.com
 *
 */


package object model
{
  type Identity = String
  type AccountId = Identity
  type AccountNumber = Identity
  type ProductId = Identity
  type PartyId = Identity
}