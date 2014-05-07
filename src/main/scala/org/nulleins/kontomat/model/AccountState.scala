package org.nulleins.kontomat.model

/**
 * @author phillipsr@gmail.com
 *
 */

abstract sealed class AccountState

case object Created extends AccountState
case object Open extends AccountState
case object Suspended extends AccountState
case object Closed extends AccountState
case object Deleted extends AccountState