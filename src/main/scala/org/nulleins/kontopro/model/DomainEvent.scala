package org.nulleins.kontopro.model

import java.util.{UUID, Calendar}

/**
 * @author phillipsr@gmail.com
 *
 */
trait DomainEvent extends Serializable
{
  val version = "1.0.0"
  val name: String = DomainEvent.this.getClass.getSimpleName
  val id = UUID.randomUUID().toString
  val timestamp = Calendar.getInstance.getTimeInMillis    
}