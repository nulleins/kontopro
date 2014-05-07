package org.nulleins.kontomat.model

/**
 * @author phillipsr@gmail.com
 *
 */
case class ErrorEvent ( 
    message: String,
    aggregateId: Option[Identity] = None,
    exception: Option[Throwable] = None)
  extends DomainEvent {

  override def toString = {
    val id = aggregateId match {
      case Some(guid) => ",id=" + guid
      case _ => "" }
    val ex = exception match {
      case Some(ex) => ",ex=" + ex
      case _ => "" }
    f"ErrorEvent: '$message'$id$ex"
  }
}

object ErrorEvent {
    def warning ( message: String, aggregateId: Option[Identity] = None) =
      new ErrorEvent ( message, aggregateId)
    def error ( message: String = "", aggregateId: Option[Identity] = None, exception: Throwable = null) =
      new ErrorEvent ( message, aggregateId, Option(exception))
}