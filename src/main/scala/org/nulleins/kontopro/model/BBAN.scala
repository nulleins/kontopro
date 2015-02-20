package org.nulleins.kontopro.model

/** Basic Bank Account Number (BBAN)<p/>
  * The BBAN format is decided by each national banking community under the restriction that it must be of
  * a fixed length of case-insensitive alphanumeric characters. It will often include the domestic bank account number,
  * branch identifier, and potential routing information.
  * @author phillipsr */
case class BBAN(id: String) extends AccountNumber {
  val nonAlpha = """[^A-Za-z0-9]""".r
  require(id != null && id.nonEmpty, "BBAN code cannot be null/blank")
  require(!nonAlpha.findFirstIn(id).isDefined, s"BBAN must be alphanumeric, was: [$id]")

  override def toString = obsusticate(id, 2, 2)
}
