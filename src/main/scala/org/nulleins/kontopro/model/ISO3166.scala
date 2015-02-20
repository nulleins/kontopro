package org.nulleins.kontopro.model

import java.util.Locale

/** ISO 3166 is a standard published by the International Organization for Standardization (ISO).<br/>
  * It defines codes for the names of countries, dependent territories, special areas of geographical
  * interest, and their principal subdivisions (e.g., provinces or states). The official name of the
  * standard is Codes for the representation of names of countries and their subdivisions.
  *
  * @author phillipsr */

case class ISO3166 (code: String) {
  val countryCodes = Locale.getISOCountries.toSet
  require(countryCodes.contains(code), "code not a known ISO3166 code: " + code)
  
  def getCountryName: String = new Locale("", code).getCountry
  def getAlpha3Code: String = new Locale("", code).getISO3Country
  override def toString = code
}
