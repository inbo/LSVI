#' Invoercontrole voor dataframe Data_habitat
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en
#' om de hoofdscripts overzichtelijk te houden, maken we voor elke
#' invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze
#' wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund
#' worden (maar worden wel gerund als de functie waarin ze voorkomen,
#' aangeroepen wordt).
#'
#' @param Data_habitat dataframe waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom stringr str_replace
#'
#' @export
#'
invoercontroleData_habitat <- function(Data_habitat, ConnectieLSVIhabitats) { #nolint
  assert_that(inherits(Data_habitat, "data.frame"))
  assert_that(has_name(Data_habitat, "ID"))
  if (!is.character(Data_habitat$ID)) {
    Data_habitat$ID <- as.character(Data_habitat$ID)
  }
  assert_that(has_name(Data_habitat, "Habitattype"))
  if (!is.character(Data_habitat$Habitattype)) {
    Data_habitat$Habitattype <- as.character(Data_habitat$Habitattype)
  }
  Data_habitat$Habitattype <- tolower(Data_habitat$Habitattype)
  Data_habitat$Habitattype <-
    str_replace(Data_habitat$Habitattype, "91e0", "91E0")
  Data_habitat$Habitattype <-
    str_replace(Data_habitat$Habitattype, "91f0", "91F0")
  controleerInvoerwaarde(
    "Data_habitat$Habitattype", Data_habitat$Habitattype,
    "Habitattype", "Code", ConnectieLSVIhabitats, Tolower = FALSE
  )
  return(Data_habitat)
}
