#' Invoercontrole voor waarde versie
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en
#' om de hoofdscripts overzichtelijk te houden, maken we voor elke
#' invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze
#' wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund
#' worden (maar worden wel gerund als de functie waarin ze voorkomen,
#' aangeroepen wordt).
#'
#' @param Versie Waarde waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom dplyr %>%
#' @importFrom stringr str_to_sentence
#'
#' @export
#'
invoercontroleVersie <- function(Versie, ConnectieLSVIhabitats) {
  assert_that(is.string(Versie))
  if (Versie != "RBB v1") Versie <- str_to_sentence(Versie)
  if (Versie == "Versie 2") Versie <- "Versie 2.0"
  if (Versie == "Versie 3.0") Versie <- "Versie 3"
  Versie <- ifelse(Versie == "Alle", "alle", Versie)
  controleerInvoerwaarde(
    "Versie", Versie,
    "Versie", "VersieLSVI", ConnectieLSVIhabitats, Tolower = FALSE
  )
  return(Versie)
}
