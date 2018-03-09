#' Invoercontrole voor waarde versie
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en om de hoofdscripts overzichtelijk te houden, maken we voor elke invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund worden (maar worden wel gerund als de functie waarin ze voorkomen, aangeroepen wordt).
#'
#' @param Versie Waarde waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that is.string
#' @importFrom dplyr %>%
#' 
#' @export
#'
invoercontroleVersie <- function(Versie, ConnectieLSVIhabitats) {
  assert_that(is.string(Versie))
  if (
    !(Versie %in%
      geefUniekeWaarden("Versie", "VersieLSVI", ConnectieLSVIhabitats)
    )
  ) {
    stop(
      sprintf(
        "Versie moet een van de volgende waarden zijn: %s",
        geefUniekeWaarden("Versie", "VersieLSVI", ConnectieLSVIhabitats)
      )
    )
  }
}
