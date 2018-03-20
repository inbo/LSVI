#' Invoercontrole voor waarde Kwaliteitsniveau
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en om de hoofdscripts overzichtelijk te houden, maken we voor elke invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund worden (maar worden wel gerund als de functie waarin ze voorkomen, aangeroepen wordt).
#'
#' @param Kwaliteitsniveau Waarde waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that is.string
#' 
#' @export
#'
invoercontroleKwaliteitsniveau <-
  function(Kwaliteitsniveau, ConnectieLSVIhabitats) {

    Kwaliteitsniveau <- ifelse(Kwaliteitsniveau == 1, "1",
                               ifelse(Kwaliteitsniveau == 2, "2",
                                      Kwaliteitsniveau))

    assert_that(is.string(Kwaliteitsniveau))
    if (!(Kwaliteitsniveau %in%
          geefUniekeWaarden(
            "Beoordeling",
            "Kwaliteitsniveau",
            ConnectieLSVIhabitats
          )
    )) {
      stop(
        sprintf(
          "Kwaliteitsniveau moet een van de volgende waarden zijn: %s",
          geefUniekeWaarden(
            "Beoordeling",
            "Kwaliteitsniveau",
            ConnectieLSVIhabitats
          )
        )
      )
    }
  }