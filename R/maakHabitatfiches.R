#' @title Genereert habitatfiche(s) van LSVI op basis van de opgegeven
#' parameters
#'
#' @description Deze functie genereert habitatfiches die gebruikt worden voor
#' de bepaling van de Lokale Staat van Instandhouding van de habitattypes die
#' voldoen aan de opgegeven parameters.  (Om een tabel te genereren met deze
#' informatie om zelf een fiche te kunnen samenstellen, wordt verwezen naar de
#' functie geefInfoHabitatfiche().  Om een rapport samen te stellen met alle
#' fiches na elkaar in 1 document, wordt verwezen naar de functie
#' maakLSVIrapport())
#'
#' @template Zoekparameters
#'
#' @inheritParams selecteerIndicatoren
#' @param verbose geeft de toestand van het systeem aan, om te zorgen dat
#' boodschappen niet onnodig gegeven worden
#'
#' @return Deze functie genereert een rapport met habitatfiches in de vorm van
#' een html-file die in de working directory opgeslagen wordt.
#'
#' @examples
#' # Omwille van de iets langere lange duurtijd van het commando staat bij
#' # onderstaand voorbeeld de vermelding 'dontrun' (om problemen te vermijden
#' # bij het testen van het package). Maar het voorbeeld werkt en mag zeker
#' # uitgetest worden.
#' \dontrun{
#' maakConnectiePool()
#' maakHabitatfiches(Versie = "Versie 2.0", Habitattype = "4030")
#' library(pool)
#' poolClose(ConnectiePool)
#' }
#'
#' @export
#'
#' @importFrom rmarkdown render
#' @importFrom assertthat assert_that noNA is.flag
#'
#'
maakHabitatfiches <-
  function(Versie = "alle",
           Habitatgroep = "alle",
           Habitattype = "alle",
           ConnectieLSVIhabitats = NULL,
           verbose = TRUE) {

    if (is.null(ConnectieLSVIhabitats)) {
      if (exists("ConnectiePool")) {
        ConnectieLSVIhabitats <- get("ConnectiePool", envir = .GlobalEnv)
      }
    }
    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren. Maak een connectiepool met maakConnectiePool of geef een connectie mee met de parameter ConnectieLSVIhabitats." #nolint
    )
    assert_that(is.flag(verbose))
    assert_that(noNA(verbose))

    Indicatoren <-
      selecteerIndicatoren(
        Versie = Versie,
        Habitatgroep = Habitatgroep,
        Habitattype = Habitattype,
        HabitatnamenToevoegen = TRUE,
        ConnectieLSVIhabitats = ConnectieLSVIhabitats)

    for (versie in unique(Indicatoren$Versie)) {
      for (habitatsubtype in unique(as.character(Indicatoren$Habitatsubtype))) {
        Bestandnaam <-
          sprintf(
            "Habitatfiche_%s_%s.html",
            habitatsubtype,
            sub(versie, pattern = " ", replacement = "")
          )
        render(
          system.file("HabitatficheParent.Rmd", package = "LSVI"),
          params =
            list(
              ConnectieLSVIhabitats = ConnectieLSVIhabitats,
              Versie = versie,
              Habitatsubtype = habitatsubtype,
              Habitatnaam =
                unique(
                  Indicatoren[
                    Indicatoren$Habitatsubtype == habitatsubtype,
                    "Habitatsubtypenaam"
                  ]
                )
            ),
          output_file = Bestandnaam,
          output_dir = getwd()
        )
      }
    }
    if (verbose) {
      message(
        sprintf(
          "De fiche(s) is/zijn opgeslagen in de working directory: %s",
          getwd()
        )
      )
    }

  }
