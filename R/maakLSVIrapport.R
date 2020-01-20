#' @title Genereert LSVI-rapport op basis van de opgegeven parameters
#'
#' @description Deze functie genereert een rapport met habitatfiches die
#' gebruikt worden voor de bepaling van de Lokale Staat van Instandhouding van
#' de habitat(sub)types die voldoen aan de opgegeven parameters.  (Om een tabel
#' te genereren met deze informatie om zelf een rapport te kunnen samenstellen,
#' wordt verwezen naar de functie geefInfoHabitatfiche().)
#'
#' @template Zoekparameters
#'
#' @param Bestandsnaam Een naam voor het html-bestand dat gegenereerd wordt,
#' bestaande uit een string die eindigt op '.html'
#' @inheritParams selecteerIndicatoren
#' @param verbose geeft de toestand van het systeem aan, om te zorgen dat
#' boodschappen niet onnodig gegeven worden
#'
#' @return Deze functie genereert habitatfiches in de vorm van html-files die
#' in de working directory opgeslagen worden.
#'
#' @examples
#' # Omwille van de iets langere lange duurtijd van de commando's staat bij
#' # onderstaande voorbeelden de vermelding 'dontrun' (om problemen te vermijden
#' # bij het testen van het package). Maar de voorbeelden werken en kunnen zeker
#' # uitgetest worden.
#' \dontrun{
#' maakConnectiePool()
#' maakLSVIrapport(
#'   Bestandsnaam = "LSVIrapport_heiden_versie3.html",
#'   Versie = "Versie 2.0", Habitatgroep = "Heiden"
#' )
#' maakLSVIrapport(
#'   Bestandsnaam = "LSVIrapport_4030.html",
#'   Habitattype = "4030"
#' )
#' library(pool)
#' poolClose(ConnectiePool)
#' }
#'
#'
#'
#' @export
#'
#' @importFrom rmarkdown render
#' @importFrom assertthat assert_that noNA is.flag
#'
#'
maakLSVIrapport <-
  function(Bestandsnaam = "LSVIrapport.html",
           Versie = "alle",
           Habitatgroep = "alle",
           Habitattype= "alle",
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
    assert_that(is.character(Bestandsnaam))
    if (!grepl(".html$", Bestandsnaam)) {
      stop("De bestandnaam moet eindigen op '.html'")
    }


    render(system.file("LSVIrapport.Rmd", package = "LSVI"),
           params = list(ConnectieLSVIhabitats = ConnectieLSVIhabitats,
                         Versie = Versie, Habitatgroep = Habitatgroep,
                         Habitattype = Habitattype),
           output_file = Bestandsnaam,
           output_dir = getwd())

    if (verbose) {
      message(
        sprintf(
          "Het rapport is opgeslagen in de working directory: %s",
          getwd()
        )
      )
    }

  }
