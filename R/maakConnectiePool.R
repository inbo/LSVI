#' @title Maak een connectiepool de databank met LSVI-indicatoren
#'
#' @description Deze functie maakt een connectiepool met de
#' LSVI-indicatorendatabank, wat nodig is om de functies te kunnen gebruiken.
#' Deze connectiepool moet eenmalig aangemaakt worden, en functies zullen
#' automatisch deze connectiepool gebruiken om te connecteren met de databank.
#' Alternatief is om een connectie aan te maken met de functie
#' connecteerMetLSVIdb() en deze bij elke functie mee te geven.  Voorlopig
#' verwijst deze connectiepool naar een databank binnen INBO, waardoor
#' gebruikers buiten INBO een kopie van de databank nodig hebben om met dit
#' package te kunnen werken, en eventueel een aangepaste versie van deze
#' functie.  Op termijn zal deze databank toegevoegd worden aan het package,
#' waardoor ze overal zou moeten werken.
#'
#' @inheritParams connecteerMetLSVIdb
#' @return Deze functie maakt een Environment-object aan dat de connecties
#' regelt met de betreffende databank.
#'
#' @examples
#' # deze functie, en dus ook onderstaande code, kan enkel gerund worden als er
#' # een connectie gelegd kan worden met de SQL Server-databank binnen INBO
#' \dontrun{
#' library(LSVI)
#' maakConnectiePool()
#' geefVersieInfo()
#' library(pool)
#' poolClose(ConnectiePool)
#' }
#'
#' @export
#'
#' @importFrom pool dbPool
#' @importFrom odbc odbc
#'

maakConnectiePool <-
  function(Server = "INBO-SQL07-PRD.inbo.be",
           Databank = "D0122_00_LSVIHabitatTypes",
           Gebruiker = "pc-eigenaar",
           Wachtwoord = "geen") {
  assert_that(is.string(Server))
  assert_that(is.string(Databank))
  assert_that(is.string(Gebruiker))
  assert_that(is.string(Wachtwoord))

  if (Gebruiker == "pc-eigenaar") {
    tryCatch(
      assign(
        "ConnectiePool",
        dbPool(
          drv = odbc(),
          Driver = "SQL Server",
          Database = Databank,
          Server = Server,
          Trusted_Connection = "TRUE"
        ),
        envir = .GlobalEnv
      ),
      error = function(e) {
        warning("Het lukt niet om een connectie te leggen naar de SQL Server-databank op INBO.  Neem contact op met de beheerder van het package als dit probleem zich blijft voordoen.")  #nolint
        maakConnectiepoolSQLite()
      }
    )
  } else {
    warning("De procedure om een connectiepool te genereren met de LSVI-indicatoren op de server van INBO, is enkel voorzien op basis van het pc-aanmeldingssysteem binnen INBO.  Neem contact op met de beheerder van het package om eventuele andere opties te bekijken.")  #nolint
  }
}
