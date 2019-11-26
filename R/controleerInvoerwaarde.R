#' @title Hulpfunctie voor het uitvoeren van foutcontroles
#'
#' @description Deze technische hulpfunctie bevat een standaardroutine om te
#' controleren of de door een gebruiker ingevoerde waarde(n) voorkomen in een
#' gespecifieerde tabel in de databank. Indien niet, dan geeft de functie een
#' informatieve error (welke waarde is fout, en welk zijn de mogelijke
#' invoerwaarden voor de variabele?).
#'
#' @param Beschrijving Hoe de invoerwaarde beschreven moet worden in de error
#' @param Invoerwaarden De waarden die de gebruiker ingevoerd heeft
#' @inheritParams geefUniekeWaarden
#' @param Tolower default (als TRUE) wordt tolower() uitgevoerd op de
#' invoerwaarden en databankwaarden vooraleer de vergelijking uitgevoerd wordt,
#' FALSE zorgt dat dit niet uitgevoerd wordt, maar idealiter worden deze stap
#' voor stap vervangen door Tolower = TRUE
#'
#' @return Deze functie geeft geen waarde terug, maar gooit een error als er
#' een foute waarde ingevoerd is
#'
#'
#' @export
#'
#'
controleerInvoerwaarde <-
  function(
    Beschrijving, Invoerwaarden, Tabelnaam, Veldnaam, ConnectieLSVIhabitats,
    Tolower = TRUE
  ) {

    Databankwaarden <-
      geefUniekeWaarden(Tabelnaam, Veldnaam, ConnectieLSVIhabitats)
    if (Tolower) {
      Databankwaarden <- tolower(Databankwaarden)
      Invoerwaarden <- tolower(Invoerwaarden)
    }
    FouteInvoer <- unique(Invoerwaarden[!Invoerwaarden %in% Databankwaarden])
    if (length(FouteInvoer) > 0) {
      stop(
        sprintf(
          "De waarde(n) '%s' ingevoerd in %s komen niet voor in de databank. Voer hier een van volgende waarden in: %s", #nolint
          paste0(FouteInvoer, collapse = "','"),
          Beschrijving,
          paste0(Databankwaarden, collapse = ", ")
        )
      )
    }

}
