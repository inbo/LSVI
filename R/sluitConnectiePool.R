#' @title Sluit een eerder aangemaakte connectiepool
#'
#' @description Deze functie zorgt ervoor dat een door de functie maakConnectiePool() aangemaakte connectiepool gesloten wordt, wat betekent dat de verbinding met de databank verbroken wordt en de functies van het LSVI-package niet meer gerund kunnen worden.  Het Environment-object 'ConnectiePool' blijft wel bestaan, maar is dus inactief.  Deze functie is handig als er in een script slechts kort gebruik gemaakt wordt van het LSVI-package.
#'
#' @return Er wordt geen waarde teruggegeven, de connectie wordt gesloten.
#'
#' @examples
#' # deze functie, en dus ook onderstaande code, kan enkel gerund worden als er
#' # een connectie gelegd kan worden met de SQL Server-databank binnen INBO
#' \dontrun{
#' library(LSVI)
#' maakConnectiePool()
#' geefVersieInfo()
#' sluitConnectiePool()
#' }
#'
#' @export
#'
#' @importFrom pool poolClose
#'

sluitConnectiePool <-
  function(){
  poolClose(ConnectiePool)
}
