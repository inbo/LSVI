#' @title Bij laden een pool van connecties openen
#' 
#' @description Deze functie opent een pool van connecties naar de indicatorendatabank LSVI habitattypes die meegegeven wordt als de gebruiker zelf geen databank opgeeft.
#' 
#' @importFrom pool dbPool
#' @importFrom odbc odbc
#' @importFrom RSQLite SQLite
#' 
.onLoad <- function(libname, pkgname) {
  tryCatch(
    ConnectiePool <<-
      dbPool(
        drv = odbc(),
        Driver = "SQL Server",
        Database = "D0122_00_LSVIHabitatTypes",
        Server = "INBO-SQL07-PRD.inbo.be",
        Trusted_Connection = "TRUE"
      ),
    error = function(e) {
      warning("Er kan geen connectie gelegd worden met de databank.  Voorzie bij elke functie zelf een link naar een databank met indicatoren voor elk habitattype.") #nolint
      tryCatch(
        ConnectiePool <<-
          dbPool(
            drv = SQLite(),
            dbname =
              system.file("data/LSVIHabitatTypes.sqlite", package = "LSVI"),
            encoding = "UTF-8"
          ),
        error = function(e) {
          warning("Er gaat iets mis met de connectie naar de databank in het package.  Neem contact op met de beheerder van het package als dit probleem zich blijft voordoen.")  #nolint
        }
      )
    }
  )
}

#' @title Bij afsluiten de pool van connecties terug sluiten
#' 
#' @description Deze functie sluit de pool van connecties terug die bij onLoad geopend werd.
#'  
#' @importFrom pool poolClose
#' 
.onUnLoad <- function(libpath) {
  poolClose(ConnectiePool)
}
