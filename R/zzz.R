#' @title Bij laden een pool van connecties openen
#' 
#' @description Deze functie opent een pool van connecties naar de indicatorendatabank LSVI habitattypes die meegegeven wordt als de gebruiker zelf geen databank opgeeft.
#' 
#' @importFrom pool dbPool
#' @importFrom odbc odbc
#' 
.onLoad <- function(libname, pkgname) {
  ConnectiePool <<-
    dbPool(
      drv = odbc(),
      Driver = "SQL Server",
      Database = "D0122_00_LSVIHabitatTypes",
      Server = "INBO-SQL07-PRD.inbo.be",
      Trusted_Connection = "TRUE"
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

