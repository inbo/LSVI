#' S4-klasse die aantal soorten met een bepaalde bedekking berekent
#' 
#' Deze klasse Aantal staat in voor de berekening van waarden voor TypeVariabele Aantal op basis van opgegeven kenmerken
#' 
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#' 
setClass(
  Class = "aantal", 
  representation =
    representation(),
  contains = "AnalyseVariabele"
)

setMethod(
  f = "berekenWaarde",
  signature = "aantal",
  definition = function(object) {
    
    return(...)
  }
)
