#' S4-klasse voor voorwaarden die enkel rechtstreeks op het terrein gemeten kunnen worden of afgeleid worden uit GIS-data
#'
#' Deze klasse meting geeft een waarde NA.  Ze is een nakomeling van de klasse AnalyseVariabele.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod
#'
#' @include s4_AnalyseVariabele.R
setClass(
  Class = "meting",
  representation =
    representation(),
  contains = "AnalyseVariabele"
)

setMethod(
  f = "berekenWaarde",
  signature = "meting",
  definition = function(object) {

    warning(paste("De waarde voor de voorwaarde met ID",
                  object@VoorwaardeID,
                  "kan niet berekend worden. Geef de waarde voor deze voorwaarde rechtstreeks in als input van de functie 'berekenLSVIBasis' via 'Data_voorwaarden'")) #nolint

    return(c(NA, NA))
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "meting",
  definition = function(object) {
    return(NA)
  }
)
