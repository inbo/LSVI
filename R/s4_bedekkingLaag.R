#' S4-klasse die de totale bedekking van een vegetatielaag berekent
#'
#' Deze klasse bedekkingLaag staat in voor de berekening van waarden voor
#' AnalyseVariabele bedekkingLaag op basis van opgegeven kenmerken.  Ze is een
#' nakomeling van de klasse bedekking.  Ze maakt de berekening op basis van de
#' studiegroep als deze aanwezig is in de opname, en anders op basis van de
#' soortengroep.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden
#' Vegetatielaag, Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod as
#' @importFrom dplyr %>% filter
#'
#' @noRd
#'
#' @include s4_bedekking.R
setClass(
  Class = "bedekkingLaag",
  representation =
    representation(),
  contains = "bedekking"
)

setMethod(
  f = "berekenWaarde",
  signature = "bedekkingLaag",
  definition = function(object) {

    if (length(object@Kenmerken) == 0) {
      return(NA)
    }
    Test <- object@Kenmerken %>%
      filter(
        .data$TypeKenmerk == "studiegroep",
        tolower(.data$Kenmerk) %in% tolower(object@Studiegroep$Waarde),
        !is.na(.data$WaardeMax)
      )

    if (nrow(Test) > 0) {
      object@Soortengroep <- data.frame()
      berekenWaarde(as(object, "bedekking"))
    } else {
      berekenWaarde(as(object, "bedekking"))
    }
  }
)
