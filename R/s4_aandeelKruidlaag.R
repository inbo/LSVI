#' S4-klasse die aandeel bedekking soorten binnen de kruidlaag berekend
#'
#' Deze klasse AandeelKruidlaag staat in voor de berekening van waarden voor
#' TypeVariabele AandeelKruidlaag op basis van opgegeven kenmerken.  Ze is een
#' nakomeling van de klasse bedekking.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk,
#' TypeKenmerk, WaardeMin en WaardeMax
#'
#' @importFrom methods setClass setMethod
#'
#' @noRd
#'
#' @include s4_bedekking.R
setClass(
  Class = "aandeelKruidlaag",
  representation =
    representation(),
  contains = "bedekking"
)

setMethod(
  f = "berekenWaarde",
  signature = "aandeelKruidlaag",
  definition = function(object) {

    # bedekking sleutelsoorten
    teller <- berekenWaarde(as(object, "bedekking"))

    #bedekking vegetatielaag
    vegetatielaag <- object@Kenmerken %>%
      filter(
        .data$TypeKenmerk == "studiegroep",
        tolower(.data$Kenmerk) %in% tolower(object@Studiegroep$Waarde),
        !is.na(.data$WaardeMax)
      )

    if (nrow(vegetatielaag) > 0) {

      #indien bedekking vegetatielaag is meegegeven wordt deze als noemer
      #gebruikt
      resultaat <-
        c(
          teller[1] / vegetatielaag$WaardeMax,
          teller[2] / vegetatielaag$WaardeMin
        )

    } else{
      #indien bedekking vegetatielaag niet is meegegeven wordt deze berekend
      #op basis van alle soorten in kruidlaag
      soorten_vegetatielaag <- object@Kenmerken %>%
        filter(
          tolower(.data$Vegetatielaag) %in% tolower(object@Studiegroep$Waarde))
      BedekkingMin <-
        (1.0 - prod((1.0 - soorten_vegetatielaag$WaardeMin), na.rm = TRUE))
      BedekkingMax <-
        (1.0 - prod((1.0 - soorten_vegetatielaag$WaardeMax), na.rm = TRUE))

      resultaat <- c(teller[1] / BedekkingMax, teller[2] / BedekkingMin)

    }

    return(resultaat)
  }
)

setMethod(
  f = "geefTheoretischMaximum",
  signature = "aandeelKruidlaag",
  definition = function(object) {
    return(1)
  }
)
