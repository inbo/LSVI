#' S4-klasse die aandeel bedekking soorten binnen de kruidlaag berekent
#'
#' Deze klasse `AandeelKruidlaag` staat in voor de berekening van waarden voor
#' `TypeVariabele` `AandeelKruidlaag` op basis van opgegeven kenmerken.
#' Ze is een nakomeling van de klasse `bedekking`.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden `Kenmerk`,
#' `TypeKenmerk`, `WaardeMin` en `WaardeMax`
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

    # VOORBEREKENING DUINEN
    # bij duinen is de vegetatielaag de totale vegetatiebedekking,
    # te berekenen als 100 % min de bedekking naakte bodem
    # (als totale vegetatiebedekking niet opgegeven is),
    # en hier worden soorten uit alle lagen meegenomen
    # (en bij afwezigheid van bedekking naakte bodem of totale
    # vegetatiebedekking worden alle vegetatielagen samengeteld)
    # stap 1: naakte bodem vervangen door totale vegetatiebedekking
    if (
      "totale vegetatiebedekking" %in% object@Studiegroep$Waarde &&
        !"totale vegetatiebedekking" %in% object@Kenmerken$Kenmerk &&
        "naakte bodem" %in% object@Kenmerken$Kenmerk
    ) {
      object@Kenmerken <- object@Kenmerken %>%
        filter(tolower(.data$Kenmerk) != "naakte bodem") %>%
        bind_rows(
          object@Kenmerken %>%
            filter(tolower(.data$Kenmerk) == "naakte bodem") %>%
            mutate(
              Kenmerk = "totale vegetatiebedekking",
              WaardeMinNew = 1.0 - .data$WaardeMax,
              WaardeMax = 1.0 - .data$WaardeMin,
              WaardeMin = .data$WaardeMinNew,
              WaardeMinNew = NULL
            )
        )
    }
    # stap 2: als de totale vegetatiebedekking opgegeven of berekend is,
    # worden andere vegetatielagen in tabel Kenmerken verwijderd
    # om te vermijden dat alle vegetatielagen nog eens extra meegeteld worden
    # (in Studiegroep blijven ze behouden om alle soorten mee te nemen)
    if (
      "totale vegetatiebedekking" %in% object@Studiegroep$Waarde &&
        "totale vegetatiebedekking" %in% object@Kenmerken$Kenmerk
    ) {
      object@Kenmerken <- object@Kenmerken %>%
        filter(tolower(.data$TypeKenmerk) == "soort_nbn") %>%
        bind_rows(
          object@Kenmerken %>%
            filter(tolower(.data$Kenmerk) == "totale vegetatiebedekking")
        )
    }

    # EIGENLIJKE BEREKENING
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
      vegetatielaag <- vegetatielaag %>%
        summarise(
          WaardeMin = 1.0 - prod((1.0 - .data$WaardeMin), na.rm = TRUE),
          WaardeMax = 1.0 - prod((1.0 - .data$WaardeMax), na.rm = TRUE)
        )

      #indien bedekking vegetatielaag is meegegeven wordt deze als noemer
      #gebruikt
      resultaat <-
        c(
          teller[1] / vegetatielaag$WaardeMax,
          teller[2] / vegetatielaag$WaardeMin
        )

    } else {
      #indien bedekking vegetatielaag niet is meegegeven wordt deze berekend
      #op basis van alle soorten in kruidlaag
      soorten_vegetatielaag <- object@Kenmerken %>%
        filter(
          tolower(.data$Vegetatielaag) %in% tolower(object@Studiegroep$Waarde)
        )

      BedekkingMin <-
        (1.0 - prod((1.0 - soorten_vegetatielaag$WaardeMin), na.rm = TRUE))
      BedekkingMax <-
        (1.0 - prod((1.0 - soorten_vegetatielaag$WaardeMax), na.rm = TRUE))

      resultaat <- c(teller[1] / BedekkingMax, teller[2] / BedekkingMin)

    }

    return(resultaat)
  }
)
