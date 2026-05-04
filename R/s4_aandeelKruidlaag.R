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
    #bedekking vegetatielaag
    Vegetatielaag <- object@Kenmerken %>%
      filter(
        .data$TypeKenmerk == "studiegroep",
        tolower(.data$Kenmerk) %in% tolower(object@Studiegroep$Waarde),
        !is.na(.data$WaardeMax)
      )

    if (nrow(Vegetatielaag) == 0) {
      #indien bedekking vegetatielaag niet is meegegeven wordt deze berekend
      #op basis van alle soorten in kruidlaag
      Vegetatielaag <- object@Kenmerken %>%
        filter(
          tolower(.data$Vegetatielaag) %in% tolower(object@Studiegroep$Waarde)
        )
    }
    # bedekking van alle lagen of soorten samenvoegen
    BedekkingLaagMin <-
      (1.0 - prod((1.0 - Vegetatielaag$WaardeMin), na.rm = TRUE))
    BedekkingLaagMax <-
      (1.0 - prod((1.0 - Vegetatielaag$WaardeMax), na.rm = TRUE))

    # bedekking sleutelsoorten relatief tot laag
    object@Kenmerken <- object@Kenmerken %>%
      mutate(
        WaardeMin = ifelse(
          .data$TypeKenmerk == "soort_nbn",
          .data$WaardeMin / BedekkingLaagMax,
          .data$WaardeMin
        ),
        WaardeMax = ifelse(
          .data$TypeKenmerk == "soort_nbn",
          .data$WaardeMax / BedekkingLaagMin,
          .data$WaardeMax
        )
      )

    resultaat <- berekenWaarde(as(object, "bedekking"))

    # kap af bij een bedekking van 100 % (1.0) om waarden > 100 % te vermijden
    resultaat <- pmin(resultaat, 1.0)

    return(resultaat)
  }
)
