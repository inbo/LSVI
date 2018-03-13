#' Invoercontrole voor dataframe Data_soortenKenmerken
#'
#' Om te vermijden dat we meermaals dezelfde invoercontrole moeten uitvoeren en om de hoofdscripts overzichtelijk te houden, maken we voor elke invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.  Deze wordt NIET geëxporteerd, dus deze functies kunnen niet als commando gerund worden (maar worden wel gerund als de functie waarin ze voorkomen, aangeroepen wordt).  Ingeval van Data_soortenKenmerken is ook de omzetting van soortnamen naar een NBNTaxonVersionKey en de omzettingen van bedekkingen naar een interval opgenomen in de functie.
#'
#' @param Data_soortenKenmerken dataframe waarop invoercontrole moet gebeuren.
#' @inheritParams berekenLSVIbasis
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom n2khelper get_nbn_key
#' 
#' @export
#'
invoercontroleData_soortenKenmerken <-
  function(Data_soortenKenmerken, ConnectieLSVIhabitats, ConnectieNBN, LIJST) {
    assert_that(inherits(ConnectieLSVIhabitats, "RODBC"))
    assert_that(inherits(ConnectieNBN, "RODBC"))

    assert_that(inherits(Data_soortenKenmerken, "data.frame"))
    assert_that(has_name(Data_soortenKenmerken, "ID"))
    assert_that(has_name(Data_soortenKenmerken, "Kenmerk"))
    if (!is.character(Data_soortenKenmerken$Kenmerk)) {
      Data_soortenKenmerken$Kenmerk <-
        as.character(Data_soortenKenmerken$Kenmerk)
    }
    assert_that(has_name(Data_soortenKenmerken, "TypeKenmerk"))
    if (!is.character(Data_soortenKenmerken$TypeKenmerk)) {
      Data_soortenKenmerken$TypeKenmerk <-
        as.character(Data_soortenKenmerken$TypeKenmerk)
    }
    #hier moet nog controle op gebeuren!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    assert_that(has_name(Data_soortenKenmerken, "Waarde"))
    assert_that(has_name(Data_soortenKenmerken, "Type"))
    if (!is.character(Data_soortenKenmerken$Type)) {
      Data_soortenKenmerken$Type <-
        as.character(Data_soortenKenmerken$Type)
    }
    if (
      !all(Data_soortenKenmerken$Type %in%
           geefUniekeWaarden(
             "TypeVariabele",
             "Naam",
             ConnectieLSVIhabitats
           )
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Type komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_soortenKenmerken, "Invoertype"))
    if (!is.character(Data_soortenKenmerken$Invoertype)) {
      Data_soortenKenmerken$Invoertype <-
        as.character(Data_soortenKenmerken$Invoertype)
    }
    if (!all(is.na(Data_soortenKenmerken$Invoertype) |
             Data_soortenKenmerken$Invoertype %in%
             geefUniekeWaarden("Lijst", "Naam", ConnectieLSVIhabitats))) {
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Invoertype komen overeen met waarden vermeld in de databank.") #nolint
    }
    assert_that(has_name(Data_soortenKenmerken, "Eenheid"))
    if (!is.character(Data_soortenKenmerken$Eenheid)) {
      Data_soortenKenmerken$Eenheid <-
        as.character(Data_soortenKenmerken$Eenheid)
    }
    if (
      !all(
        Data_soortenKenmerken$Eenheid %in%
        geefUniekeWaarden(
          "AnalyseVariabele",
          "Eenheid",
          ConnectieLSVIhabitats
        )
      )
    ) {
      stop("Niet alle waarden vermeld onder Data_soortenKenmerken$Eenheid komen overeen met waarden vermeld in de databank.") #nolint
    }


    #○mzettingen naar een bruikbare dataframe
    Kenmerken <- Data_soortenKenmerken #naamsverandering is omdat code verplaatst is

    KenmerkenSoort <- Kenmerken %>%
      filter(tolower(.data$TypeKenmerk) == "soort_latijn") %>%
      mutate(
        Kenmerk =
          gsub(
            pattern = "^([[:alpha:]]*) ([[:alpha:]]*) (.*)",
            replacement = "\\1 \\2",
            x = .data$Kenmerk
          )
      ) %>%
      bind_rows(
        Kenmerken %>%
          filter(tolower(.data$TypeKenmerk) == "soort_nl")
      )

    Vertaling <-
      get_nbn_key(KenmerkenSoort$Kenmerk, channel = ConnectieNBN) %>%
      select(.data$InputName, .data$NBNKey)

    KenmerkenSoort <- KenmerkenSoort %>%
      left_join(
        Vertaling,
        by = c("Kenmerk" = "InputName")
      ) %>%
      mutate(
        Kenmerk = .data$NBNKey,
        NBNKey = NULL,
        TypeKenmerk = "soort_nbn"
      )

    Kenmerken <- Kenmerken %>%
      filter(
        !tolower(.data$TypeKenmerk) %in% c("soort_latijn", "soort_nl")
      ) %>%
      bind_rows(
        KenmerkenSoort
      ) %>%
      mutate(
        Rijnr = row_number(.data$Kenmerk)
      )

    VertaaldeKenmerken <-
      vertaalInvoerInterval(
        Kenmerken[
          , c("Rijnr", "Type", "Waarde",
              "Eenheid", "Invoertype")
          ],
        LIJST,
        ConnectieLSVIhabitats
      ) %>%
      rename(
        WaardeMin = .data$Min,
        WaardeMax = .data$Max
      )

    Kenmerken2 <- Kenmerken %>%
      left_join(
        VertaaldeKenmerken,
        by = c("Rijnr")
      ) %>%
      mutate(
        Rijnr = NULL,
        Kenmerk = tolower(.data$Kenmerk)
      )

    return(Kenmerken2)
  }