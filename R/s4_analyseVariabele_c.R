#' Constructor van s4-classe AnalyseVariabele
#'
#' Een constructor is een gebruiksvriendelijke functie die een s4-klasse
#' aanmaakt, zodat een gebruiker niet rechtstreeks geconfronteerd wordt met het
#' aanmaken van een object voor een s4-klasse.  In dit geval worden alle als
#' parameter toegevoegde gegevens netjes in het object gestoken, alsook extra
#' info die uit de databank gehaald wordt.  Een deel van de validatie gebeurt
#' in de s4-klasse AnalyseVariabele zelf.
#'
#' @inheritParams berekenVoorwaarde
#'
#' @importFrom assertthat assert_that
#' @importFrom DBI dbGetQuery
#' @importFrom methods new
#' @importFrom dplyr %>% mutate select filter summarise
#' @importFrom rlang .data
#'

analyseVariabele_c <- #nolint
  function(
    VoorwaardeID,
    Kenmerken,
    ConnectieLSVIhabitats,
    LIJST
  ) {
    assert_that(
      inherits(ConnectieLSVIhabitats, "DBIConnection") |
        inherits(ConnectieLSVIhabitats, "Pool"),
      msg = "Er is geen connectie met de databank met de LSVI-indicatoren"
    )

    queryVoorwaarde <-
      sprintf(
        "SELECT AV.VariabeleNaam AS TypeAnalyseVariabele,
        Voorwaarde.TaxongroepId,
        Voorwaarde.StudiegroepId,
        SAV.VariabeleNaam AS SubAnalyseVariabele,
        SAV.Eenheid,
        TypeVariabele.Naam AS TypeSubVariabele,
        Voorwaarde.SubReferentiewaarde, Voorwaarde.SubOperator,
        Lijst.Naam AS SubInvoermasker
        FROM ((Voorwaarde LEFT JOIN
          (AnalyseVariabele SAV LEFT JOIN TypeVariabele
              ON SAV.TypeVariabeleId = TypeVariabele.Id)
            ON Voorwaarde.SubAnalyseVariabeleId = SAV.Id)
          LEFT JOIN AnalyseVariabele AV
            ON Voorwaarde.AnalyseVariabeleId = AV.Id)
        LEFT JOIN Lijst ON Voorwaarde.SubInvoermaskerId = Lijst.Id
        WHERE Voorwaarde.Id = '%s'",
          VoorwaardeID
        )
    VoorwaardeInfo <-
      dbGetQuery(
        ConnectieLSVIhabitats,
        queryVoorwaarde
      ) %>%
      mutate(
        TypeAnalyseVariabele =
          ifelse(
            grepl("meting", .data$TypeAnalyseVariabele),
            "meting",
            .data$TypeAnalyseVariabele
          )
      )

    AnalyseObject <-
      new(
        Class = VoorwaardeInfo$TypeAnalyseVariabele,
        VoorwaardeID = VoorwaardeID)

    if (nrow(Kenmerken) > 0) {
      setKenmerken(AnalyseObject) <- Kenmerken
    }

    if (!is.na(VoorwaardeInfo$TaxongroepId)) {
      Soortengroep <-
        geefSoortenlijstVoorIDs(
          Taxongroeplijst = as.character(VoorwaardeInfo$TaxongroepId),
          Taxonlijsttype = "alle",
          ConnectieLSVIhabitats = ConnectieLSVIhabitats
        ) %>%
        mutate(
          NbnTaxonVersionKey =
            tolower(.data$NbnTaxonVersionKey)
        ) %>%
        select(
          .data$TaxongroepId,
          .data$TaxonsubgroepId,
          .data$TaxonId,
          .data$SubTaxonId,
          .data$NbnTaxonVersionKey,
          .data$TaxonType
        ) %>%
        distinct()
      setSoortengroep(AnalyseObject) <- Soortengroep
    }

    if (!is.na(VoorwaardeInfo$StudiegroepId)) {
      queryStudiegroep <-
        sprintf(
          "SELECT StudieItem.Waarde, StudieItem.Volgnummer,
            Studiegroep.LijstNaam
          FROM Studiegroep INNER JOIN StudieItem
          ON Studiegroep.Id = StudieItem.StudiegroepId
          WHERE Studiegroep.Id  = '%s'",
          VoorwaardeInfo$StudiegroepId
        )
      Studiegroep <-
        dbGetQuery(
          ConnectieLSVIhabitats,
          queryStudiegroep
        )
      setStudiegroep(AnalyseObject) <- Studiegroep
    }

    if (!is.na(VoorwaardeInfo$SubAnalyseVariabele)) {
      setSubAnalyseVariabele(AnalyseObject) <-
        VoorwaardeInfo$SubAnalyseVariabele
      SAV <-
        vertaalInvoerInterval(
          data.frame(
            Rijnr = 1,
            Type = VoorwaardeInfo$TypeSubVariabele,
            Waarde = VoorwaardeInfo$SubReferentiewaarde,
            Eenheid = VoorwaardeInfo$Eenheid,
            Invoertype = VoorwaardeInfo$SubInvoermasker,
            stringsAsFactors = FALSE
          ),
          LIJST,
          ConnectieLSVIhabitats
        )
      setSubRefMin(AnalyseObject) <- SAV$Min
      setSubRefMax(AnalyseObject) <- SAV$Max
      setSubOperator(AnalyseObject) <- VoorwaardeInfo$SubOperator
      setSubReferentiewaarde(AnalyseObject) <-
        VoorwaardeInfo$SubReferentiewaarde
    }

    return(AnalyseObject)
  }
