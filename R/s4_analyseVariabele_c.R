#' Constructor van s4-classe AnalyseVariabele
#' 
#' Een constructor is een gebruiksvriendelijke functie die een s4-klasse aanmaakt, zodat een gebruiker niet rechtstreeks geconfronteerd wordt met het aanmaken van een object voor een s4-klasse.  In dit geval worden alle als parameter toegevoegde gegevens netjes in het object gestoken, alsook extra info die uit de databank gehaald wordt.  Een deel van de validatie gebeurt in de s4-klasse AnalyseVariabele zelf.
#' 
#' @inheritParams berekenVoorwaarde
#' 
#' @importFrom assertthat assert_that
#' @importFrom DBI dbGetQuery
#' @importFrom methods new
#' @importFrom dplyr %>% mutate select filter summarise
#' @importFrom rlang .data
#' 
#' @export

analyseVariabele_c <-
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
        Voorwaarde.SoortengroepId,
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
      )

    AnalyseObject <-
      new(
        Class = VoorwaardeInfo$TypeAnalyseVariabele,
        VoorwaardeID = VoorwaardeID)

    if (nrow(Kenmerken) > 0) {
      setKenmerken(AnalyseObject) <- Kenmerken
    }

    if (!is.na(VoorwaardeInfo$SoortengroepId)) {
      Soortengroep <-
        geefSoortenlijstInvoerniveau(
          data.frame(
            Niveau = 1,
            SoortengroepIDs = as.character(VoorwaardeInfo$SoortengroepId),
            stringsAsFactors = FALSE
          ),
          ConnectieLSVIhabitats = ConnectieLSVIhabitats
        ) %>%
        mutate(
          NBNTaxonVersionKey =
            tolower(.data$NBNTaxonVersionKey)
        ) %>%
        select(
          .data$SoortengroepID,
          .data$SoortensubgroepID,
          .data$NBNTaxonVersionKey,
          .data$Taxontype
        )
      setSoortengroep(AnalyseObject) <- Soortengroep

      if (!all(is.na(Soortengroep$SoortensubgroepID))) {
        Subsoorten <- Soortengroep %>%
          filter(!is.na(.data$SoortensubgroepID)) %>%
          summarise(
            SoortensubgroepIDs =
              paste(.data$SoortensubgroepID, collapse = ",")
          )
        Subsoortengroep <-
          geefSoortenlijstSoortniveau(
            Subsoorten$SoortensubgroepIDs,
            ConnectieLSVIhabitats = ConnectieLSVIhabitats
          )
        setSoortensubgroep(AnalyseObject) <- Subsoortengroep
      }
    }

    if (!is.na(VoorwaardeInfo$StudiegroepId)) {
      queryStudiegroep <-
        sprintf(
          "SELECT StudieItem.Waarde, StudieItem.Volgnummer
          FROM StudieItem
          WHERE StudieItem.StudiegroepId  = '%s'",
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
    }

    return(AnalyseObject)
  }