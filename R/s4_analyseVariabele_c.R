#' Constructor van s4-classe AnalyseVariabele
#' 
#' @importFrom assertthat assert_that
#' @importFrom RODBC sqlQuery
#' @importFrom n2khelper get_nbn_key
#' 
#' @export

analyseVariabele_c <-
  function(
    VoorwaardeID,
    Kenmerken,
    ConnectieLSVIhabitats,
    ConnectieNBN,
    LIJST
  ) {
    assert_that(inherits(ConnectieLSVIhabitats, "RODBC"))
    assert_that(inherits(ConnectieNBN, "RODBC"))
    
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
      sqlQuery(
        ConnectieLSVIhabitats,
        queryVoorwaarde,
        as.is = TRUE,
        stringsAsFactors = FALSE
      )
    
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
        LIJST
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
    
    AnalyseObject <-
      new(
        Class = VoorwaardeInfo$TypeAnalyseVariabele,
        VoorwaardeID = VoorwaardeID,
        Kenmerken = Kenmerken2) 
    
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
        select(
          .data$SoortengroepID,
          .data$SoortensubgroepID,
          .data$NBNTaxonVersionKey,
          .data$Taxontype
        )
      setSoortengroep(AnalyseObject) <- Soortengroep
      
      if (!all(is.na(Soortengroep$SoortensubgroepID))) {
        Subsoorten <- Soortengroep %>%
          filter_(~!is.na(SoortensubgroepID)) %>%
          summarise_(
            SoortensubgroepIDs =
              ~ paste(SoortensubgroepID, collapse = ",")
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
        sqlQuery(
          ConnectieLSVIhabitats,
          queryStudiegroep,
          stringsAsFactors = FALSE
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
          LIJST
        )
      setSubRefMin(AnalyseObject) <- SAV$Min
      setSubRefMax(AnalyseObject) <- SAV$Max
      setSubOperator(AnalyseObject) <- VoorwaardeInfo$SubOperator
    }
    
    return(AnalyseObject)
  }