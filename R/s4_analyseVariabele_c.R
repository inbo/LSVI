#' Constructor van s4-classe AnalyseVariabele
#' 
#' @importFrom assertthat assert_that
#' @importFrom RODBC sqlQuery
#' 
#' @export

analyseVariabele_c <-
  function(
    VoorwaardeID,
    Kenmerken,
    ConnectieLSVIhabitats,
    LIJST
  ) {
    assert_that(inherits(ConnectieLSVIhabitats, "RODBC"))
    
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
    
    Kenmerken <- Kenmerken %>%
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
    
    #Soortengroep nog toevoegen!!!
      
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