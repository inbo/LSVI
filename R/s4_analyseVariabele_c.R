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
        stringsAsFactors = FALSE
      )
    
    AnalyseObject <-
      new(
        Class = VoorwaardeInfo$TypeAnalyseVariabele,
        VoorwaardeID = VoorwaardeID,
        Kenmerken = Kenmerken) 
    
    #Soortengroep nog toevoegen!!!
      
    if (!is.na(VoorwaardeInfo$StudiegroepId)) {
      queryStudiegroep <-
        sprintf(
          "SELECT StudieItem.Waarde, StudieItem.Ondergrens,
          StudieItem.Gemiddelde, StudieItem.Bovengrens
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