#' @title Geeft de invoervereisten voor de waarde van een opname
#'
#' @description Deze functie geeft alle informatie die nodig is om veldobservaties klaar te maken voor de berekening van de de Lokale Staat van Instandhouding met de functie berekenLSVI().  Allereerst geeft ze de 'VoorwaardeID' die gekoppeld moet worden aan de observaties, samen met informatie uit de LSVI-tabellen (vnl. beoordelingsmatrix) en een beschrijving van de voorwaarde ('VoorwaardeNaam') die zou moeten toelaten om de koppeling te maken.
#'
#' Verder geeft ze informatie over de Waarde die verwacht wordt in de functie berekenLSVI().  AnalyseVariabele is een korte omschrijving voor de variabele waarde, bv. 'aantal_frequent_aanwezig' staat voor 'het aantal soorten dat minimaal frequent aanwezig is'.  Voor een aantal voorwaarden, i.e. de voorwaarden met betrekking tot opnames van soorten, kan Waarde berekend worden met de functie berekenAnalyseVariabele op basis van de AnalyseVariabele, SoortengroepID en een soortenlijst met bedekkingen.  Voor andere gegevens zal de koppeling met VoorwaardeID 'handmatig' moeten gebeuren.  Ingeval van grote datasets is het aan te raden om een koppeling te zoeken tussen deze AnalyseVariabele en de variabele in de dataset, zodat niet voor elke voorwaarde afzonderlijk een koppeling moet gemaakt worden.
#'
#' Voor elke AnalyseVariabele is informatie over het formaat dat verwacht wordt voor Waarde: de Eenheid (die niet opgenomen moet worden in Waarde maar wel de grootte-orde van het verwachte getal aangeeft), het formaat van de variabele (TypeVariabele), bij categorische variabelen het 'Invoermasker' (mogelijke waarden) en de Vegetatielaag die bekeken moet worden.
#'
#' @inheritParams selecteerIndicatoren
#' @inheritParams berekenLSVIbasis
#'
#' @return Deze functie geeft een tabel met de hierboven beschreven informatie uit de databank.
#'
#' @examples
#' geefInvoervereisten(Versie = "Versie 3",
#'                     Habitattype = "4010", Kwaliteitsniveau = "1")
#'
#' @export
#'
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom dplyr %>% select_ filter_ group_by_ summarise_ ungroup left_join mutate_ rowwise
#' @importFrom tidyr gather_
#' @importFrom assertthat assert_that is.string
#'
#'
geefInvoervereisten <- function(Versie = "alle",
                                Habitatgroep = "alle",
                                Habitattype = "alle",
                                Criterium = "alle",
                                Indicator = "alle",
                                Kwaliteitsniveau = "alle",
                                ConnectieLSVIhabitats = connecteerMetLSVIdb()){

  assert_that(inherits(ConnectieLSVIhabitats, "RODBC"))

  Kwaliteitsniveau <- ifelse(Kwaliteitsniveau == 1, "1",
                             ifelse(Kwaliteitsniveau == 2, "2",
                                    Kwaliteitsniveau))
  assert_that(is.string(Kwaliteitsniveau))
  if (!(Kwaliteitsniveau %in% geefUniekeWaarden("Beoordeling",
                                               "Kwaliteitsniveau",
                                               ConnectieLSVIhabitats))) {
    stop(
      sprintf(
        "Kwaliteitsniveau moet een van de volgende waarden zijn: %s",
        geefUniekeWaarden(
          "Beoordeling",
          "Kwaliteitsniveau",
          ConnectieLSVIhabitats
        )
      )
    )
  }



  Selectiewaarden <-
    selecteerIndicatoren(
      Versie = Versie,
      Habitatgroep = Habitatgroep,
      Habitattype = Habitattype,
      Criterium = Criterium,
      Indicator = Indicator,
      ConnectieLSVIhabitats = ConnectieLSVIhabitats
    ) %>%
    select_(~Versie, ~Habitattype, ~Habitatsubtype, ~Indicator_beoordelingID)

  Indicator_beoordelingIDs <-
    paste(
      unique(
        (Selectiewaarden %>%
           filter_(~!is.na(Indicator_beoordelingID)))$Indicator_beoordelingID
      ),
      collapse = "','"
    )

  query_selectKwaliteitsniveau <-
    ifelse(Kwaliteitsniveau[1] == "alle", "",
           sprintf("AND Beoordeling.Kwaliteitsniveau = '%s'",
                   Kwaliteitsniveau))

  query_LSVIinfo <-
    sprintf("SELECT Indicator_beoordeling.Id AS Indicator_beoordelingID,
            Criterium.Naam AS Criterium, Indicator.Naam AS Indicator,
            Beoordeling.Beoordeling_letterlijk AS Beoordeling,
            Beoordeling.Kwaliteitsniveau,
            Beoordeling.Id as BeoordelingID
            FROM
              (Indicator_beoordeling LEFT JOIN Beoordeling
              ON Indicator_beoordeling.Id = Beoordeling.Indicator_beoordelingID)
            LEFT JOIN
              (Indicator INNER JOIN Criterium
                ON Indicator.CriteriumID = Criterium.Id)
            ON Indicator_beoordeling.IndicatorID = Indicator.Id
            WHERE Indicator_beoordeling.Id in ('%s') %s",
            Indicator_beoordelingIDs, query_selectKwaliteitsniveau)

  LSVIinfo <-
    sqlQuery(ConnectieLSVIhabitats, query_LSVIinfo, stringsAsFactors = FALSE)

  BeoordelingIDs <-
    paste(
      unique(
        (LSVIinfo %>% filter_(~!is.na(BeoordelingID)))$BeoordelingID
      ),
      collapse = "','"
    )

  query_combinerenVoorwaarden <-
    sprintf("
            WITH voorwaardencombinatie
            AS
            (
            SELECT CombinerenVoorwaarden1.Id,
            CombinerenVoorwaarden1.BeoordelingID,
            CombinerenVoorwaarden1.VoorwaardeID1,
            CombinerenVoorwaarden1.VoorwaardeID2,
            CombinerenVoorwaarden1.ChildID1,
            CombinerenVoorwaarden1.ChildID2,
            CombinerenVoorwaarden1.BewerkingAND
            FROM CombinerenVoorwaarden AS CombinerenVoorwaarden1
            WHERE CombinerenVoorwaarden1.BeoordelingID in ('%s')
            UNION ALL
            SELECT CombinerenVoorwaarden2.Id,
            CombinerenVoorwaarden2.BeoordelingID,
            CombinerenVoorwaarden2.VoorwaardeID1,
            CombinerenVoorwaarden2.VoorwaardeID2,
            CombinerenVoorwaarden2.ChildID1,
            CombinerenVoorwaarden2.ChildID2,
            CombinerenVoorwaarden2.BewerkingAND
            FROM CombinerenVoorwaarden AS CombinerenVoorwaarden2
            INNER JOIN voorwaardencombinatie
            ON CombinerenVoorwaarden2.Id = voorwaardencombinatie.ChildID1
            UNION ALL
            SELECT CombinerenVoorwaarden3.Id,
            CombinerenVoorwaarden3.BeoordelingID,
            CombinerenVoorwaarden3.VoorwaardeID1,
            CombinerenVoorwaarden3.VoorwaardeID2,
            CombinerenVoorwaarden3.ChildID1,
            CombinerenVoorwaarden3.ChildID2,
            CombinerenVoorwaarden3.BewerkingAND
            FROM CombinerenVoorwaarden AS CombinerenVoorwaarden3
            INNER JOIN voorwaardencombinatie
            ON CombinerenVoorwaarden3.Id = voorwaardencombinatie.ChildID2
            )
            Select * FROM voorwaardencombinatie",
              BeoordelingIDs)

  Voorwaarden <- sqlQuery(ConnectieLSVIhabitats, query_combinerenVoorwaarden,
                          stringsAsFactors = FALSE) %>%
    mutate_(
      Combinatie =
        ~ifelse(
          is.na(VoorwaardeID1),
          ifelse(is.na(VoorwaardeID2), "", VoorwaardeID2),
          ifelse(
            is.na(VoorwaardeID2),
            VoorwaardeID1,
            ifelse(
              BewerkingAND,
              paste(VoorwaardeID1, VoorwaardeID2, sep = " EN "),
              paste(VoorwaardeID1, VoorwaardeID2, sep = " OF ")
            )
          )
        )
    ) %>%
    distinct_()

  RecFunctie <- function(ID) {
    Record <- Voorwaarden %>%
      filter_(~Id == ID)
    Combinatie <-
      paste(Record$Combinatie,
            ifelse(
              (Record$Combinatie != "" & !is.na(Record$ChildID1)) |
                (Record$Combinatie != "" & !is.na(Record$ChildID2)),
              ifelse(Record$BewerkingAND, " EN ", " OF "), ""
            ),
            ifelse(is.na(Record$ChildID1), "",
                   paste("(", RecFunctie(Record$ChildID1), ")", sep = "")),
            ifelse(!is.na(Record$ChildID1) & !is.na(Record$ChildID2),
                   ifelse(Record$BewerkingAND, " EN ", " OF "), ""),
            ifelse(is.na(Record$ChildID2), "",
                   paste("(", RecFunctie(Record$ChildID2), ")", sep = "")),
            sep = "")[1]
    return(Combinatie)
  }

  Children <-
    unique(
      c(
        (Voorwaarden %>% filter_(~!is.na(ChildID1)))$ChildID1,
        (Voorwaarden %>% filter_(~!is.na(ChildID2)))$ChildID2
      )
    )

  BasisVoorwaarden <- Voorwaarden %>%
    filter_(~!Id %in% Children) %>%
    select_(~Id, ~BeoordelingID) %>%
    rowwise() %>%
    mutate_(
      Combinatie = ~RecFunctie(Id)
    ) %>%
    left_join(
      Voorwaarden %>% select_(~BeoordelingID, ~VoorwaardeID1, ~VoorwaardeID2),
      by = c("BeoordelingID")
    ) %>%
    select_(~BeoordelingID, ~VoorwaardeID1, ~VoorwaardeID2, ~Combinatie) %>%
    gather_("MagWeg", "VoorwaardeID", c("VoorwaardeID1", "VoorwaardeID2")) %>%
    filter_(~!is.na(VoorwaardeID)) %>%
    select_(~BeoordelingID, ~Combinatie, ~VoorwaardeID)

  VoorwaardenIDs <-
    paste(
      unique(
        (BasisVoorwaarden %>% filter_(~!is.na(VoorwaardeID)))$VoorwaardeID
      ),
      collapse = "','"
    )

  query_voorwaardeinfo <-
    sprintf("SELECT Voorwaarde.Id AS VoorwaardeID,
            Voorwaarde.VoorwaardeNaam, Voorwaarde.ExtraBewerking,
            Voorwaarde.Referentiewaarde, Voorwaarde.Operator,
            AnalyseVariabele.VariabeleNaam as AnalyseVariabele,
            AnalyseVariabele.Eenheid, TypeVariabele.Naam AS TypeVariabele,
            Lijst.Naam AS Invoertype, LijstItem.Waarde As Invoerwaarde,
            LijstItem.Volgnummer AS Invoervolgnr,
            LijstItem.Omschrijving AS Invoeromschrijving,
            LijstItem.Ondergrens AS Invoerondergrens,
            LijstItem.Gemiddelde AS Invoergemiddelde,
            Lijstitem.Bovengrens AS Invoerbovengrens,
            Voorwaarde.SoortengroepID,
            Soortengroep.Omschrijving AS SoortengroepNaam,
            Studiegroep.Naam AS Studiegroepnaam,
            Studiegroep.LijstNaam as Studielijstnaam,
            StudieItem.Waarde As Studiewaarde,
            StudieItem.Volgnummer AS Studievolgnr,
            StudieItem.Omschrijving AS Studieomschrijving,
            StudieItem.Ondergrens AS Studieondergrens,
            StudieItem.Gemiddelde AS Studiegemiddelde,
            Studieitem.Bovengrens AS Studiebovengrens,
            SubAnalyseVariabele.VariabeleNaam as SubAnalyseVariabele,
            SubAnalyseVariabele.Eenheid AS SubEenheid,
            TypeSubVariabele.Naam AS TypeSubVariabele,
            Voorwaarde.SubReferentiewaarde, Voorwaarde.SubOperator,
            SubLijst.Naam AS SubInvoertype,
            SubLijstItem.Waarde As SubInvoerwaarde,
            SubLijstItem.Volgnummer AS SubInvoervolgnr,
            SubLijstItem.Omschrijving AS SubInvoeromschrijving,
            SubLijstItem.Ondergrens AS SubInvoerondergrens,
            SubLijstItem.Gemiddelde AS SubInvoergemiddelde,
            SubLijstitem.Bovengrens AS SubInvoerbovengrens
            FROM (((((Voorwaarde LEFT JOIN Soortengroep
                       ON Voorwaarde.SoortengroepID = Soortengroep.Id)
            LEFT JOIN (AnalyseVariabele
              LEFT JOIN TypeVariabele
                       ON AnalyseVariabele.TypeVariabeleID = TypeVariabele.Id)
            ON Voorwaarde.AnalyseVariabeleID = AnalyseVariabele.Id)
            LEFT JOIN (Lijst
                      LEFT JOIN LijstItem ON Lijst.Id = LijstItem.LijstId)
              ON Voorwaarde.InvoermaskerId = Lijst.Id)
            LEFT JOIN (Studiegroep
                      LEFT JOIN StudieItem
                            ON Studiegroep.Id = StudieItem.StudiegroepId)
              ON Voorwaarde.StudiegroepId = Studiegroep.Id)
            LEFT JOIN (AnalyseVariabele AS SubAnalyseVariabele
              LEFT JOIN TypeVariabele AS TypeSubVariabele
            ON SubAnalyseVariabele.TypeVariabeleID = TypeSubVariabele.Id)
            ON Voorwaarde.SubAnalyseVariabeleID = SubAnalyseVariabele.Id)
            LEFT JOIN (Lijst AS SubLijst
                      LEFT JOIN LijstItem AS SubLijstItem
                        ON SubLijst.Id = SubLijstItem.LijstId)
              ON Voorwaarde.SubInvoermaskerId = SubLijst.Id
            WHERE Voorwaarde.Id in ('%s')", VoorwaardenIDs)

  Voorwaardeinfo <-
    sqlQuery(
      ConnectieLSVIhabitats,
      query_voorwaardeinfo,
      stringsAsFactors = FALSE
    ) #%>%
    # arrange_(~Invoervolgnr, ~Studievolgnr, ~SubInvoervolgnr) %>%
    # group_by_(-~Invoervolgnr)
    # group_by_(~VoorwaardeID, ~VoorwaardeNaam, ~Referentiewaarde, ~Operator,
    #           ~SoortengroepID, ~SoortengroepNaam,
    #           ~AnalyseVariabele, ~Vegetatielaag, ~Eenheid, ~TypeVariabele) %>%
    # summarise_(
    #   Invoermasker = ~paste(Invoerwaarde, collapse = ", ")
    # ) %>%
    # ungroup()


  Invoervereisten <- Selectiewaarden %>%
    left_join(
      LSVIinfo,
      by = c("Indicator_beoordelingID" = "Indicator_beoordelingID")
    ) %>%
    mutate_(Indicator_beoordelingID = ~NULL) %>%
    left_join(BasisVoorwaarden, by = c("BeoordelingID" = "BeoordelingID")) %>%
    left_join(Voorwaardeinfo, by = c("VoorwaardeID" = "VoorwaardeID"))

  return(Invoervereisten)
}
