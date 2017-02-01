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
#' ConnectieLSVIhabitats <- connecteerMetLSVIdb()
#' geefInvoervereisten(ConnectieLSVIhabitats, Versie = "Versie 3",
#'                     Habitatsubtype = "4010", Kwaliteitsniveau = "1")
#' library(RODBC)
#' odbcClose(ConnectieLSVIhabitats)
#'
#' @export
#'
#' @importFrom RODBC sqlQuery odbcClose
#' @importFrom dplyr %>% select_ filter_ group_by_ summarise_ ungroup left_join mutate_ rowwise
#' @importFrom tidyr gather_
#'
#'
geefInvoervereisten <- function(ConnectieLSVIhabitats,
                                Versie = "alle",
                                Habitatgroep = "alle",
                                Habitattype = "alle",
                                Habitatsubtype = "alle",
                                Criterium = "alle",
                                Indicator = "alle",
                                Kwaliteitsniveau = "alle"){

  assert_that(inherits(ConnectieLSVIhabitats,"RODBC"))

  Kwaliteitsniveau <- ifelse(Kwaliteitsniveau == 1, "1",
                             ifelse(Kwaliteitsniveau == 2, "2",
                                    Kwaliteitsniveau))
  assert_that(is.string(Kwaliteitsniveau))
  if (!(Kwaliteitsniveau %in% geefUniekeWaarden(ConnectieLSVIhabitats,"Beoordeling",
                                               "Kwaliteitsniveau"))) {
    stop(sprintf("Kwaliteitsniveau moet een van de volgende waarden zijn: %s",
                 geefUniekeWaarden(ConnectieLSVIhabitats,"Beoordeling","Kwaliteitsniveau")))
  }



  Selectiewaarden <-
    selecteerIndicatoren(ConnectieLSVIhabitats, Versie, Habitatgroep, Habitattype, Habitatsubtype,
                         Criterium, Indicator) %>%
    select_(~Versie, ~Habitattype, ~Habitatsubtype, ~Indicator_beoordelingID)

  Indicator_beoordelingIDs <-
    paste(unique((Selectiewaarden %>% filter_(~!is.na(Indicator_beoordelingID)))$Indicator_beoordelingID),
                 collapse = ",")

  query_selecteerKwaliteitsniveau <-
    ifelse(Kwaliteitsniveau[1] == "alle","",
           sprintf("AND Beoordeling.Kwaliteitsniveau = %s",
                   Kwaliteitsniveau))

  query_LSVIinfo <-
    sprintf("SELECT Indicator_beoordeling.Id AS Indicator_beoordelingID,
            Criterium.Naam AS Criterium, Indicator.Naam AS Indicator,
            Beoordeling.Beoordeling_letterlijk AS Beoordeling,
            Beoordeling.kwaliteitsniveau,
            Beoordeling.Id as BeoordelingID
            FROM (Indicator_beoordeling LEFT JOIN Beoordeling ON Indicator_beoordeling.Id = Beoordeling.Indicator_beoordelingID)
            LEFT JOIN (Indicator INNER JOIN Criterium ON Indicator.CriteriumID = Criterium.Id)
            ON Indicator_beoordeling.IndicatorID = Indicator.Id
            WHERE Indicator_beoordeling.Id in (%s) %s",
            Indicator_beoordelingIDs, query_selecteerKwaliteitsniveau)

  LSVIinfo <- sqlQuery(ConnectieLSVIhabitats, query_LSVIinfo, stringsAsFactors = FALSE)

  BeoordelingIDs <-
    paste(unique((LSVIinfo %>% filter_(~!is.na(BeoordelingID)))$BeoordelingID),
                 collapse = ",")

  query_combinerenVoorwaarden <-
    sprintf("
            WITH voorwaardencombinatie
            AS
            (
            SELECT CombinerenVoorwaarden.Id,
            CombinerenVoorwaarden.BeoordelingID,
            CombinerenVoorwaarden.VoorwaardeID1,
            CombinerenVoorwaarden.VoorwaardeID2,
            CombinerenVoorwaarden.ChildID1,
            CombinerenVoorwaarden.ChildID2,
            CombinerenVoorwaarden.BewerkingAND
            FROM CombinerenVoorwaarden
            WHERE CombinerenVoorwaarden.BeoordelingID in (%s)
            UNION ALL
            SELECT CombinerenVoorwaarden2.Id,
            CombinerenVoorwaarden2.BeoordelingID,
            CombinerenVoorwaarden2.VoorwaardeID1,
            CombinerenVoorwaarden2.VoorwaardeID2,
            CombinerenVoorwaarden2.ChildID1,
            CombinerenVoorwaarden2.ChildID2,
            CombinerenVoorwaarden2.BewerkingAND
            FROM CombinerenVoorwaarden AS CombinerenVoorwaarden2
            INNER JOIN CombinerenVoorwaarden
            ON CombinerenVoorwaarden2.Id = CombinerenVoorwaarden.ChildID1
            UNION ALL
            SELECT CombinerenVoorwaarden3.Id,
            CombinerenVoorwaarden3.BeoordelingID,
            CombinerenVoorwaarden3.VoorwaardeID1,
            CombinerenVoorwaarden3.VoorwaardeID2,
            CombinerenVoorwaarden3.ChildID1,
            CombinerenVoorwaarden3.ChildID2,
            CombinerenVoorwaarden3.BewerkingAND
            FROM CombinerenVoorwaarden AS CombinerenVoorwaarden3
            INNER JOIN CombinerenVoorwaarden
            ON CombinerenVoorwaarden3.Id = CombinerenVoorwaarden.ChildID2
            )
            Select * FROM voorwaardencombinatie",
              BeoordelingIDs)

  Voorwaarden <- sqlQuery(ConnectieLSVIhabitats, query_combinerenVoorwaarden,
                          stringsAsFactors = FALSE) %>%
    mutate_(
      Combinatie = ~ifelse(is.na(VoorwaardeID1),
                           ifelse(is.na(VoorwaardeID2),"",VoorwaardeID2),
                           ifelse(is.na(VoorwaardeID2),VoorwaardeID1,
                                  ifelse(BewerkingAND,
                                         paste(VoorwaardeID1,VoorwaardeID2, sep = " EN "),
                                         paste(VoorwaardeID1,VoorwaardeID2, sep = " OF "))))
    ) %>%
    distinct_()

  RecFunctie <- function(ID) {
    Record <- Voorwaarden %>%
      filter_(~Id == ID)
    Combinatie <-
      paste(Record$Combinatie,
            ifelse((Record$Combinatie != "" & !is.na(Record$ChildID1)) |
                     (Record$Combinatie != "" & !is.na(Record$ChildID2)),
                   ifelse(Record$BewerkingAND," EN ", " OF "),""),
            ifelse(is.na(Record$ChildID1),"",
                   paste("(",RecFunctie(Record$ChildID1),")", sep = "")),
            ifelse(!is.na(Record$ChildID1) & !is.na(Record$ChildID2),
                   ifelse(Record$BewerkingAND," EN ", " OF "),""),
            ifelse(is.na(Record$ChildID2),"",
                   paste("(",RecFunctie(Record$ChildID2),")", sep = "")),
            sep = "")[1]
    return(Combinatie)
  }

  Children <- unique(c((Voorwaarden %>% filter_(~!is.na(ChildID1)))$ChildID1,
                       (Voorwaarden %>% filter_(~!is.na(ChildID2)))$ChildID2))

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
    paste(unique((BasisVoorwaarden %>% filter_(~!is.na(VoorwaardeID)))$VoorwaardeID),
          collapse = ",")

  query_voorwaardeinfo <-
    sprintf("SELECT Voorwaarde.Id AS VoorwaardeID,
            VoorwaardeNaam.Omschrijving AS VoorwaardeNaam,
            Voorwaarde.SoortengroepID, Soortengroep.Omschrijving AS SoortengroepNaam,
            AnalyseVariabele.VariabeleNaam as AnalyseVariabele,
            AnalyseVariabele.Eenheid, TypeVariabele.Naam AS TypeVariabele,
            Invoermasker.Waarde AS Invoerwaarde,
            Vegetatielaag.Omschrijving AS Vegetatielaag
            FROM ((Voorwaarde LEFT JOIN VoorwaardeNaam ON Voorwaarde.VoorwaardeNaamID = VoorwaardeNaam.Id)
                        LEFT JOIN Soortengroep ON Voorwaarde.SoortengroepID = Soortengroep.Id)
            LEFT JOIN (((AnalyseVariabele LEFT JOIN Invoermasker ON AnalyseVariabele.ID = Invoermasker.AnalyseVariabeleID)
                       LEFT JOIN TypeVariabele ON AnalyseVariabele.TypeVariabeleID = TypeVariabele.Id)
                       LEFT JOIN Vegetatielaag ON AnalyseVariabele.VegetatielaagID = Vegetatielaag.Id)
            ON Voorwaarde.AnalyseVariabeleID = AnalyseVariabele.Id
            WHERE Voorwaarde.Id in (%s)",VoorwaardenIDs)

  Voorwaardeinfo <- sqlQuery(ConnectieLSVIhabitats, query_voorwaardeinfo, stringsAsFactors = FALSE) %>%
    group_by_(~VoorwaardeID, ~VoorwaardeNaam, ~SoortengroepID, ~SoortengroepNaam,
              ~AnalyseVariabele, ~Vegetatielaag, ~Eenheid, ~TypeVariabele) %>%
    summarise_(
      Invoermasker = ~paste(Invoerwaarde, collapse = ", ")
    ) %>%
    ungroup()


  Invoervereisten <- Selectiewaarden %>%
    left_join(LSVIinfo, by = c("Indicator_beoordelingID" = "Indicator_beoordelingID")) %>%
    mutate_(Indicator_beoordelingID = ~NULL) %>%
    left_join(BasisVoorwaarden, by = c("BeoordelingID" = "BeoordelingID")) %>%
    left_join(Voorwaardeinfo, by = c("VoorwaardeID" = "VoorwaardeID"))

  return(Invoervereisten)
}

