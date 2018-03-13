#' @title Geeft de invoervereisten voor de waarde van een opname
#'
#' @description Deze functie geeft alle informatie die nodig is om veldobservaties klaar te maken voor de berekening van de de Lokale Staat van Instandhouding met de functie berekenLSVI(), alsook de berekeningsregels die gebruikt worden.  Allereerst geeft ze de 'Voorwaarde' die vermeld moet worden bij de observaties (zie Data_voorwaarden bij berekenLSVIbasis), samen met informatie uit de LSVI-tabellen (vnl. beoordelingsmatrix) en een beschrijving van de voorwaarde ('Voorwaarde') die zou moeten toelaten om de koppeling te maken.
#'
#' Verder geeft ze informatie over de Waarde die verwacht wordt in de functie berekenLSVIbasis().  AnalyseVariabele is een korte omschrijving voor de variabele waarde, bv. 'aantal' staat voor het aantal soorten of klassen en 'bedekking' voor de totale bedekking van de lijst soorten of klassen.  'Referentiewaarde' en 'Operator' geven respectievelijk de grenswaarde en de vergelijking aan op basis waarvan de beoordeling van de waarde zal gebeuren.  Voor elke AnalyseVariabele wordt informatie gegeven over het formaat dat verwacht wordt voor Waarde: de 'Eenheid' (die niet opgenomen moet worden in Waarde maar wel de grootte-orde van het verwachte getal aangeeft), het formaat van de variabele ('TypeVariabele'), en bij categorische variabelen het 'Invoertype' en de 'Invoerwaarde' (een naam voor de categorische variabele en de mogelijke waarden die deze kan aannemen).
#' 
#' Waar nodig, wordt een soortengroep of studiegroep opgegeven.  Een studiegroep is eigenlijk equivalent aan een soortengroep: de verschillende klassen of fasen of ... waarvoor een bedekking of andere analysevariabele moet berekend worden.  Voorbeelden zijn groeiklassen, vegetatielagen, ...  Omwille van de overzichtelijkheid van de tabel is voor de Soortengroep enkel een ID gegeven, de volledige lijst kan opgevraagd worden met de functie geefSoortenlijstInvoerniveau.
#' 
#' Ingeval van de AnalyseVariabele aantal kan er ook een SubAnalyseVariabele vermeld zijn, meestal 'bedekking', die aangeeft aan welke voorwaarde elke soort of klasse afzonderlijk moet voldoen.  Aan deze SubAnalysevariabele zijn dezelfde velden gekoppeld als aan AnalyseVariabele, nl. SubReferentiewaarde, SubOperator, SubEenheid, TypeSubVariabele, SubInvoertype en SubInvoerwaarde.  Bijvoorbeeld, bij de voorwaarde 'minimum 5 soorten minimum talrijk aanwezig' zal de AnalyseVariabele 'aantal' zijn, de Referentiewaarde '5', de Operator '>=', TypeVariabele 'Geheel getal', SubAnalysevariabele 'bedekking', SubReferentiewaarde 'T', SubOperator '>=', TypeSubVariabele 'Categorie' en SubInvoertype 'Beheermonitoringsschaal 2017'.
#'
#' 
#'
#' @inheritParams selecteerIndicatoren
#' @inheritParams berekenLSVIbasis
#' @param Weergave Wat moet er in de tabel weergegeven worden?  De default 'basis' geeft een meer overzichtelijke tabel waarbij mogelijke invoerwaarden gescheiden door een komma in 1 cel weergegeven worden, 'uitgebreid' geeft deze invoerwaarden met alle bijhorende informatie weer in aparte records, waardoor de tabel groot en onoverzichtelijk is.
#'
#' @return Deze functie geeft een tabel met de hierboven beschreven informatie uit de databank.
#'
#' @examples
#' geefInvoervereisten(
#'   Versie = "Versie 3",
#'   Habitattype = "4030",
#'   Kwaliteitsniveau = "1"
#' )
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
                                Weergave = c("basis", "uitgebreid"),
                                ConnectieLSVIhabitats = connecteerMetLSVIdb()){

  assert_that(inherits(ConnectieLSVIhabitats, "RODBC"))
  match.arg(Weergave)

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
            Voorwaarde.VoorwaardeNaam AS Voorwaarde, Voorwaarde.ExtraBewerking,
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
      as.is = TRUE,
      stringsAsFactors = FALSE
    )

  if (tolower(Weergave[1]) == "basis") {
    Voorwaardeinfo <- Voorwaardeinfo %>%
      arrange(
        .data$Invoervolgnr,
        .data$Studievolgnr,
        .data$SubInvoervolgnr
      ) %>%
      group_by(
        .data$VoorwaardeID, .data$Voorwaarde,
        .data$ExtraBewerking, .data$Referentiewaarde,
        .data$Operator, .data$AnalyseVariabele,
        .data$Eenheid, .data$TypeVariabele,
        .data$Invoertype,
        .data$SoortengroepID, .data$SoortengroepNaam,
        .data$Studiegroepnaam, .data$Studielijstnaam,
        .data$SubAnalyseVariabele, .data$SubEenheid,
        .data$TypeSubVariabele, .data$SubReferentiewaarde,
        .data$SubOperator, .data$SubInvoertype
      ) %>%
      summarise(
        Invoerwaarde =
          paste(unique(.data$Invoerwaarde), collapse = ", "),
        Studiewaarde =
          paste(unique(.data$Studiewaarde), collapse = ", "),
        SubInvoerwaarde =
          paste(unique(.data$SubInvoerwaarde), collapse = ", "),
      ) %>%
      ungroup() %>%
      select(                   #volgorde aanpassen
        .data$VoorwaardeID, .data$Voorwaarde,
        .data$ExtraBewerking, .data$Referentiewaarde,
        .data$Operator, .data$AnalyseVariabele,
        .data$Eenheid, .data$TypeVariabele,
        .data$Invoertype, .data$Invoerwaarde,
        .data$SoortengroepID, .data$SoortengroepNaam,
        .data$Studiegroepnaam, .data$Studielijstnaam,
        .data$Studiewaarde,
        .data$SubAnalyseVariabele, .data$SubEenheid,
        .data$TypeSubVariabele, .data$SubReferentiewaarde,
        .data$SubOperator, .data$SubInvoertype,
        .data$SubInvoerwaarde
      )
  }

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
