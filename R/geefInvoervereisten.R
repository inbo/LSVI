#' @title Geeft de invoervereisten voor de waarde van een opname
#'
#' @description Deze functie geeft alle informatie die nodig is om
#' veldobservaties klaar te maken voor de berekening van de de Lokale Staat van
#' Instandhouding met de functie berekenLSVI(), alsook de berekeningsregels die
#' gebruikt worden.  Allereerst geeft ze de 'Voorwaarde' die vermeld moet
#' worden bij de observaties (zie Data_voorwaarden bij berekenLSVIbasis), samen
#' met informatie uit de LSVI-tabellen (vnl. beoordelingsmatrix) en een
#' beschrijving van de voorwaarde ('Voorwaarde') die zou moeten toelaten om de
#' koppeling te maken.
#'
#' Verder geeft ze informatie over de Waarde die verwacht wordt in de functie
#' berekenLSVIbasis().  AnalyseVariabele is een korte omschrijving voor de
#' variabele waarde, bv. 'aantal' staat voor het aantal soorten of klassen en
#' 'bedekking' voor de totale bedekking van de lijst soorten of klassen.
#' 'Referentiewaarde' en 'Operator' geven respectievelijk de grenswaarde en de
#' vergelijking aan op basis waarvan de beoordeling van de waarde zal gebeuren.
#' Voor elke AnalyseVariabele wordt informatie gegeven over het formaat dat
#' verwacht wordt voor Waarde: de 'Eenheid' (die niet opgenomen moet worden in
#' Waarde maar wel de grootte-orde van het verwachte getal aangeeft), het
#' formaat van de variabele ('TypeVariabele'), en bij categorische variabelen
#' het 'Invoertype' en de 'Invoerwaarde' (een naam voor de categorische
#' variabele en de mogelijke waarden die deze kan aannemen).
#'
#' Waar nodig, wordt een soortengroep of studiegroep opgegeven.  Een
#' studiegroep is eigenlijk equivalent aan een soortengroep: de verschillende
#' klassen of fasen of ... waarvoor een bedekking of andere analysevariabele
#' moet berekend worden.  Voorbeelden zijn groeiklassen, vegetatielagen, ...
#' Omwille van de overzichtelijkheid van de tabel is voor de Soortengroep enkel
#' een ID gegeven, de volledige lijst kan opgevraagd worden met de functie
#' geefSoortenlijstInvoerniveau.
#'
#' Ingeval van de AnalyseVariabele aantal kan er ook een SubAnalyseVariabele
#' vermeld zijn, meestal 'bedekking', die aangeeft aan welke voorwaarde elke
#' soort of klasse afzonderlijk moet voldoen.  Aan deze SubAnalysevariabele
#' zijn dezelfde velden gekoppeld als aan AnalyseVariabele, nl.
#' SubReferentiewaarde, SubOperator, SubEenheid, TypeSubVariabele,
#' SubInvoertype en SubInvoerwaarde.  Bijvoorbeeld, bij de voorwaarde 'minimum
#' 5 soorten minimum talrijk aanwezig' zal de AnalyseVariabele 'aantal' zijn,
#' de Referentiewaarde '5', de Operator '>=', TypeVariabele 'Geheel getal',
#' SubAnalysevariabele 'bedekking', SubReferentiewaarde 'T', SubOperator '>=',
#' TypeSubVariabele 'Categorie' en SubInvoertype 'Beheermonitoringsschaal 2017'.
#'
#'
#' @inheritParams selecteerIndicatoren
#' @inheritParams berekenLSVIbasis
#' @param Weergave Wat moet er in de tabel weergegeven worden?  De default
#' 'basis' geeft een meer overzichtelijke tabel waarbij mogelijke invoerwaarden
#' gescheiden door een komma in 1 cel weergegeven worden, 'uitgebreid' geeft
#' deze invoerwaarden met alle bijhorende informatie weer in aparte records,
#' waardoor de tabel groot en onoverzichtelijk is.
#'
#' @return Deze functie geeft een tabel met de hierboven beschreven informatie
#' uit de databank.
#'
#' @examples
#' # Omwille van de iets langere lange duurtijd van de commando's staat bij
#' # onderstaande voorbeelden de vermelding 'dontrun' (om problemen te vermijden
#' # bij het testen van het package). Maar de voorbeelden werken en kunnen zeker
#' # uitgetest worden.
#' \dontrun{
#' library(LSVI)
#' maakConnectiePool()
#' geefInvoervereisten(
#'   Versie = "Versie 2.0",
#'   Habitattype = "4030",
#'   Kwaliteitsniveau = "1"
#' )
#' library(pool)
#' poolClose(ConnectiePool)
#' }
#'
#' @export
#'
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr %>% select filter group_by summarise ungroup left_join
#' mutate rowwise arrange distinct
#' @importFrom tidyr gather
#' @importFrom rlang .data
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
                                ConnectieLSVIhabitats = NULL) {

  if (is.null(ConnectieLSVIhabitats)) {
    if (exists("ConnectiePool")) {
      ConnectieLSVIhabitats <- get("ConnectiePool", envir = .GlobalEnv)
    }
  }
  assert_that(
    inherits(ConnectieLSVIhabitats, "DBIConnection") |
      inherits(ConnectieLSVIhabitats, "Pool"),
    msg = "Er is geen connectie met de databank met de LSVI-indicatoren. Maak een connectiepool met maakConnectiePool of geef een connectie mee met de parameter ConnectieLSVIhabitats." #nolint
  )
  match.arg(Weergave)

  Kwaliteitsniveau <-
    invoercontroleKwaliteitsniveau(Kwaliteitsniveau, ConnectieLSVIhabitats)


  Selectiewaarden <-
    selecteerIndicatoren(
      Versie = Versie,
      Habitatgroep = Habitatgroep,
      Habitattype = Habitattype,
      Criterium = Criterium,
      Indicator = Indicator,
      ConnectieLSVIhabitats = ConnectieLSVIhabitats
    ) %>%
    select(
      .data$Versie,
      .data$Habitattype,
      .data$Habitatsubtype,
      .data$Indicator_beoordelingID
    ) %>%
    distinct() %>%
    filter(!is.na(.data$Indicator_beoordelingID))

  indicator_beoordeling_ids <-
    paste(
      unique(
        (Selectiewaarden %>%
           filter(
             !is.na(.data$Indicator_beoordelingID)
           )
         )$Indicator_beoordelingID
      ),
      collapse = "','"
    )

  query_select_kwaliteitsniveau <-
    ifelse(Kwaliteitsniveau[1] == "alle", "",
           sprintf("AND Beoordeling.Kwaliteitsniveau = '%s'",
                   Kwaliteitsniveau))

  query_lsvi_info <-
    sprintf("SELECT Indicator_beoordeling.Id AS Indicator_beoordelingID,
            Criterium.Naam AS Criterium, Indicator.Naam AS Indicator,
            Beoordeling.Beoordeling_letterlijk AS Beoordeling,
            Beoordeling.Kwaliteitsniveau,
            Indicator_beoordeling.Belang,
            Beoordeling.Id as BeoordelingID
            FROM
              (Indicator_beoordeling LEFT JOIN Beoordeling
              ON Indicator_beoordeling.Id = Beoordeling.Indicator_beoordelingId)
            LEFT JOIN
              (Indicator INNER JOIN Criterium
                ON Indicator.CriteriumId = Criterium.Id)
            ON Indicator_beoordeling.IndicatorId = Indicator.Id
            WHERE Indicator_beoordeling.Id in ('%s') %s",
            indicator_beoordeling_ids, query_select_kwaliteitsniveau)

  LSVIinfo <-
    dbGetQuery(ConnectieLSVIhabitats, query_lsvi_info)

  BeoordelingIDs <-
    paste(
      unique(
        (LSVIinfo %>% filter(!is.na(.data$BeoordelingID)))$BeoordelingID
      ),
      collapse = "','"
    )

  query_combineren_voorwaarden <-
    sprintf("SELECT CV.Id, CV.BeoordelingId AS BeoordelingID,
              CV.VoorwaardeID1, CV.VoorwaardeID2,
              CV.ChildID1, CV.ChildID2, CV.BewerkingOperator
            FROM CombinerenVoorwaarden CV
            WHERE CV.BeoordelingId in ('%s')",
              BeoordelingIDs)

  Voorwaarden <-
    dbGetQuery(ConnectieLSVIhabitats, query_combineren_voorwaarden) %>%
    mutate(
      Combinatie =
        ifelse(
          is.na(.data$VoorwaardeID1),
          ifelse(is.na(.data$VoorwaardeID2), "", .data$VoorwaardeID2),
          ifelse(
            is.na(.data$VoorwaardeID2),
            .data$VoorwaardeID1,
            paste(
              .data$VoorwaardeID1, .data$BewerkingOperator, .data$VoorwaardeID2
            )
          )
        )
    ) %>%
    distinct()

  RecFunctie <- function(ID) {
    Record <- Voorwaarden %>%
      filter(.data$Id == ID)
    Combinatie <-
      paste(Record$Combinatie,
            ifelse(
              (Record$Combinatie != "" & !is.na(Record$ChildID1)) |
                (Record$Combinatie != "" & !is.na(Record$ChildID2)),
              paste(" ", Record$BewerkingOperator, " ", sep = ""),
              ""
            ),
            ifelse(is.na(Record$ChildID1), "",
                   paste("(", RecFunctie(Record$ChildID1), ")", sep = "")),
            ifelse(!is.na(Record$ChildID1) & !is.na(Record$ChildID2),
                   paste(" ", Record$BewerkingOperator, " ", sep = ""), ""),
            ifelse(is.na(Record$ChildID2), "",
                   paste("(", RecFunctie(Record$ChildID2), ")", sep = "")),
            sep = "")[1]
    return(Combinatie)
  }

  Children <-
    unique(
      c(
        (Voorwaarden %>% filter(!is.na(.data$ChildID1)))$ChildID1,
        (Voorwaarden %>% filter(!is.na(.data$ChildID2)))$ChildID2
      )
    )

  BasisVoorwaarden <- Voorwaarden %>%
    filter(!.data$Id %in% Children) %>%
    select(.data$Id, .data$BeoordelingID) %>%
    rowwise() %>%
    mutate(
      Combinatie = RecFunctie(.data$Id)
    ) %>%
    left_join(
      Voorwaarden %>%
        select(
          .data$BeoordelingID,
          .data$VoorwaardeID1,
          .data$VoorwaardeID2
        ),
      by = c("BeoordelingID")
    ) %>%
    select(
      .data$BeoordelingID,
      .data$VoorwaardeID1,
      .data$VoorwaardeID2,
      .data$Combinatie
    ) %>%
    gather("MagWeg", "VoorwaardeID", c("VoorwaardeID1", "VoorwaardeID2")) %>%
    filter(!is.na(.data$VoorwaardeID)) %>%
    select(.data$BeoordelingID, .data$Combinatie, .data$VoorwaardeID)

  VoorwaardenIDs <-
    paste(
      unique(
        (BasisVoorwaarden %>% filter(!is.na(.data$VoorwaardeID)))$VoorwaardeID
      ),
      collapse = "','"
    )

  query_voorwaardeinfo <-
    sprintf("SELECT Voorwaarde.Id AS VoorwaardeID,
            Voorwaarde.VoorwaardeNaam AS Voorwaarde,
            Voorwaarde.Referentiewaarde, Voorwaarde.Operator,
            AnalyseVariabele.VariabeleNaam as AnalyseVariabele,
            AnalyseVariabele.Eenheid, TypeVariabele.Naam AS TypeVariabele,
            Lijst.Naam AS Invoertype, LijstItem.Waarde As Invoerwaarde,
            LijstItem.Volgnummer AS Invoervolgnr,
            LijstItem.Omschrijving AS Invoeromschrijving,
            LijstItem.Ondergrens AS Invoerondergrens,
            LijstItem.Gemiddelde AS Invoergemiddelde,
            Lijstitem.Bovengrens AS Invoerbovengrens,
            Voorwaarde.TaxongroepId,
            Taxongroep.Omschrijving AS TaxongroepNaam,
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
            FROM (((((Voorwaarde LEFT JOIN Taxongroep
                       ON Voorwaarde.TaxongroepID = Taxongroep.Id)
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
    dbGetQuery(
      ConnectieLSVIhabitats,
      query_voorwaardeinfo
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
        .data$Referentiewaarde,
        .data$Operator, .data$AnalyseVariabele,
        .data$Eenheid, .data$TypeVariabele,
        .data$Invoertype,
        .data$TaxongroepId, .data$TaxongroepNaam,
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
      ungroup() %>%                   #volgorde aanpassen
      select(
        .data$VoorwaardeID, .data$Voorwaarde,
        .data$Referentiewaarde,
        .data$Operator, .data$AnalyseVariabele,
        .data$Eenheid, .data$TypeVariabele,
        .data$Invoertype, .data$Invoerwaarde,
        .data$TaxongroepId, .data$TaxongroepNaam,
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
