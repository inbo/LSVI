#' Lijst alle nog op te lossen databankfouten op
#' 
#' Deze functie geeft een log-tabel met alle problemen die nog in de databank zitten.  Enerzijds is er een beperkte tabel met problemen die op een hoger niveau opgelost kunnen worden en anderzijds een detail met alle Voorwaarden waar nog een fout in zit.  Problemen die op beide niveaus kunnen opgelost worden (bv. benoemen van AnalyseVariabelen), staan op beide niveaus vermeld.
#' 
#' @inheritParams selecteerIndicatoren
#' 
#' @return Deze functie geeft een list met 2 dataframes terug
#' 
#' @export
#' 
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr %>% bind_rows filter mutate transmute

logDatabankfouten <- function(ConnectieLSVIhabitats = NULL) {

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
  OndergrensOntbreekt <-
    dbGetQuery(
      ConnectieLSVIhabitats,
      "SELECT Lijst.Naam as Lijstnaam, LijstItem.Waarde
      FROM Lijst INNER JOIN LijstItem
      ON Lijst.Id = LijstItem.LijstId
      WHERE LijstItem.Ondergrens is NULL"
    ) %>%
    transmute(
      Item =
        paste("Schaal: ", .data$Lijstnaam, "; Waarde: ",
              .data$Waarde, sep = ""),
      Categorie = "Ondergrens ontbreekt"
    )
  BovengrensOntbreekt <-
    dbGetQuery(
      ConnectieLSVIhabitats,
      "SELECT Lijst.Naam as Lijstnaam, LijstItem.Waarde
      FROM Lijst INNER JOIN LijstItem
      ON Lijst.Id = LijstItem.LijstId
      WHERE LijstItem.Bovengrens is NULL"
    ) %>%
    transmute(
      Item =
        paste("Schaal: ", .data$Lijstnaam, "; Waarde: ",
              .data$Waarde, sep = ""),
      Categorie = "Bovengrens ontbreekt"
    )
  OnbekendeAV <-
    dbGetQuery(
      ConnectieLSVIhabitats,
      "SELECT distinct(VariabeleNaam)
      FROM AnalyseVariabele INNER JOIN Voorwaarde
      On AnalyseVariabele.Id = Voorwaarde.AnalyseVariabeleId
      WHERE NOT VariabeleNaam in ('aantal', 'aandeel', 'aandeelKruidlaag',
        'bedekking', 'maxBedekking', 'maxBedekkingExcl', 'bedekkingLaag',
        'bedekkingSom', 'bedekkingExcl')
      AND NOT VariabeleNaam LIKE 'meting%'"
    ) %>%
    transmute(
      Item = .data$VariabeleNaam,
      Categorie = "AnalyseVariabele waarvoor geen code ontwikkeld is"
    )

  Fouten <-
    OndergrensOntbreekt %>%
    bind_rows(
      BovengrensOntbreekt
    ) %>%
    bind_rows(
      OnbekendeAV
    )

  Invoervereisten <-
    geefInvoervereisten(ConnectieLSVIhabitats = ConnectieLSVIhabitats)
  OnbekendeAV <- Invoervereisten %>%
    filter(
      !.data$AnalyseVariabele %in%
        c("aantal", "aandeel", "aandeelKruidlaag", "bedekking", "bedekkingLaag",
          "maxBedekking", "maxBedekkingExcl", "bedekkingSom", "bedekkingExcl"),
      !grepl("^meting", .data$AnalyseVariabele)
    )
  TypeAantalNietGeheelGetal <- Invoervereisten %>%
    filter(
      .data$AnalyseVariabele == "aantal" &
        .data$TypeVariabele != "Geheel getal"
    )
  TypeBedekkingFout <- Invoervereisten %>%
    filter(
      .data$AnalyseVariabele %in%
        c("bedekking", "maxBedekking", "maxBedekkingExcl"),
      !.data$TypeVariabele %in% c("Percentage", "Categorie")
    )
  TypeAandeelFout <- Invoervereisten %>%
    filter(
      .data$AnalyseVariabele %in%
        c("aandeel", "aandeelKruidlaag", "bedekkingSom", "bedekkingExcl"),
      !.data$TypeVariabele %in% c("Percentage")
    )
  LijstItems <-
    dbGetQuery(
      ConnectieLSVIhabitats,
      "SELECT Waarde FROM LijstItem"
    )

  Voorwaarden <- OnbekendeAV %>%
    mutate(
      Probleem = "AnalyseVariabele waarvoor geen code ontwikkeld is"
    ) %>%
    bind_rows(
      TypeAantalNietGeheelGetal %>%
        mutate(
          Probleem =
            "TypeVariabele moet een geheel getal zijn (AV aantal)"
        )
    ) %>%
    bind_rows(
      TypeBedekkingFout %>%
        mutate(
          Probleem =
            "TypeVariabele moet een percentage of categorie zijn (bedekking)"
        )
    ) %>%
    bind_rows(
      TypeAandeelFout %>%
        mutate(
          Probleem =
            "TypeVariabele moet een percentage zijn (aandeel)"
        )
    ) %>%
    bind_rows(
      Invoervereisten %>%
        filter(
          is.na(.data$TypeVariabele) & !is.na(.data$VoorwaardeID)
        ) %>%
        mutate(
          Probleem =
            "TypeVariabele moet ingevuld worden"
        )
    ) %>%
    bind_rows(
      Invoervereisten %>%
        filter(
          .data$TypeVariabele == "Vrije tekst"
        ) %>%
        mutate(
          Probleem =
            "TypeVariabele mag geen vrije tekst zijn"
        )
    ) %>%
    bind_rows(
      Invoervereisten %>%
        filter(
          .data$TypeVariabele == "Geheel getal"
        ) %>%
        filter(
          !as.numeric(.data$Referentiewaarde) -
            round(as.numeric(.data$Referentiewaarde)) == 0
        ) %>%
        mutate(
          Probleem =
            "Referentiewaarde moet geheel getal zijn (of TypeVariabele aanpassen)" #nolint
        )
    ) %>%
    bind_rows(
      Invoervereisten %>%
        filter(
          .data$TypeVariabele == "Percentage",
          !.data$Referentiewaarde %in% unique(Invoervereisten$Voorwaarde)
        ) %>%
        filter(
          as.numeric(.data$Referentiewaarde) > 100
        ) %>%
        mutate(
          Probleem =
            "Referentiewaarde moet percentage zijn (of TypeVariabele aanpassen)"
        )
    ) %>%
    bind_rows(
      Invoervereisten %>%
        filter(
          .data$TypeVariabele == "Categorie"
        ) %>%
        filter(
          !.data$Referentiewaarde %in% LijstItems$Waarde
        ) %>%
        mutate(
          Probleem =
            paste(
              "Referentiewaarde moet in volgende lijst voorkomen: ",
              paste(LijstItems$Waarde, collapse = ","),
              " (ofwel TypeVariabele niet als categorie opgeven) ",
              "OPMERKING: afblijven als AnalyseVariabele nog niet in orde is!",
              sep = ""
            )
        )
    ) %>%
    bind_rows(
      Invoervereisten %>%
        filter(
          .data$TypeVariabele == "Categorie" & is.na(.data$Invoertype)
        ) %>%
        mutate(
          Probleem =
            "Aan een categorische variabele moet een lijst (schaal) gekoppeld zijn." #nolint
        )
    ) %>%
    bind_rows(
      Invoervereisten %>%
        filter(
          .data$AnalyseVariabele %in% c("aantal", "bedekking")
        ) %>%
        filter(
          is.na(.data$TaxongroepId) & is.na(.data$Studiegroepnaam)
        ) %>%
        mutate(
          Probleem =
            "Er is geen soortengroep of studiegroep opgegeven"
        )
    ) %>%
    bind_rows(
      Invoervereisten %>%
        filter(
          !is.na(.data$SubAnalyseVariabele) &
            .data$SubAnalyseVariabele != "bedekking"
        ) %>%
        mutate(
          Probleem =
            "De SubAnalyseVariabele moet bedekking zijn"
        )
    ) %>%
    bind_rows(
      Invoervereisten %>%
        filter(
          is.na(.data$SubAnalyseVariabele) &
            !is.na(.data$SubReferentiewaarde)
        ) %>%
        mutate(
          Probleem =
            "De SubAnalyseVariabele moet ingevuld zijn als er een SubReferentiewaarde opgegeven is" #nolint
        )
    ) %>%
    bind_rows(
      Invoervereisten %>%
        filter(
          .data$Operator == "=",
          .data$TypeVariabele != "Ja/nee"
        ) %>%
        mutate(
          Probleem =
            "De Operator '=' mag niet gebruikt worden, tenzij TypeVariabele 'Ja/nee' is" #nolint
        )
    )

  return(list(Fouten, Voorwaarden))
}
