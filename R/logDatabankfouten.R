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
#' @importFrom dplyr %>% bind_rows filter mutate

logDatabankfouten <- function(ConnectieLSVIhabitats = ConnectiePool) {

  OndergrensOntbreekt <-
    dbGetQuery(
      ConnectieLSVIhabitats,
      "SELECT Lijst.Naam as Lijstnaam, LijstItem.Waarde
      FROM Lijst INNER JOIN LijstItem
      ON Lijst.Id = LijstItem.LijstId
      WHERE LijstItem.Ondergrens is NULL"
    )
  BovengrensOntbreekt <-
    dbGetQuery(
      ConnectieLSVIhabitats,
      "SELECT Lijst.Naam as Lijstnaam, LijstItem.Waarde
      FROM Lijst INNER JOIN LijstItem
      ON Lijst.Id = LijstItem.LijstId
      WHERE LijstItem.Bovengrens is NULL"
    )
  OnbekendeAV <-
    dbGetQuery(
      ConnectieLSVIhabitats,
      "SELECT distinct(VariabeleNaam)
      FROM AnalyseVariabele INNER JOIN Voorwaarde
      On AnalyseVariabele.Id = Voorwaarde.AnalyseVariabeleId
      WHERE NOT VariabeleNaam in ('aantal', 'aandeel', 'aandeelKruidlaag',
        'bedekking', 'maxBedekking', 'maxBedekkingExcl', 'bedekkingLaag')
      AND NOT VariabeleNaam LIKE 'meting%'"
    )

  Fouten <-
    data.frame(
      Categorie = "Ondergrens ontbreekt",
      Item =
        paste("Schaal: ", OndergrensOntbreekt$Lijstnaam, "; Waarde: ",
              OndergrensOntbreekt$Waarde, sep = ""),
      stringsAsFactors = FALSE
    ) %>%
    bind_rows(
      data.frame(
        Categorie = "Bovengrens ontbreekt",
        Item =
          paste("Schaal: ", BovengrensOntbreekt$Lijstnaam, "; Waarde: ",
                BovengrensOntbreekt$Waarde, sep = ""),
        stringsAsFactors = FALSE
      )
    ) %>%
    bind_rows(
      data.frame(
        Categorie = "AnalyseVariabele waarvoor geen code ontwikkeld is",
        Item = OnbekendeAV$VariabeleNaam,
        stringsAsFactors = FALSE
      )
    )

  Invoervereisten <- geefInvoervereisten()
  OnbekendeAV <- Invoervereisten %>%
    filter(
      !.data$AnalyseVariabele %in%
        c("aantal", "aandeel", "aandeelKruidlaag", "bedekking", "bedekkingLaag",
          "maxBedekking", "maxBedekkingExcl"),
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
        c("aandeel", "aandeelKruidlaag"),
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
    )

  return(list(Fouten, Voorwaarden))
}
