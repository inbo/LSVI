context("test berekenLSVIbasis vegetatielaag")

library(readr)
library(dplyr)
library(rlang)
library(stringr)

maakConnectiePool()

describe("berekenLSVIbasis vegetatielaag", {
  it("de vegetatielagen worden correct geselecteerd", {
    Data_habitat <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Test9190habitat.csv", package = "LSVI"),
        col_types = list(col_character(), col_character())
      )
    attr(Data_habitat, "spec") <- NULL #nolint: object_name_linter
    Data_voorwaarden <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Test9190voorwaarden.csv", package = "LSVI"),
        col_types = list(
          col_character(), col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(), col_character()
        )
      )
    Data_soortenKenmerken <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Test9190soortenKenmerken.csv", package = "LSVI"),
        col_types = list(
          col_character(), col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(), col_character()
        )
      )

    Resultaatv2 <- list(
      Resultaat_criterium = read_csv2(
        system.file(
          "vbdata/Resultaat_test_bosv2/Resultaat_criterium.csv",
          package = "LSVI"
        ),
        col_types = list(
          col_character(), col_character(), col_character(),
          col_character(), col_character(), col_integer(), col_logical(),
          col_character(), col_double(), col_double()
        )
      ) %>%
        select(-"...1") %>%
        mutate(
          ID = as.character(.data$ID),
          Habitattype = as.character(.data$Habitattype)
        ),
      Resultaat_indicator = read_csv2(
        system.file(
          "vbdata/Resultaat_test_bosv2/Resultaat_indicator.csv",
          package = "LSVI"
        ),
        col_types = list(
          col_character(), col_character(), col_character(),
          col_character(), col_character(),
          col_character(), col_character(), col_character(),
          col_integer(), col_logical(), col_double()
        )
      ) %>%
        select(-"...1"),
      Resultaat_detail = read_csv2(
        system.file(
          "vbdata/Resultaat_test_bosv2/Resultaat_detail.csv",
          package = "LSVI"
        ),
        col_types = list(
          col_character(), col_character(), col_character(),
          col_character(), col_character(),
          col_character(), col_character(), col_integer(),
          col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(), col_double(),
          col_logical(), col_double()
        )
      ) %>%
        select(-"...1"),
      Resultaat_globaal = read_csv2(
        system.file(
          "vbdata/Resultaat_test_bosv2/Resultaat_globaal.csv",
          package = "LSVI"
        ),
        col_types = list(
          col_character(), col_character(), col_character(),
          col_character(), col_integer(), col_logical(),
          col_character(), col_double(), col_double(), col_double()
        )
      ) %>%
        select(-"...1")
    )
    attr(Resultaatv2[["Resultaat_criterium"]], "spec") <- NULL
    attr(Resultaatv2[["Resultaat_indicator"]], "spec") <- NULL
    attr(Resultaatv2[["Resultaat_detail"]], "spec") <- NULL
    attr(Resultaatv2[["Resultaat_globaal"]], "spec") <- NULL

    BerekendRes <-
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      )
    stopifnot(
      all.equal(
        as.data.frame(BerekendRes[["Resultaat_criterium"]]),
        as.data.frame(Resultaatv2[["Resultaat_criterium"]])
      )
    )
    stopifnot(
      all.equal(
        as.data.frame(BerekendRes[["Resultaat_indicator"]]),
        as.data.frame(Resultaatv2[["Resultaat_indicator"]])
      )
    )
    stopifnot(
      all.equal(
        as.data.frame(BerekendRes[["Resultaat_detail"]]),
        as.data.frame(Resultaatv2[["Resultaat_detail"]])
      )
    )
    stopifnot(
      all.equal(
        as.data.frame(BerekendRes[["Resultaat_globaal"]]),
        as.data.frame(Resultaatv2[["Resultaat_globaal"]])
      )
    )

    BerekendRes2 <-
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken =
            Data_soortenKenmerken %>%
            mutate(
              Vegetatielaag = ifelse(
                Vegetatielaag == "struiklaag",
                "boomlaag",
                Vegetatielaag
              )
            )
        )
      )
    stopifnot(
      all.equal(
        as.data.frame(BerekendRes2[["Resultaat_criterium"]]),
        as.data.frame(Resultaatv2[["Resultaat_criterium"]])
      )
    )
    stopifnot(
      all.equal(
        as.data.frame(BerekendRes[["Resultaat_indicator"]]),
        as.data.frame(Resultaatv2[["Resultaat_indicator"]])
      )
    )
    stopifnot(
      all.equal(
        as.data.frame(BerekendRes2[["Resultaat_detail"]]),
        as.data.frame(Resultaatv2[["Resultaat_detail"]])
      )
    )
    stopifnot(
      all.equal(
        as.data.frame(BerekendRes2[["Resultaat_globaal"]]),
        as.data.frame(Resultaatv2[["Resultaat_globaal"]])
      )
    )

    BerekendRes3 <-
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken =
            Data_soortenKenmerken %>%
            mutate(
              Vegetatielaag = ifelse(
                Vegetatielaag == "struiklaag",
                "kruidlaag",
                .data$Vegetatielaag
              )
            )
        )
      )
    stopifnot(
      all.equal(
        BerekendRes3[["Resultaat_criterium"]],
        Resultaatv2[["Resultaat_criterium"]] %>%
          mutate(
            Index_min_criterium =
              ifelse(
                .data$Criterium == "Vegetatie",
                -0.8163,
                .data$Index_min_criterium
              ),
            Index_harm_criterium =
              ifelse(
                .data$Criterium == "Vegetatie",
                -0.6849,
                .data$Index_harm_criterium
              )
          )
      )
    )
    stopifnot(
      all.equal(
        BerekendRes3[["Resultaat_indicator"]],
        Resultaatv2[["Resultaat_indicator"]] %>%
          mutate(
            Verschilscore =
              ifelse(
                .data$Indicator ==
                  "sleutelsoorten van de boom- en struiklaag",
                0.1057,
                .data$Verschilscore
              ),
            Verschilscore =
              ifelse(
                .data$Indicator ==
                  "sleutelsoorten van de kruidlaag",
                -0.8163,
                .data$Verschilscore
              )
          )
      )
    )
    stopifnot(
      all.equal(
        BerekendRes3[["Resultaat_detail"]],
        Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            Waarde =
              ifelse(
                .data$Voorwaarde ==
                  "grondvlak sleutelsoorten boom- en struiklaag",
                "73.1707317073171",
                .data$Waarde
              ),
            Waarde =
              ifelse(
                .data$Voorwaarde ==
                  "aandeel sleutelsoorten kruidlaag",
                "5.51168326466858",
                .data$Waarde
              ),
            Status_voorwaarde =
              ifelse(
                .data$Voorwaarde ==
                  "aandeel sleutelsoorten kruidlaag",
                FALSE,
                .data$Status_voorwaarde
              ),
            Verschilscore =
              ifelse(
                .data$Voorwaarde ==
                  "grondvlak sleutelsoorten boom- en struiklaag",
                0.1057,
                .data$Verschilscore
              ),
            Verschilscore =
              ifelse(
                .data$Voorwaarde ==
                  "aandeel sleutelsoorten kruidlaag",
                -0.8163,
                .data$Verschilscore
              )
          )
      )
    )
    stopifnot(
      all.equal(
        BerekendRes3[["Resultaat_globaal"]],
        Resultaatv2[["Resultaat_globaal"]] %>%
          mutate(
            Index_min_min = -0.8163,
            Index_min_harm = -0.6142,
            Index_harm_harm = -0.3676
          )
      )
    )
  })

  it("de functie geeft een warning of error als een vegetatielaag ontbreekt", {
    Data_habitat <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Test9190habitat.csv", package = "LSVI"),
        col_types = list(col_character(), col_character())
      )
    attr(Data_habitat, "spec") <- NULL #nolint: object_name_linter
    Data_voorwaarden <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Test9190voorwaarden.csv", package = "LSVI"),
        col_types = list(
          col_character(), col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(), col_character()
        )
      )
    Data_soortenKenmerken <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Test9190soortenKenmerken.csv", package = "LSVI"),
        col_types = list(
          col_character(), col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(), col_character()
        )
      ) %>%
      mutate(
        Vegetatielaag = ifelse(
          .data$Vegetatielaag == "struiklaag",
          NA,
          .data$Vegetatielaag
        )
      )
    expect_error(
      suppressWarnings(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      "Bij Data_soortenKenmerken is niet voor alle soorten de kolom Vegetatielaag ingevuld"  #nolint: line_length_linter
    )

    Data_habitat <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
        col_types = list(col_character(), col_character(), col_character())
      )
    attr(Data_habitat, "spec") <- NULL #nolint: object_name_linter
    Data_voorwaarden <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Opname4030voorwaarden.csv", package = "LSVI"),
        col_types = list(
          col_character(), col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(), col_character()
        )
      )
    Data_soortenKenmerken <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI"),
        col_types = list(
          col_character(), col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(), col_character()
        )
      ) %>%
      mutate(
        Vegetatielaag = ifelse(
          .data$Kenmerk == "Festuca filiformis",
          NA,
          .data$Vegetatielaag
        )
      )
    load(system.file("vbdata/Resultaat_test4030v2.Rdata", package = "LSVI"))
    expect_warning(
      Testresultaat <- idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      "Bij Data_soortenKenmerken is niet voor alle soorten de kolom Vegetatielaag ingevuld"  #nolint: line_length_linter
    )
    expect_equal(
      Testresultaat,
      Resultaatv2
    )
  })

  it("s4-klasse bedekkingLaag werkt correct", {
    Data_habitat <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
        col_types = list(col_character(), col_character(), col_character())
      )
    attr(Data_habitat, "spec") <- NULL #nolint: object_name_linter
    Data_voorwaarden <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Opname4030voorwaarden.csv", package = "LSVI"),
        col_types = list(
          col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(),
          col_character(), col_character()
        )
      )
    Data_soortenKenmerken <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI"),
        col_types = list(
          col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(),
          col_character(), col_character()
        )
      )
    load(system.file("vbdata/Resultaat_test4030v2.Rdata", package = "LSVI"))
    WarningVergrassingVerruiging <-
      "Volgende records uit Data_voorwaarden kunnen niet gekoppeld worden aan indicatoren uit de databank omdat de criterium-indicator-voorwaarde-combinatie niet voorkomt bij de LSVI-regels van het opgegeven habitattype: <JR0216, Verstoring, vergrassing, bedekking vergrassing> <Ts2036, Verstoring, vergrassing, bedekking vergrassing> <JR0216, Verstoring, verruiging, bedekking verruiging> <Ts2036, Verstoring, verruiging, bedekking verruiging> <JR0216, Verstoring, invasieve exoten, bedekking invasieve exoten> <Ts2036, Verstoring, invasieve exoten, bedekking invasieve exoten>" #nolint: line_length_linter
    expect_warning(
      TestResultaat <- idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      WarningVergrassingVerruiging
    )
    expect_equal(
      TestResultaat,
      Resultaatv2
    )
    Data_voorwaarden <- Data_voorwaarden %>% #nolint: object_name_linter
      filter(.data$Voorwaarde != "bedekking verbossing")
    Data_soortenKenmerken1 <- Data_soortenKenmerken %>% #nolint: object_name_linter, line_length_linter
      bind_rows(
        data.frame(
          ID = c("JR0216", "Ts2036"),
          Kenmerk = "boomlaag",
          TypeKenmerk = "studiegroep",
          Waarde = c("35", "7,5"),
          Type = "Percentage",
          Eenheid = "%",
          Vegetatielaag = NA,
          stringsAsFactors = FALSE
        )
      )
    expect_warning(
      Test <-
        idsWissen(
          berekenLSVIbasis(
            Versie = "Versie 2.0", Kwaliteitsniveau = "1",
            Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
            Data_soortenKenmerken = Data_soortenKenmerken1
          )
        ),
      WarningVergrassingVerruiging
    )
    ResultaatBerekening <-
      Resultaatv2[["Resultaat_detail"]] %>%
      mutate(
        AfkomstWaarde = ifelse(
          .data$Indicator == "verbossing",
          "berekend",
          .data$AfkomstWaarde
        ),
        Waarde = ifelse(
          .data$Waarde == "7,5",
          "7.5",
          .data$Waarde
        )
      )
    stopifnot(
      all.equal(
        Test[["Resultaat_detail"]],
        ResultaatBerekening
      )
    )
    Data_soortenKenmerken2 <- Data_soortenKenmerken %>% #nolint: object_name_linter, line_length_linter
      bind_rows(
        data.frame(
          ID = c("JR0216", "Ts2036"),
          Kenmerk = "Quercus robur",
          TypeKenmerk = "soort_Latijn",
          Waarde = c("35", "7,5"),
          Type = "Percentage",
          Eenheid = "%",
          Vegetatielaag = NA,
          stringsAsFactors = FALSE
        )
      )
    expect_error(
      suppressWarnings(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken2
        )
      ),
      "Bij Data_soortenKenmerken is niet voor alle soorten de kolom Vegetatielaag ingevuld"  #nolint: line_length_linter
    )
    Data_soortenKenmerken3 <- Data_soortenKenmerken %>% #nolint: object_name_linter, line_length_linter
      bind_rows(
        data.frame(
          ID = c("JR0216", "Ts2036"),
          Kenmerk = "Quercus robur",
          TypeKenmerk = "soort_Latijn",
          Waarde = c("35", "7,5"),
          Type = "Percentage",
          Eenheid = "%",
          Vegetatielaag = "boomlaag",
          stringsAsFactors = FALSE
        )
      )
    expect_warning(
      Test3 <-
        idsWissen(
          berekenLSVIbasis(
            Versie = "Versie 2.0", Kwaliteitsniveau = "1",
            Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
            Data_soortenKenmerken = Data_soortenKenmerken3
          )
        ),
      WarningVergrassingVerruiging
    )
    stopifnot(
      all.equal(
        Test3[["Resultaat_detail"]],
        ResultaatBerekening
      )
    )
  })

  it("bij verbossing wordt Salix repens niet meegeteld als boom", {
    Data_habitat <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
        col_types = list(col_character(), col_character(), col_character())
      )
    attr(Data_habitat, "spec") <- NULL #nolint: object_name_linter
    Data_voorwaarden <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Opname4030voorwaardenv2.csv", package = "LSVI")
      )
    Data_soortenKenmerken <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI")
      )
    expect_equal(
      suppressWarnings(
        idsWissen(
          berekenLSVIbasis(
            Versie = "Versie 2.0",
            Kwaliteitsniveau = "1",
            Data_habitat,
            Data_voorwaarden %>%
              filter(.data$Voorwaarde != "bedekking verbossing"),
            Data_soortenKenmerken %>%
              bind_rows(
                data.frame(
                  ID = c("JR0216", "Ts2036"),
                  Kenmerk = "Quercus robur",
                  TypeKenmerk = "Soort_Latijn",
                  Waarde = "10",
                  Type = "Percentage",
                  Eenheid = "%",
                  Vegetatielaag = "boomlaag",
                  stringsAsFactors = FALSE
                )
              )
          )
        )
      ),
      suppressWarnings(
        idsWissen(
          berekenLSVIbasis(
            Versie = "Versie 2.0",
            Kwaliteitsniveau = "1",
            Data_habitat,
            Data_voorwaarden %>%
              filter(.data$Voorwaarde != "bedekking verbossing"),
            Data_soortenKenmerken %>%
              bind_rows(
                data.frame(
                  ID = rep(c("JR0216", "Ts2036"), 2),
                  Kenmerk = c(rep("Quercus robur", 2), rep("Salix repens", 2)),
                  TypeKenmerk = "Soort_Latijn",
                  Waarde = "10",
                  Type = "Percentage",
                  Eenheid = "%",
                  Vegetatielaag = "boomlaag",
                  stringsAsFactors = FALSE
                )
              )
          )
        )
      )
    )
  })
  it("correcte berekening t.o.v. vegetatiebedekking bij duinen", {
    Data_habitat <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/data_habitat_2120.csv", package = "LSVI"),
        col_types = list(col_character(), col_character(), col_character())
      )
    attr(Data_habitat, "spec") <- NULL #nolint: object_name_linter
    Data_voorwaarden <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/data_voorwaarden_2120.csv", package = "LSVI"),
        col_types = list(
          col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(), col_character()
        )
      )
    Data_soortenKenmerken <- #nolint: object_name_linter
      read_csv2(
        system.file("vbdata/data_soorten_2120.csv", package = "LSVI"),
        col_types = list(
          col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(),
          col_character(), col_character(), col_character(), col_character()
        )
      )
    load(system.file("vbdata/Resultaat_test4030v2.Rdata", package = "LSVI"))
    Warning <-
      "De waarde\\(n\\) voor de voorwaarde\\(n\\) afwisseling van begroeide en vegetatieloze duinen \\(VoorwaardeID 746\\) kunnen niet berekend worden voor opname\\(n\\) 10010417_2023-08-11, 10010417_2120_2019-10-24, 10038833_2023-08-31, 10071601_2023-08-31. Geef de waarde voor deze voorwaarde rechtstreeks in als input van de functie 'berekenLSVIBasis' via tabel 'Data_voorwaarden' \\(zie \\?berekenLSVIbasis voor meer info\\). Vermeld hierbij Criterium = Structuur, Indicator = naakte bodem en Voorwaarde = afwisseling van begroeide en vegetatieloze duinen." #nolint: line_length_linter
    expect_warning(
      TestResultaat <- berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat = Data_habitat,
        Data_voorwaarden = Data_voorwaarden,
        Data_soortenKenmerken = Data_soortenKenmerken,
        Aggregatiemethode = "RapportageHR",
        na.rm = TRUE
      ),
      Warning
    )
    expect_equal(
      TestResultaat[["Resultaat_detail"]] |>
        filter(Indicator == "kruidlaag") |>
        pull(Waarde),
      c("52", "100", "1.25", "16.9965799156789")
    )
    expect_equal(
      TestResultaat[["Resultaat_detail"]] |>
        filter(Indicator == "(korst)moslaag") |>
        pull(Waarde),
      c("0", "0", "0", "5.38842975206612")
    )
    expect_equal(
      TestResultaat[["Resultaat_detail"]] |>
        filter(
          Voorwaarde == "aantal sleutelsoorten weinig talrijk in begroeiing"
        ) |>
        pull(Waarde),
      c("1", "1", "2", "2")
    )
    expect_equal(
      TestResultaat[["Resultaat_detail"]] |>
        filter(Indicator == "exoten") |>
        pull(Waarde),
      c("20", "100", "5", "7.27")
    )
    expect_equal(
      TestResultaat[["Resultaat_detail"]] |>
        filter(Indicator == "vergrassing") |>
        pull(Waarde),
      c("75.42", "100", "3.7", "10.61")
    )
    expect_equal(
      TestResultaat[["Resultaat_detail"]] |>
        filter(Indicator == "verruiging") |>
        pull(Waarde),
      rep("0", 4)
    )
  })
})

library(pool)
poolClose(ConnectiePool)
