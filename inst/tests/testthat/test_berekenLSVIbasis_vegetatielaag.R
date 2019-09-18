context("test berekenLSVIbasis vegetatielaag")

library(readr)
library(dplyr)
library(rlang)
library(stringr)

maakConnectiepoolSQLite()

describe("berekenLSVIbasis vegetatielaag", {
  it("de vegetatielagen worden correct geselecteerd", {
    Data_habitat <-
      read_csv2(
        system.file("vbdata/Test9190habitat.csv", package = "LSVI"),
        col_types = list(col_character(), col_character())
      )
    Data_voorwaarden <-
      read_csv2(
        system.file("vbdata/Test9190voorwaarden.csv", package = "LSVI"),
        col_types =
          list(
            col_character(), col_character(), col_character(), col_character(),
            col_character(), col_character(), col_character(), col_character()
          )
      )
    Data_soortenKenmerken <-
      read_csv2(
        system.file("vbdata/Test9190soortenKenmerken.csv", package = "LSVI"),
        col_types =
          list(col_character(), col_character(), col_character(),
               col_character(), col_character(), col_character(),
               col_character(), col_character())
      )

    Resultaatv2 <-
      list(
        Resultaat_criterium =
          read_csv2(
            system.file(
              "vbdata/Resultaat_test_bosv2/Resultaat_criterium.csv",
              package = "LSVI"
            ),
            col_types =
              list(
                col_character(), col_character(), col_character(),
                col_character(), col_character(), col_integer(), col_logical(),
                col_character(), col_double(), col_double()
              )
          ) %>%
          select(-.data$X1) %>%
          mutate(
            ID = as.character(.data$ID),
            Habitattype = as.character(.data$Habitattype)
          ),
        Resultaat_indicator =
          read_csv2(
            system.file(
              "vbdata/Resultaat_test_bosv2/Resultaat_indicator.csv",
              package = "LSVI"
            ),
            col_types =
              list(
                col_character(), col_character(), col_character(),
                col_character(), col_character(), col_character(),
                col_character(), col_character(), col_character(),
                col_integer(), col_logical(), col_double()
              )
          ) %>%
          select(-.data$X1),
        Resultaat_detail =
          read_csv2(
            system.file(
              "vbdata/Resultaat_test_bosv2/Resultaat_detail.csv",
              package = "LSVI"
            ),
            col_types =
              list(
                col_character(), col_character(), col_character(),
                col_character(), col_character(), col_character(),
                col_character(), col_character(), col_integer(),
                col_character(), col_character(), col_character(),
                col_character(), col_character(), col_character(),
                col_character(), col_character(), col_character(),
                col_character(), col_character(), col_character(), col_double(),
                col_logical(), col_double()
              )
          ) %>%
          select(-.data$X1),
        Resultaat_globaal =
          read_csv2(
            system.file(
              "vbdata/Resultaat_test_bosv2/Resultaat_globaal.csv",
              package = "LSVI"
            ),
            col_types =
              list(
                col_character(), col_character(), col_character(),
                col_character(), col_integer(), col_logical(),
                col_character(), col_double(), col_double(), col_double()
              )
          ) %>%
          select(-.data$X1)
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
              Vegetatielaag =
                ifelse(
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
                Vegetatielaag =
                  ifelse(
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
                -0.842220185,
                .data$Index_min_criterium
              ),
            Index_harm_criterium =
              ifelse(
                .data$Criterium == "Vegetatie",
                -0.723846855,
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
                0.105691057,
                .data$Verschilscore
              ),
            Verschilscore =
              ifelse(
                .data$Indicator ==
                  "sleutelsoorten van de kruidlaag",
                -0.84222018,
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
                "4.73339441538505",
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
                0.105691057,
                .data$Verschilscore
              ),
            Verschilscore =
              ifelse(
                .data$Voorwaarde ==
                  "aandeel sleutelsoorten kruidlaag",
                -0.8422202,
                .data$Verschilscore
              )
          )
      )
    )
    stopifnot(
      all.equal(
        BerekendRes3[["Resultaat_globaal"]],
        Resultaatv2[["Resultaat_globaal"]]
      )
    )
  })

  it("de functie geeft een warning of error als een vegetatielaag ontbreekt", {
    Data_habitat <-
      read_csv2(
        system.file("vbdata/Test9190habitat.csv", package = "LSVI"),
        col_types = list(col_character(), col_character())
      )
    Data_voorwaarden <-
      read_csv2(
        system.file("vbdata/Test9190voorwaarden.csv", package = "LSVI"),
        col_types =
          list(
            col_character(), col_character(), col_character(), col_character(),
            col_character(), col_character(), col_character(), col_character()
          )
      )
    Data_soortenKenmerken <-
      read_csv2(
        system.file("vbdata/Test9190soortenKenmerken.csv", package = "LSVI"),
        col_types =
          list(col_character(), col_character(), col_character(),
               col_character(), col_character(), col_character(),
               col_character(), col_character())
      ) %>%
      mutate(
        Vegetatielaag =
          ifelse(
            .data$Vegetatielaag == "struiklaag",
            NA,
            .data$Vegetatielaag
          )
      )
    expect_error(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      "Bij Data_soortenKenmerken is niet voor alle soorten de kolom Vegetatielaag ingevuld"  #nolint
    )

    Data_habitat <-
      read_csv2(
        system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
        col_types = list(col_character(), col_character(), col_character())
      )
    Data_voorwaarden <-
      read_csv2(
        system.file("vbdata/Opname4030voorwaarden.csv", package = "LSVI"),
        col_types =
          list(
            col_character(), col_character(), col_character(), col_character(),
            col_character(), col_character(), col_character(), col_character()
          )
      )
    Data_soortenKenmerken <-
      read_csv2(
        system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI"),
        col_types =
          list(col_character(), col_character(), col_character(),
               col_character(), col_character(), col_character(),
               col_character(), col_character())
      ) %>%
      mutate(
        Vegetatielaag =
          ifelse(
            .data$Kenmerk == "Festuca filiformis",
            NA,
            .data$Vegetatielaag
          )
      )
    load(system.file("vbdata/Resultaat_test4030v2.Rdata", package = "LSVI"))
    expect_warning(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      "Bij Data_soortenKenmerken is niet voor alle soorten de kolom Vegetatielaag ingevuld"  #nolint
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      Resultaatv2
    )
  })

  it("s4-klasse bedekkingLaag werkt correct", {
    Data_habitat <-
      read_csv2(
        system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
        col_types = list(col_character(), col_character(), col_character())
      )
    if (
      class(ConnectiePool$.__enclos_env__$private$createObject())[1] ==
      "SQLiteConnection"
    ) {
      Data_voorwaarden <-
        read_csv2(
          system.file("vbdata/Opname4030voorwaardenv2.csv", package = "LSVI"),
          col_types =
            list(
              col_character(), col_character(), col_character(),
              col_character(), col_character(), col_character(),
              col_character(), col_character()
            )
        )
    } else {
      Data_voorwaarden <-
        read_csv2(
          system.file("vbdata/Opname4030voorwaarden.csv", package = "LSVI"),
          col_types =
            list(
              col_character(), col_character(), col_character(),
              col_character(), col_character(), col_character(),
              col_character(), col_character()
            )
        )
    }
    Data_soortenKenmerken <-
      read_csv2(
        system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI"),
        col_types =
          list(col_character(), col_character(), col_character(),
               col_character(), col_character(), col_character(),
               col_character(), col_character())
      )
    load(system.file("vbdata/Resultaat_test4030v2.Rdata", package = "LSVI"))
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      Resultaatv2
    )
    Data_voorwaarden <- Data_voorwaarden %>%
      filter(.data$Voorwaarde != "bedekking verbossing")
    Data_soortenKenmerken1 <- Data_soortenKenmerken %>%
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
    Test <-
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken1
        )
      )
    ResultaatBerekening <-
      Resultaatv2[["Resultaat_detail"]] %>%
      mutate(
        AfkomstWaarde =
          ifelse(
            .data$Indicator == "verbossing",
            "berekend",
            .data$AfkomstWaarde
          ),
        Waarde =
          ifelse(
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
    Data_soortenKenmerken2 <- Data_soortenKenmerken %>%
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
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken2
        )
      ),
      "Bij Data_soortenKenmerken is niet voor alle soorten de kolom Vegetatielaag ingevuld"  #nolint
    )
    Data_soortenKenmerken3 <- Data_soortenKenmerken %>%
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
    Test3 <-
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken3
        )
      )
    stopifnot(
      all.equal(
        Test3[["Resultaat_detail"]],
        ResultaatBerekening
      )
    )
  })
})

library(pool)
poolClose(ConnectiePool)
