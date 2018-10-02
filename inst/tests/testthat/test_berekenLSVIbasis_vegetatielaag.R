context("test berekenLSVIbasis vegetatielaag")

library(readr)
library(dplyr)
library(rlang)

describe("berekenLSVIbasis vegetatielaag", {
  it("de vegetatielagen worden correct geselecteerd", {
    skip_on_cran()
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
      ) %>%
      mutate(  #idee is om onderstaande code te wissen zodra de databank/rekenmodule hiervoor aangepast is
        Eenheid =
          ifelse(
            .data$Eenheid == "ha",
            NA,
            .data$Eenheid
          )
      ) %>%
      bind_rows(
        data.frame(
          ID = "1", Criterium = "Vegetatie",
          Indicator = "sleutelsoorten van de kruidlaag",
          Voorwaarde = "aandeel sleutelsoorten kruidlaag", Waarde = "20",
          Type = "Geheel getal",
          stringsAsFactors = FALSE
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

    # Resultaat <-
    #   idsWissen(
    #     berekenLSVIbasis(
    #       Versie = "Versie 3",
    #       Kwaliteitsniveau = "1", Data_habitat,
    #       Data_voorwaarden, Data_soortenKenmerken
    #     )
    #   )
    #
    # save(Resultaat, file = "inst/vbdata/Resultaat_test_bos.Rdata")  #nolint
    # load("inst/vbdata/Resultaat_test_bos.Rdata")  #nolint

    load(system.file("vbdata/Resultaat_test_bos.Rdata", package = "LSVI"))
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      Resultaat
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3", Kwaliteitsniveau = "1",
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
      ),
      Resultaat
    )
    BerekendRes <-
      idsWissen(
        berekenLSVIbasis(
            Versie = "Versie 3", Kwaliteitsniveau = "1",
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
        BerekendRes[["Resultaat_criterium"]],
        Resultaat[["Resultaat_criterium"]] %>%
          mutate(
            Index_min_criterium =
              ifelse(
                .data$Criterium == "Vegetatie",
                -0.61904762,
                .data$Index_min_criterium
              ),
            Index_harm_criterium =
              ifelse(
                .data$Criterium == "Vegetatie",
                -0.39515897,
                .data$Index_harm_criterium
              )
          )
      )
    )
    stopifnot(
      all.equal(
        BerekendRes[["Resultaat_indicator"]],
        Resultaat[["Resultaat_indicator"]] %>%
          mutate(
            Verschilscore =
              ifelse(
                .data$Indicator ==
                  "sleutelsoorten van de boom- en struiklaag",
                -0.61904762,
                .data$Verschilscore
              )
          )
      )
    )
    stopifnot(
      all.equal(
        BerekendRes[["Resultaat_detail"]],
        Resultaat[["Resultaat_detail"]] %>%
          mutate(
            Waarde =
              ifelse(
                .data$Voorwaarde ==
                  "grondvlak sleutelsoorten boom- en struiklaag",
                "26.6666666666667",
                .data$Waarde
              ),
            Verschilscore =
              ifelse(
                .data$Voorwaarde ==
                  "grondvlak sleutelsoorten boom- en struiklaag",
                -0.61904762,
                .data$Verschilscore
              )
          )
      )
    )
    stopifnot(
      all.equal(
        BerekendRes[["Resultaat_globaal"]],
        Resultaat[["Resultaat_globaal"]]
      )
    )
  })

  it("de functie geeft een warning of error als een vegetatielaag ontbreekt", {
    skip_on_cran()
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
      ) %>%
      mutate(  #idee is om onderstaande code te wissen zodra de databank/rekenmodule hiervoor aangepast is
        Eenheid =
          ifelse(
            .data$Eenheid == "ha",
            NA,
            .data$Eenheid
          )
      ) %>%
      bind_rows(
        data.frame(
          ID = "1", Criterium = "Vegetatie",
          Indicator = "sleutelsoorten van de kruidlaag",
          Voorwaarde = "aandeel sleutelsoorten kruidlaag", Waarde = "20",
          Type = "Geheel getal",
          stringsAsFactors = FALSE
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
          Versie = "Versie 3", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      "Bij Data_soortenKenmerken is niet voor alle soorten de kolom Vegetatielaag ingevuld"  #nolint
    )

    Data_habitat <-
      read_csv2(
        system.file("vbdata/opname4030habitat.csv", package = "LSVI"),
        col_types = list(col_character(), col_character(), col_character())
      )
    Data_voorwaarden <-
      read_csv2(
        system.file("vbdata/opname4030voorwaarden.csv", package = "LSVI"),
        col_types =
          list(
            col_character(), col_character(), col_character(), col_character(),
            col_character(), col_character(), col_character(), col_character()
          )
      )
    Data_soortenKenmerken <-
      read_csv2(
        system.file("vbdata/opname4030soortenKenmerken.csv", package = "LSVI"),
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
    load(system.file("vbdata/Resultaat_test.Rdata", package = "LSVI"))
    expect_warning(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      "Bij Data_soortenKenmerken is niet voor alle soorten de kolom Vegetatielaag ingevuld"  #nolint
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      Resultaat
    )
  })

  it("s4-klasse bedekkingLaag werkt correct", {
    skip_on_cran()
    Data_habitat <-
      read_csv2(
        system.file("vbdata/opname4030habitat.csv", package = "LSVI"),
        col_types = list(col_character(), col_character(), col_character())
      )
    Data_voorwaarden <-
      read_csv2(
        system.file("vbdata/opname4030voorwaarden.csv", package = "LSVI"),
        col_types =
          list(
            col_character(), col_character(), col_character(), col_character(),
            col_character(), col_character(), col_character(), col_character()
          )
      )
    Data_soortenKenmerken <-
      read_csv2(
        system.file("vbdata/opname4030soortenKenmerken.csv", package = "LSVI"),
        col_types =
          list(col_character(), col_character(), col_character(),
               col_character(), col_character(), col_character(),
               col_character(), col_character())
      )
    load(system.file("vbdata/Resultaat_test.Rdata", package = "LSVI"))
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken
        )
      ),
      Resultaat
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
          Versie = "Versie 3", Kwaliteitsniveau = "1",
          Data_habitat = Data_habitat, Data_voorwaarden = Data_voorwaarden,
          Data_soortenKenmerken = Data_soortenKenmerken1
        )
      )
    ResultaatBerekening <-
      Resultaat[["Resultaat_detail"]] %>%
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
          Versie = "Versie 3", Kwaliteitsniveau = "1",
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
          Versie = "Versie 3", Kwaliteitsniveau = "1",
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
