context("test bereken Status Rapportage HR")

library(readr)
library(dplyr)
library(rlang)
maakConnectiePool()

Data_habitat <- #nolint
    read_csv2(
      system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
      col_types = list(col_character(), col_character(), col_character())
    )
attr(Data_habitat, "spec") <- NULL
Data_voorwaarden <- #nolint
    read_csv2(system.file("vbdata/Opname4030voorwaarden.csv", package = "LSVI"))
Data_soortenKenmerken <- #nolint
    read_csv2(
      system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI")
    )

load(system.file("vbdata/Resultaat_test4030v2.Rdata", package = "LSVI"))

describe("bereken status criterium en globaal volgens Rapportage HR", {
  it("Status Rapportage HR correct berekend", {
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken,
          Aggregatiemethode = "RapportageHR"
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]] %>%
          mutate(
            Aggregatiemethode = "RapportageHR"
          ),
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]],
        Resultaat_detail = Resultaatv2[["Resultaat_detail"]],
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]] %>%
          mutate(
            Status = ifelse(.data$ID == "Ts2036", TRUE, .data$Status),
            Aggregatiemethode = "RapportageHR"
          )
      )
    )
  })
})


describe("bereken status criterium en globaal volgens Rapportage HR met NA's", {
  it("Correct berekend in geval van NA's voor status van indicatoren", {

    data_voorwaarden_na <- Data_voorwaarden %>%
      mutate(
        Waarde = ifelse(
          .data$Voorwaarde == "bedekking verbossing",
          NA,
          .data$Waarde
          )
      )

    Resultaat <- berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          data_voorwaarden_na,
          Data_soortenKenmerken,
          Aggregatiemethode = "RapportageHR"
        )

    resultaat_negeer_na <- berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          data_voorwaarden_na,
          Data_soortenKenmerken,
          Aggregatiemethode = "RapportageHR",
          na.rm = TRUE
        )

    expect_equal(
      Resultaat$Resultaat_globaal %>%
        select(ID, Status),
      Resultaatv2$Resultaat_globaal %>%
        select(ID, Status) %>%
        mutate(Status = ifelse(.data$ID == "Ts2036", TRUE, .data$Status))
    )

    expect_equal(
      Resultaat$Resultaat_criterium %>%
        select(ID, Criterium, Status_criterium),
      Resultaatv2$Resultaat_criterium %>%
        select(ID, Criterium, Status_criterium) %>%
        mutate(Status_criterium = ifelse(
          .data$Criterium == "Verstoring",
          NA,
          .data$Status_criterium
          )
          )
    )

    expect_equal(
      resultaat_negeer_na$Resultaat_criterium %>%
        select(ID, Criterium, Status_criterium),
      Resultaatv2$Resultaat_criterium %>%
        select(ID, Criterium, Status_criterium) %>%
        mutate(Status_criterium = ifelse(
          .data$Criterium == "Verstoring" & .data$ID == "JR0216",
          TRUE,
          .data$Status_criterium))
    )
  })
})

library(pool)
poolClose(ConnectiePool)
