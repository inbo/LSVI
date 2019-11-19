context("test berekenLSVIbasis: ontbreken van soorten of kenmerken")

library(readr)
library(dplyr)
library(rlang)

maakConnectiePool()
Data_habitat <-
    read_csv2(
      system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
      col_types = list(col_character(), col_character(), col_character())
    )
Data_voorwaarden2 <-
  read_csv2(
    system.file("vbdata/Opname4030voorwaardenv2.csv", package = "LSVI")
  )
Data_voorwaarden <-
  read_csv2(
    system.file("vbdata/Opname4030voorwaarden.csv", package = "LSVI")
  )
if (
  class(ConnectiePool$.__enclos_env__$private$createObject())[1] ==
  "SQLiteConnection"
) {
  Data_voorwaarden <- Data_voorwaarden2
}
Data_soortenKenmerken <-
    read_csv2(
      system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI")
    )

load(system.file("vbdata/Resultaat_test4030.Rdata", package = "LSVI"))
load(system.file("vbdata/Resultaat_test4030v2.Rdata", package = "LSVI"))

describe("ontbreken van soorten of kenmerken", {
  it("geen enkele soort opgeven geeft NA en een warning", {
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken %>%
          filter(
            .data$TypeKenmerk != "Soort_Latijn"
          )
      ),
      "Er is geen enkele soort opgegeven"
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken %>%
            filter(
              .data$TypeKenmerk != "Soort_Latijn"
            )
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]] %>%
          mutate(
            Status_criterium =
              ifelse(
                .data$Criterium == "Vegetatie",
                NA,
                .data$Status_criterium
              ),
            Index_min_criterium =
              ifelse(
                .data$Criterium == "Vegetatie",
                NA,
                .data$Index_min_criterium
              ),
            Index_harm_criterium =
              ifelse(
                .data$Criterium == "Vegetatie",
                NA,
                .data$Index_harm_criterium
              )
          ),
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]] %>%
          mutate(
            Status_indicator =
              ifelse(
                .data$Criterium == "Vegetatie",
                NA,
                .data$Status_indicator
              ),
            Verschilscore =
              ifelse(
                .data$Criterium == "Vegetatie",
                NA,
                .data$Verschilscore
              )
          ),
        Resultaat_detail = Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            Waarde =
              ifelse(
                .data$Criterium == "Vegetatie",
                NA,
                .data$Waarde
              ),
            Status_voorwaarde =
              ifelse(
                .data$Criterium == "Vegetatie",
                NA,
                .data$Status_voorwaarde
              ),
            Verschilscore =
              ifelse(
                .data$Criterium == "Vegetatie",
                NA,
                .data$Verschilscore
              )
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]] %>%
          mutate(
            Status =
              ifelse(ID == "Ts2036", NA, .data$Status),
            Index_min_min = as.numeric(NA),
            Index_min_harm = as.numeric(NA),
            Index_harm_harm = as.numeric(NA)
          )
      )
    )
  })

  it("als 1 soort opgegeven is, wordt de bedekking van ontbrekende soorten 0", {
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken %>%
            filter(
              .data$TypeKenmerk != "Soort_Latijn"
            ) %>%
            bind_rows(
              data.frame(
                ID = c("JR0216", "Ts2036"),
                Kenmerk = "Madeliefje",
                TypeKenmerk = "Soort_NL",
                Waarde = "0",
                Type = "Percentage",
                Eenheid = "%",
                Vegetatielaag = "kruidlaag",
                stringsAsFactors = FALSE
              )
            )
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]] %>%
          mutate(
            Status_criterium =
              ifelse(
                .data$Criterium == "Vegetatie",
                FALSE,
                .data$Status_criterium
              ),
            Index_min_criterium =
              ifelse(
                .data$Criterium == "Vegetatie",
                -1,
                .data$Index_min_criterium
              ),
            Index_harm_criterium =
              ifelse(
                .data$Criterium == "Vegetatie",
                -1,
                .data$Index_harm_criterium
              )
          ),
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]] %>%
          mutate(
            Status_indicator =
              ifelse(
                .data$Criterium == "Vegetatie",
                FALSE,
                .data$Status_indicator
              ),
            Verschilscore =
              ifelse(
                .data$Criterium == "Vegetatie",
                -1,
                .data$Verschilscore
              )
          ),
        Resultaat_detail = Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            Waarde =
              ifelse(
                .data$Criterium == "Vegetatie",
                0,
                .data$Waarde
              ),
            Status_voorwaarde =
              ifelse(
                .data$Criterium == "Vegetatie",
                FALSE,
                .data$Status_voorwaarde
              ),
            Verschilscore =
              ifelse(
                .data$Criterium == "Vegetatie",
                -1,
                .data$Verschilscore
              )
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]]
      )
    )
  })

  it("geen enkel kenmerk opgeven geeft NA en een warning", {
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken %>%
          filter(
            .data$TypeKenmerk != "studiegroep"
          )
      ),
      "JR0216, Ts2036 is er geen enkel kenmerk opgegeven van studielijst ouderdomsstadia. Er wordt van uitgegaan dat er voor deze studiegroepen geen observaties uitgevoerd zijn en berekeningen op basis van deze studiegroepen zullen resulteren in NA"  #nolint
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken %>%
            filter(
              .data$TypeKenmerk != "studiegroep"
            )
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]] %>%
          mutate(
            Status_criterium =
              ifelse(
                .data$Criterium == "Structuur" & ID == "Ts2036",
                NA,
                .data$Status_criterium
              ),
            Index_min_criterium =
              ifelse(
                .data$Criterium == "Structuur",
                NA,
                .data$Index_min_criterium
              ),
            Index_harm_criterium =
              ifelse(
                .data$Criterium == "Structuur",
                NA,
                .data$Index_harm_criterium
              )
          ),
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]] %>%
          mutate(
            Status_indicator =
              ifelse(
                .data$Indicator == "ouderdomsstructuur Struikheide",
                NA,
                .data$Status_indicator
              ),
            Verschilscore =
              ifelse(
                .data$Indicator == "ouderdomsstructuur Struikheide",
                NA,
                .data$Verschilscore
              )
          ),
        Resultaat_detail = Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            Waarde =
              ifelse(
                .data$Indicator == "ouderdomsstructuur Struikheide",
                NA,
                .data$Waarde
              ),
            Status_voorwaarde =
              ifelse(
                .data$Indicator == "ouderdomsstructuur Struikheide",
                NA,
                .data$Status_voorwaarde
              ),
            Verschilscore =
              ifelse(
                .data$Indicator == "ouderdomsstructuur Struikheide",
                NA,
                .data$Verschilscore
              )
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]] %>%
          mutate(
            Index_min_min = as.numeric(NA),
            Index_min_harm = as.numeric(NA),
            Index_harm_harm = as.numeric(NA)
          )
      )
    )
  })

  it("als 1 stadium opgegeven is, wordt de bedekking van ontbrekende stadia 0", { #nolint
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken %>%
            filter(
              .data$Waarde != "0"
            )
        )
      ),
      Resultaatv2
    )
  })
})

describe("samenstelling soortengroepen", {
  it("bedekkingen op genusniveau en soortniveau geven hetzelfde resultaat (waar dit mag)", { #nolint
    expect_equal(
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
                Kenmerk = "Quercus",
                TypeKenmerk = "Soort_Latijn",
                Waarde = "10",
                Type = "Percentage",
                Eenheid = "%",
                Vegetatielaag = "boomlaag",
                stringsAsFactors = FALSE
              )
            )
        )
      ),
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
    )
  })
})

library(pool)
poolClose(ConnectiePool)
