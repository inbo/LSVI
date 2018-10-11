context("test berekenLSVIbasis")

library(readr)
library(dplyr)
library(rlang)

Data_habitat <-
    read_csv2(
      system.file("vbdata/opname4030habitat.csv", package = "LSVI"),
      col_types = list(col_character(), col_character(), col_character())
    )
Data_voorwaarden <-
    read_csv2(system.file("vbdata/opname4030voorwaarden.csv", package = "LSVI"))
Data_soortenKenmerken <-
    read_csv2(
      system.file("vbdata/opname4030soortenKenmerken.csv", package = "LSVI")
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
# save(Resultaat, file = "inst/vbdata/Resultaat_test.Rdata")  #nolint
# load("inst/vbdata/Resultaat_test.Rdata")  #nolint

load(system.file("vbdata/Resultaat_test.Rdata", package = "LSVI"))

describe("berekenLSVIbasis", {
  it("ConnectieLSVIhabitats is een open DBI-connectie", {
    skip_on_cran()
    expect_error(
      berekenLSVIbasis(
        ConnectieLSVIhabitats = "geenConnectie",
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken
      ),
      "Er is geen connectie met de databank met de LSVI-indicatoren"
    )
    ConnectieLSVIhabitats <-
      connecteerMetLSVIdb()
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          ConnectieLSVIhabitats = ConnectieLSVIhabitats,
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )
      ),
      Resultaat
    )
  })

  it("parameter versie heeft correct formaat", {
    skip_on_cran()
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )
      ),
      Resultaat
    )
    expect_error(
      berekenLSVIbasis(
        Versie = 2,
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken
      ),
      "Versie is not a string"
    )
  })

  it("parameter kwaliteitsniveau heeft correct formaat", {
    skip_on_cran()
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )
      ),
      Resultaat
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = 1,
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )
      ),
      Resultaat
    )
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "streefwaarde",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken
      ),
      "Kwaliteitsniveau moet een van de volgende waarden zijn"
    )
  })

  it("dataframe Data_habitat heeft correct formaat", {
    skip_on_cran()
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat %>%
          bind_rows(
            data.frame(
              ID = "fouttest",
              Habitattype = "onbestaand",
              stringsAsFactors = FALSE
            )
          ),
        Data_voorwaarden,
        Data_soortenKenmerken
      )
    )
  })

  it("dataframe Data_voorwaarden heeft correct formaat", {
    skip_on_cran()
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden %>%
          mutate(
            Waarde =
              ifelse(.data$Waarde == 35, "drie", .data$Waarde)
          ),
        Data_soortenKenmerken
    ),
      "Niet alle opgegeven getallen en percentages zijn numerieke waarden." #nolint
    )
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden %>%
          mutate(
            Waarde =
              ifelse(.data$Waarde == "35", "-35", .data$Waarde)
          ),
        Data_soortenKenmerken
      ),
      "Niet alle opgegeven getallen en percentages zijn positieve waarden"
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            mutate(
              Waarde =
                ifelse(.data$Waarde == "7,5", NA, .data$Waarde)
            ),
          Data_soortenKenmerken
        )
      ),
      list(
        Resultaat_criterium =
          Resultaat[["Resultaat_criterium"]] %>%
          mutate(
            Status_criterium =
              ifelse(
                .data$Criterium == "Verstoring" & .data$ID == "Ts2036",
                NA,
                .data$Status_criterium
              ),
            Index_min_criterium =
              ifelse(
                .data$Criterium == "Verstoring" & .data$ID == "Ts2036",
                NA,
                .data$Index_min_criterium
              ),
            Index_harm_criterium =
              ifelse(
                .data$Criterium == "Verstoring" & .data$ID == "Ts2036",
                NA,
                .data$Index_harm_criterium
              )
          ),
        Resultaat_indicator =
          Resultaat[["Resultaat_indicator"]] %>%
          mutate(
            Status_indicator =
              ifelse(
                .data$Indicator == "verbossing" & .data$ID == "Ts2036",
                NA,
                .data$Status_indicator
              ),
            Verschilscore =
              ifelse(
                .data$Indicator == "verbossing" & .data$ID == "Ts2036",
                NA,
                .data$Verschilscore
              )
          ),
        Resultaat_detail =
          Resultaat[["Resultaat_detail"]] %>%
          mutate(
            Waarde = ifelse(.data$Waarde == "7,5", NA, .data$Waarde),
            Status_voorwaarde =
              ifelse(
                .data$Indicator == "verbossing" & .data$ID == "Ts2036",
                NA,
                .data$Status_voorwaarde
              ),
            Verschilscore =
              ifelse(
                .data$Indicator == "verbossing" & .data$ID == "Ts2036",
                NA,
                .data$Verschilscore
              )
          ),
        Resultaat_globaal = Resultaat[["Resultaat_globaal"]]
      )
    )
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden %>%
          mutate(
            Waarde =
              ifelse(.data$Waarde == "f", "foute invoer", .data$Waarde)
          ),
        Data_soortenKenmerken
      ),
      "Niet voor elke opgegeven categorische variabele is er een numerieke waarde opgenomen in de databank"   #nolint
    )
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden %>%
          mutate(
            Waarde =
              ifelse(.data$Waarde == "f", 2, .data$Waarde)
          ),
        Data_soortenKenmerken
      ),
      "Niet voor elke opgegeven categorische variabele is er een numerieke waarde opgenomen in de databank"   #nolint
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            mutate(
              Waarde =
                ifelse(.data$Waarde == "f", "F", .data$Waarde)
            ),
          Data_soortenKenmerken
        )
      ),
      list(
        Resultaat_criterium = Resultaat[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaat[["Resultaat_indicator"]],
        Resultaat_detail =
          Resultaat[["Resultaat_detail"]] %>%
          mutate(
            Waarde =
              ifelse(.data$Waarde == "f", "F", .data$Waarde)
          ),
        Resultaat_globaal = Resultaat[["Resultaat_globaal"]]))

    Resultaat_NA <- idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            mutate(
              Waarde =
                ifelse(.data$Waarde == "f", NA, .data$Waarde)
            ),
          Data_soortenKenmerken
        ))

    Indicator_vergrassing <- Resultaat_NA[["Resultaat_indicator"]] %>%
      filter(.data$Indicator == "vergrassing" & .data$ID == "Ts2036") %>%
      select(Indicator, Status_indicator)

    Criterium_verstoring <- Resultaat_NA[["Resultaat_criterium"]] %>%
      filter(.data$Criterium == "Verstoring" & .data$ID == "Ts2036") %>%
      select(Criterium, Status_criterium)

    Status_globaal <- Resultaat_NA[["Resultaat_globaal"]] %>%
      filter( .data$ID == "Ts2036")

    expect_equal(
      Indicator_vergrassing$Status_indicator,
      NA
      )

    expect_equal(
      Criterium_verstoring$Status_criterium,
      NA
      )

    expect_equal(
      Status_globaal$Status,
      FALSE
      )


  })

  it("parameter kwaliteitsniveau heeft correct formaat", {
    skip_on_cran()
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )
      ),
      Resultaat
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = 1,
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )
      ),
      Resultaat
    )
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "streefwaarde",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken
      ),
      "Kwaliteitsniveau moet een van de volgende waarden zijn"
    )
  })

  it("dataframe Data_habitat heeft correct formaat", {
    skip_on_cran()
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat %>%
          bind_rows(
            data.frame(
              ID = "fouttest",
              Habitattype = "onbestaand",
              stringsAsFactors = FALSE
            )
          ),
        Data_voorwaarden,
        Data_soortenKenmerken
      )
    )
  })

  it("een interval wordt correct omgezet", {
    skip_on_cran()
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            mutate(
              Type =
                ifelse(.data$Waarde == "f", "Percentage", .data$Type),
              Invoertype =
                ifelse(.data$Waarde == "f", NA, .data$Invoertype),
              Waarde =
                ifelse(.data$Waarde == "f", "5-10", .data$Waarde)
            ),
          Data_soortenKenmerken
        )
      ),
      list(
        Resultaat_criterium = Resultaat[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaat[["Resultaat_indicator"]],
        Resultaat_detail =
          Resultaat[["Resultaat_detail"]] %>%
          mutate(
            TypeWaarde =
              ifelse(.data$Waarde == "f", "Percentage", .data$TypeWaarde),
            InvoertypeWaarde =
              ifelse(.data$Waarde == "f", NA, .data$InvoertypeWaarde),
            TheoretischMaximum =
              ifelse(
                .data$Waarde == "f" & .data$TypeRefwaarde == "Percentage",
                100, .data$TheoretischMaximum
              ),
            Waarde =
              ifelse(.data$Waarde == "f", "5-10", .data$Waarde)
          ),
        Resultaat_globaal = Resultaat[["Resultaat_globaal"]])
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            mutate(
              Type =
                ifelse(.data$Waarde == "f", "Percentage", .data$Type),
              Invoertype =
                ifelse(.data$Waarde == "f", NA, .data$Invoertype),
              Waarde =
                ifelse(.data$Waarde == "f", "5 - 10", .data$Waarde)
            ),
          Data_soortenKenmerken
        )
      ),
      list(
        Resultaat_criterium = Resultaat[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaat[["Resultaat_indicator"]],
        Resultaat_detail =
          Resultaat[["Resultaat_detail"]] %>%
          mutate(
            TypeWaarde =
              ifelse(.data$Waarde == "f", "Percentage", .data$TypeWaarde),
            InvoertypeWaarde =
              ifelse(.data$Waarde == "f", NA, .data$InvoertypeWaarde),
            TheoretischMaximum =
              ifelse(
                .data$Waarde == "f" & .data$TypeRefwaarde == "Percentage",
                100, .data$TheoretischMaximum
              ),
            Waarde =
              ifelse(.data$Waarde == "f", "5 - 10", .data$Waarde)
          ),
        Resultaat_globaal = Resultaat[["Resultaat_globaal"]])
    )
  })

  it("dataframe Data_soortenKenmerken heeft correct formaat", {
    skip_on_cran()
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken %>%
          mutate(
            Waarde =
              ifelse(
                .data$Waarde == "f",
                "foute invoer",
                .data$Waarde
              )
          )
      ),
      "Niet voor elke opgegeven categorische variabele is er een numerieke waarde opgenomen in de databank"  #nolint
    )
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken %>%
          mutate(
            Waarde =
              ifelse(
                .data$Waarde == "f",
                11,
                .data$Waarde
              )
          )
      ),
      "Niet voor elke opgegeven categorische variabele is er een numerieke waarde opgenomen in de databank" #nolint
    )
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken %>%
          mutate(
            Waarde =
              ifelse(
                .data$Waarde == 0,
                "foute invoer",
                .data$Waarde
              )
          )
      ),
      "Niet alle opgegeven getallen en percentages zijn numerieke waarden."
    )
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken %>%
          mutate(
            Kenmerk =
              ifelse(
                .data$Kenmerk == "Calluna vulgaris",
                "Calla vulgaris",
                .data$Kenmerk
              )
          )
      ),
      "Volgende soortnamen zijn niet teruggevonden in de databank"
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken %>%
            mutate(
              Kenmerk =
                ifelse(
                  .data$Kenmerk == "Calluna vulgaris",
                  "Struikhei",
                  .data$Kenmerk
                ),
              TypeKenmerk =
                ifelse(
                  .data$Kenmerk == "Struikhei",
                  "soort_nl",
                  .data$TypeKenmerk
                )
            )
        )
      ),
      Resultaat
    )
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken %>%
          mutate(
            TypeKenmerk =
              ifelse(
                .data$Kenmerk == "Calluna vulgaris",
                "soort_NL",
                .data$TypeKenmerk
              )
          )
      ),
      "Volgende soortnamen zijn niet teruggevonden in de databank"
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken %>%
            mutate(
              Kenmerk =
                ifelse(
                  .data$Kenmerk == "Calluna vulgaris",
                  "NBNSYS0000003902",
                  .data$Kenmerk
                ),
              TypeKenmerk =
                ifelse(
                  .data$Kenmerk == "NBNSYS0000003902",
                  "soort_nbn",
                  .data$TypeKenmerk
                )
            )
        )
      ),
      Resultaat
    )
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken %>%
          mutate(
            TypeKenmerk =
              ifelse(
                .data$Kenmerk == "Calluna vulgaris",
                "soort_nbn",
                .data$TypeKenmerk
              )
          )
      ),
      "Volgende NBNTaxonVersionKeys zijn niet teruggevonden in de databank"
    )
  })

  it("afhandeling van Ja/nee in Data_soortenKenmerken is correct", {
    skip_on_cran()
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken %>%
          mutate(
            Waarde =
              ifelse(
                .data$Waarde == "0" & ID == "Ts2036",
                "1",
                .data$Waarde
              ),
            Type =
              ifelse(
                .data$Type == "Percentage" & ID == "Ts2036",
                "Ja/nee",
                .data$Type
              )
          )
      ),
      "Voor sommige soorten of kenmerken is enkel aan- of afwezigheid opgegeven, geen bedekking,"  #nolint
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken %>%
            mutate(
              Waarde =
                ifelse(
                  .data$Waarde == "0" & ID == "Ts2036",
                  "1",
                  .data$Waarde
                ),
              Type =
                ifelse(
                  .data$Type == "Percentage" & ID == "Ts2036",
                  "Ja/nee",
                  .data$Type
                )
            )
        )
      ),
      list(
        Resultaat_criterium = Resultaat[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaat[["Resultaat_indicator"]] %>%
          mutate(
            Verschilscore =
              ifelse(
                .data$Criterium == "Structuur" & ID == "Ts2036" &
                  Indicator == "ouderdomsstructuur Struikheide",
                0.5,
                .data$Verschilscore
              )
          ),
        Resultaat_detail = Resultaat[["Resultaat_detail"]] %>%
          mutate(
            Waarde =
              ifelse(
                .data$Criterium == "Structuur" & ID == "Ts2036" &
                  Referentiewaarde == "1",
                "1 - 2",
                .data$Waarde
              ),
            Waarde =
              ifelse(
                .data$Criterium == "Structuur" & ID == "Ts2036" &
                  Referentiewaarde == "2",
                "3 - 4",
                .data$Waarde
              ),
            Verschilscore =
              ifelse(
                .data$Criterium == "Structuur" & ID == "Ts2036" &
                  Referentiewaarde == "1",
                0.5,
                .data$Verschilscore
              ),
            Verschilscore =
              ifelse(
                .data$Criterium == "Structuur" & ID == "Ts2036" &
                  Referentiewaarde == "2",
                0.75,
                .data$Verschilscore
              )
          ),
        Resultaat_globaal = Resultaat[["Resultaat_globaal"]]
      )
    )
  })

  it("De afhandeling van taxa en subtaxa gebeurt correct", {
    skip_on_cran()
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            filter(
              !(.data$ID == "JR0216" & .data$Indicator == "verruiging")
            ),
          Data_soortenKenmerken %>%
            bind_rows(
              data.frame(
                ID = "JR0216",
                Kenmerk = "Rubus",
                TypeKenmerk = "soort_Latijn",
                Waarde = "35",
                Type = "Percentage",
                Eenheid = "%",
                stringsAsFactors = FALSE
              )
            )
        )
      ),
      list(
        Resultaat_criterium = Resultaat[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaat[["Resultaat_indicator"]],
        Resultaat_detail = Resultaat[["Resultaat_detail"]] %>%
          mutate(
            AfkomstWaarde =
              ifelse(
                .data$ID == "JR0216" & .data$Indicator == "verruiging",
                "berekend",
                .data$AfkomstWaarde
              )
          ),
        Resultaat_globaal = Resultaat[["Resultaat_globaal"]]
      )
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            filter(
              !(.data$ID == "JR0216" & .data$Indicator == "verruiging")
            ),
          Data_soortenKenmerken %>%
            bind_rows(
              data.frame(
                ID = "JR0216",
                Kenmerk = "Rubus fruticosus",
                TypeKenmerk = "soort_Latijn",
                Waarde = "35",
                Type = "Percentage",
                Eenheid = "%",
                stringsAsFactors = FALSE
              )
            )
        )
      ),
      list(
        Resultaat_criterium = Resultaat[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaat[["Resultaat_indicator"]],
        Resultaat_detail = Resultaat[["Resultaat_detail"]] %>%
          mutate(
            AfkomstWaarde =
              ifelse(
                .data$ID == "JR0216" & .data$Indicator == "verruiging",
                "berekend",
                .data$AfkomstWaarde
              )
          ),
        Resultaat_globaal = Resultaat[["Resultaat_globaal"]]
      )
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 3",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            filter(
              !(.data$ID == "JR0216" & .data$Indicator == "verruiging")
            ),
          Data_soortenKenmerken %>%
            bind_rows(
              data.frame(
                ID = "JR0216",
                Kenmerk = c("Rubus", "Rubus fruticosus"),
                TypeKenmerk = "soort_Latijn",
                Waarde = "35",
                Type = "Percentage",
                Eenheid = "%",
                stringsAsFactors = FALSE
              )
            )
        )
      ),
      list(
        Resultaat_criterium = Resultaat[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaat[["Resultaat_indicator"]],
        Resultaat_detail = Resultaat[["Resultaat_detail"]] %>%
          mutate(
            AfkomstWaarde =
              ifelse(
                .data$ID == "JR0216" & .data$Indicator == "verruiging",
                "berekend",
                .data$AfkomstWaarde
              )
          ),
        Resultaat_globaal = Resultaat[["Resultaat_globaal"]]
      )
    )
  })

})


#werking childID nog testen!
