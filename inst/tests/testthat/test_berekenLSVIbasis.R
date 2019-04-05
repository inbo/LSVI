context("test berekenLSVIbasis")

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

# Resultaat <-
#   idsWissen(
#     berekenLSVIbasis(
#       Versie = "Versie 3",
#       Kwaliteitsniveau = "1", Data_habitat,
#       Data_voorwaarden, Data_soortenKenmerken
#     )
#   )
# 
# save(Resultaat, file = "inst/vbdata/Resultaat_test4030.Rdata")  #nolint
# load("inst/vbdata/Resultaat_test4030.Rdata")  #nolint

# Resultaatv2 <-
#   idsWissen(
#     berekenLSVIbasis(
#       Versie = "Versie 2.0",
#       Kwaliteitsniveau = "1", Data_habitat,
#       Data_voorwaarden, Data_soortenKenmerken
#     )
#   )
# 
# save(Resultaatv2, file = "inst/vbdata/Resultaat_test4030v2.Rdata")  #nolint
# load("inst/vbdata/Resultaat_test4030v2.Rdata")  #nolint

load(system.file("vbdata/Resultaat_test4030.Rdata", package = "LSVI"))
load(system.file("vbdata/Resultaat_test4030v2.Rdata", package = "LSVI"))

describe("berekenLSVIbasis", {
  it("ConnectieLSVIhabitats is een open DBI-connectie", {
    expect_error(
      berekenLSVIbasis(
        ConnectieLSVIhabitats = "geenConnectie",
        Versie = "Versie 2.0",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken
      ),
      "Er is geen connectie met de databank met de LSVI-indicatoren"
    )
    ConnectieLSVIhabitats <-
      connecteerMetLSVIlite()
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          ConnectieLSVIhabitats = ConnectieLSVIhabitats,
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )
      ),
      Resultaatv2
    )
  })

  it("parameter versie heeft correct formaat", {
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )
      ),
      Resultaatv2
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
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )
      ),
      Resultaatv2
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = 1,
          Data_habitat,
          Data_voorwaarden,
          Data_soortenKenmerken
        )
      ),
      Resultaatv2
    )
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = "streefwaarde",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken
      ),
      "Kwaliteitsniveau moet een van de volgende waarden zijn"
    )
  })

  it("dataframe Data_habitat heeft correct formaat", {
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
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
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden %>%
          mutate(
            Waarde =
              ifelse(.data$Waarde == 35, "drie", .data$Waarde)
          ),
        Data_soortenKenmerken
    ),
      "Niet alle opgegeven getallen en percentages zijn numerieke waarden" #nolint
    )
    expect_error(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
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
          Versie = "Versie 2.0",
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
          Resultaatv2[["Resultaat_criterium"]] %>%
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
          Resultaatv2[["Resultaat_indicator"]] %>%
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
          Resultaatv2[["Resultaat_detail"]] %>%
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
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]] %>%
          mutate(
            Index_min_min =
              ifelse(
                .data$ID == "Ts2036",
                NA,
                Index_min_min
              ),
            Index_min_harm =
              ifelse(
                .data$ID == "Ts2036",
                NA,
                Index_min_harm
              ),
            Index_harm_harm =
              ifelse(
                .data$ID == "Ts2036",
                NA,
                Index_harm_harm
              )
          )
      )
    )
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
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
        Versie = "Versie 2.0",
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
          Versie = "Versie 2.0",
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
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]],
        Resultaat_detail =
          Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            Waarde =
              ifelse(.data$Waarde == "f", "F", .data$Waarde)
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]]))
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            mutate(
              Waarde =
                ifelse(.data$Waarde == "f", NA, .data$Waarde)
            ),
          Data_soortenKenmerken
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]] %>%
          mutate(
            Index_min_criterium =
              ifelse(
                .data$Criterium == "Structuur" & .data$ID == "JR0216",
                NA,
                .data$Index_min_criterium
              ),
            Index_harm_criterium =
              ifelse(
                .data$Criterium == "Structuur" & .data$ID == "JR0216",
                NA,
                .data$Index_harm_criterium
              )
          ),
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]] %>%
          mutate(
            Status_indicator =
              ifelse(
                .data$Indicator == "dwergstruiken" & .data$ID == "JR0216",
                NA,
                .data$Status_indicator
              ),
            Verschilscore =
              ifelse(
                .data$Indicator == "dwergstruiken" & .data$ID == "JR0216",
                NA,
                .data$Verschilscore
              )
          ),
        Resultaat_detail =
          Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            Status_voorwaarde =
              ifelse(.data$Waarde == "f", NA, .data$Status_voorwaarde),
            Waarde =
              ifelse(.data$Waarde == "f", NA, .data$Waarde),
            Verschilscore =
              ifelse(.data$Waarde == "f", NA, .data$Verschilscore)
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]] %>%
          mutate(
            Index_min_min =
              ifelse(
                .data$ID == "JR0216",
                NA,
                .data$Index_min_min
              ),
            Index_min_harm =
              ifelse(
                .data$ID == "JR0216",
                NA,
                .data$Index_min_harm
              ),
            Index_harm_harm =
              ifelse(
                .data$ID == "JR0216",
                NA,
                .data$Index_harm_harm
              )
          )))
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden2 %>%
          mutate(
            Indicator =
              ifelse(
                .data$Indicator == "verbossing",
                "dwergstruiken",
                .data$Indicator
              )
          ),
        Data_soortenKenmerken
      ),
      "Volgende records uit Data_voorwaarden kunnen niet gekoppeld worden aan indicatoren uit de databank omdat de criterium-indicator-voorwaarde-combinatie niet voorkomt bij de LSVI-regels van het opgegeven habitattype:" #nolint
    )
  })

  it("functie werkt zonder opgave Data_voorwaarden", {
    Data_soortenKenmerken2 <-
      read_csv2(
        system.file(
          "vbdata/Opname4030soortenKenmerkenv2tot.csv",
          package = "LSVI"
        )
      )
    Resultaat_berekening <-
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_soortenKenmerken = Data_soortenKenmerken2
        )
      )
    stopifnot(
      all.equal(
        Resultaat_berekening[["Resultaat_criterium"]],
        Resultaatv2[["Resultaat_criterium"]]
      )
    )
    stopifnot(
      all.equal(
        Resultaat_berekening[["Resultaat_indicator"]],
        Resultaatv2[["Resultaat_indicator"]]
      )
    )
    Resultaatv2detail <- Resultaatv2[["Resultaat_detail"]]
    Resultaatv2detail <-
      Resultaatv2detail[
        shuffle_columns(
          names(Resultaatv2detail),
          "AfkomstWaarde before EenheidWaarde"
        )
      ]
    Resultaatv2detail <-
      Resultaatv2detail[
        shuffle_columns(
          names(Resultaatv2detail),
          "AfkomstWaarde before InvoertypeWaarde"
        )
      ]
    Resultaatv2detail <-
      Resultaatv2detail[
        shuffle_columns(
          names(Resultaatv2detail),
          "TheoretischMaximum before EenheidWaarde"
        )
        ]
    Resultaatv2detail <-
      Resultaatv2detail[
        shuffle_columns(
          names(Resultaatv2detail),
          "TheoretischMaximum before InvoertypeWaarde"
        )
        ]
    stopifnot(
      all.equal(
        Resultaat_berekening[["Resultaat_detail"]],
        Resultaatv2detail %>%
          mutate(
            AfkomstWaarde = "berekend",
            TypeWaarde =
              ifelse(
                .data$Waarde == "o" & .data$Criterium == "Verstoring",
                "Percentage",
                .data$TypeWaarde
              ),
            InvoertypeWaarde =
              ifelse(
                .data$Waarde == "o" & .data$Criterium == "Verstoring",
                NA,
                .data$InvoertypeWaarde
              ),
            EenheidWaarde =
              ifelse(
                .data$Waarde == "o" & .data$Criterium == "Verstoring",
                "%",
                .data$EenheidWaarde
              ),
            TheoretischMaximum =
              ifelse(
                .data$Waarde == "o" & .data$Criterium == "Verstoring",
                100,
                .data$TheoretischMaximum
              ),
            Waarde =
              ifelse(
                .data$Waarde == "o" & .data$Criterium == "Verstoring",
                "2 - 5",
                .data$Waarde
              ),
            TypeWaarde =
              ifelse(
                .data$Waarde == "cd" & .data$Criterium == "Verstoring",
                "Percentage",
                .data$TypeWaarde
              ),
            InvoertypeWaarde =
              ifelse(
                .data$Waarde == "cd" & .data$Criterium == "Verstoring",
                NA,
                .data$InvoertypeWaarde
              ),
            EenheidWaarde =
              ifelse(
                .data$Waarde == "cd" & .data$Criterium == "Verstoring",
                "%",
                .data$EenheidWaarde
              ),
            TheoretischMaximum =
              ifelse(
                .data$Waarde == "cd" & .data$Criterium == "Verstoring",
                100,
                .data$TheoretischMaximum
              ),
            Waarde =
              ifelse(
                .data$Waarde == "cd" & .data$Criterium == "Verstoring",
                "25 - 50",
                .data$Waarde
              ),
            InvoertypeWaarde =
              ifelse(
                .data$InvoertypeWaarde == "Tansley IHD",
                "TANSLEY IHD",
                .data$InvoertypeWaarde
              ),
            Waarde =
              ifelse(
                .data$Waarde == "7,5",
                "7.5",
                .data$Waarde
              )
          )
      )
    )
  })

  it("een interval wordt correct omgezet", {
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
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
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]],
        Resultaat_detail =
          Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            TypeWaarde =
              ifelse(.data$Waarde == "f", "Percentage", .data$TypeWaarde),
            InvoertypeWaarde =
              ifelse(.data$Waarde == "f", NA, .data$InvoertypeWaarde),
            TheoretischMaximum =
              ifelse(
                .data$Waarde == "f" & .data$TypeWaarde == "Percentage",
                100, .data$TheoretischMaximum
              ),
            Waarde =
              ifelse(.data$Waarde == "f", "5-10", .data$Waarde)
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]])
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
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
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]],
        Resultaat_detail =
          Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            TypeWaarde =
              ifelse(.data$Waarde == "f", "Percentage", .data$TypeWaarde),
            InvoertypeWaarde =
              ifelse(.data$Waarde == "f", NA, .data$InvoertypeWaarde),
            TheoretischMaximum =
              ifelse(
                .data$Waarde == "f" & .data$TypeWaarde == "Percentage",
                100, .data$TheoretischMaximum
              ),
            Waarde =
              ifelse(.data$Waarde == "f", "5 - 10", .data$Waarde)
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]])
    )
  })

  it("dataframe Data_soortenKenmerken heeft correct formaat", {
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
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
        Versie = "Versie 2.0",
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
        Versie = "Versie 2.0",
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
      "Niet alle opgegeven getallen en percentages zijn numerieke waarden"
    )
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
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
          Versie = "Versie 2.0",
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
      Resultaatv2
    )
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
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
          Versie = "Versie 2.0",
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
      Resultaatv2
    )
    expect_warning(
      berekenLSVIbasis(
        Versie = "Versie 2.0",
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

  it("Berekening gebeurt correct zonder opgave Data_soortenKenmerken", {
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden =
            Data_voorwaarden2 %>%
            bind_rows(
              data.frame(
                ID = rep(c("JR0216", "Ts2036"), 3),
                Criterium =
                  c(
                    rep("Structuur", 2),
                    rep("Vegetatie", 4)
                  ),
                Indicator =
                  c(
                    rep("ouderdomsstructuur Struikheide", 2),
                    rep("sleutelsoorten", 4)
                  ),
                Voorwaarde =
                  c(
                    rep("aantal ouderdomsstadia", 2),
                    rep("aanwezigheid struikheide", 2),
                    rep("aantal sleutelsoorten", 2)
                  ),
                Waarde = c("1", "3", "1", "1", rep("0", 2)),
                Type = "Geheel getal",
                Invoertype = NA,
                Eenheid = NA,
                stringsAsFactors = FALSE
              )
            )
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]] %>%
          mutate(
            Index_min_criterium =
              ifelse(
                .data$Criterium == "Vegetatie",
                NA,
                .data$Index_min_criterium
              ),
            Index_min_criterium =
              ifelse(
                .data$Criterium == "Structuur" & .data$ID == "Ts2036",
                NA,
                .data$Index_min_criterium
              ),
            Index_harm_criterium =
              ifelse(
                is.na(.data$Index_min_criterium),
                NA,
                .data$Index_harm_criterium
              )
          ),
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]] %>%
          mutate(
            Verschilscore =
              ifelse(
                .data$Criterium == "Vegetatie",
                NA,
                .data$Verschilscore
              ),
            Verschilscore =
              ifelse(
                .data$Indicator == "ouderdomsstructuur Struikheide" &
                  .data$ID == "Ts2036",
                NA,
                .data$Verschilscore
              )
          ),
        Resultaat_detail =
          Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            TheoretischMaximum =
              ifelse(
                .data$AfkomstWaarde == "berekend",
                NA,
                .data$TheoretischMaximum
              ),
            Verschilscore =
              ifelse(
                .data$Voorwaarde == "aanwezigheid struikheide",
                NA,
                .data$Verschilscore
              ),
            Verschilscore =
              ifelse(
                .data$Waarde == "3",
                NA,
                .data$Verschilscore
              ),
            AfkomstWaarde = "observatie"
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

  it("afhandeling van Ja/nee in Data_soortenKenmerken is correct", {
    skip_if_not(
      class(ConnectiePool$.__enclos_env__$private$createObject())[1] ==
        "Microsoft SQL Server",
      "SQL Server niet beschikbaar"
    )
    expect_warning(
      BerekendRes <-
        idsWissen(
          berekenLSVIbasis(
            Versie = "Versie 3",
            Kwaliteitsniveau = "1",
            Data_habitat,
            Data_voorwaarden %>%
              filter(.data$Indicator != "vergrassing/verruiging"),
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
      "is enkel aan- of afwezigheid opgegeven, geen bedekking. Hierdoor kon het aantal soorten dat aan een welbepaalde voorwaarde voldoet"  #nolint
    )
    stopifnot(
      all.equal(
        BerekendRes[["Resultaat_criterium"]],
        Resultaat[["Resultaat_criterium"]] %>%
          mutate(
            Index_harm_criterium =
              ifelse(
                .data$Criterium == "Structuur" & .data$ID == "Ts2036",
                -0.1428571429,
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
                .data$Criterium == "Structuur" & .data$ID == "Ts2036" &
                  .data$Indicator == "ouderdomsstructuur Struikheide",
                0.5,
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
                .data$Criterium == "Structuur" & .data$ID == "Ts2036" &
                  .data$Referentiewaarde == "1",
                "1 - 2",
                .data$Waarde
              ),
            Waarde =
              ifelse(
                .data$Criterium == "Structuur" & .data$ID == "Ts2036" &
                  .data$Referentiewaarde == "2",
                "3 - 4",
                .data$Waarde
              ),
            Verschilscore =
              ifelse(
                .data$Criterium == "Structuur" & .data$ID == "Ts2036" &
                  .data$Referentiewaarde == "1",
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
          )
      )
    )
    stopifnot(
      all.equal(
        BerekendRes[["Resultaat_globaal"]],
        Resultaat[["Resultaat_globaal"]] %>%
          mutate(
            Index_harm_harm =
              ifelse(
                .data$ID == "Ts2036",
                0.1313062683,
                .data$Index_harm_harm
              )
          )
      )
    )
  })

  it("De afhandeling van taxa en subtaxa gebeurt correct", {
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
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
                Vegetatielaag = "kruidlaag",
                stringsAsFactors = FALSE
              )
            )
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]],
        Resultaat_detail = Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            AfkomstWaarde =
              ifelse(
                .data$ID == "JR0216" & .data$Indicator == "verruiging",
                "berekend",
                .data$AfkomstWaarde
              )
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]]
      )
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
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
                Vegetatielaag = "kruidlaag",
                stringsAsFactors = FALSE
              )
            )
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]],
        Resultaat_detail = Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            AfkomstWaarde =
              ifelse(
                .data$ID == "JR0216" & .data$Indicator == "verruiging",
                "berekend",
                .data$AfkomstWaarde
              )
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]]
      )
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
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
                Vegetatielaag = "kruidlaag",
                stringsAsFactors = FALSE
              )
            )
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]],
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]],
        Resultaat_detail = Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            AfkomstWaarde =
              ifelse(
                .data$ID == "JR0216" & .data$Indicator == "verruiging",
                "berekend",
                .data$AfkomstWaarde
              )
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]]
      )
    )
  })

  it("Een beoordeling op indicatorniveau wordt correct afgehandeld", {
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            mutate(
              Voorwaarde =
                ifelse(
                  .data$Indicator == "vergrassing/verruiging",
                  NA,
                  .data$Voorwaarde
                ),
              Waarde =
                ifelse(
                  .data$Indicator == "vergrassing/verruiging",
                  "TRUE",
                  .data$Waarde
                ),
              Type =
                ifelse(
                  .data$Indicator == "vergrassing/verruiging",
                  NA,
                  .data$Type
                ),
              Eenheid =
                ifelse(
                  .data$Indicator == "vergrassing/verruiging",
                  NA,
                  .data$Eenheid
                )
            ),
          Data_soortenKenmerken
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]] %>%
          mutate(
            Index_min_criterium =
              ifelse(
                .data$Criterium == "Verstoring",
                NA,
                .data$Index_min_criterium
              ),
            Index_harm_criterium =
              ifelse(
                .data$Criterium == "Verstoring",
                NA,
                .data$Index_harm_criterium
              )
          ),
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]] %>%
          mutate(
            Verschilscore =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$Verschilscore
              )
          ),
        Resultaat_detail = Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            Voorwaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$Voorwaarde
              ),
            Referentiewaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$Referentiewaarde
              ),
            Operator =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$Operator
              ),
            EenheidRefwaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$EenheidRefwaarde
              ),
            TypeRefwaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$TypeRefwaarde
              ),
            Waarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                "TRUE",
                .data$Waarde
              ),
            TypeWaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$TypeWaarde
              ),
            InvoertypeWaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$InvoertypeWaarde
              ),
            EenheidWaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$EenheidWaarde
              ),
            AfkomstWaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                "beoordeling indicator",
                .data$AfkomstWaarde
              ),
            TheoretischMaximum =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$TheoretischMaximum
              ),
            Status_voorwaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$Status_voorwaarde
              ),
            Verschilscore =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$Verschilscore
              )
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]] %>%
          mutate(
            Index_min_min = as.double(NA),
            Index_min_harm = as.double(NA),
            Index_harm_harm = as.double(NA)
          )
      )
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            mutate(
              Voorwaarde =
                ifelse(
                  .data$Indicator == "vergrassing/verruiging",
                  NA,
                  .data$Voorwaarde
                ),
              Waarde =
                ifelse(
                  .data$Indicator == "vergrassing/verruiging",
                  "TRUE",
                  .data$Waarde
                ),
              Type =
                ifelse(
                  .data$Indicator == "vergrassing/verruiging",
                  "TRUE/FALSE",
                  .data$Type
                ),
              Eenheid =
                ifelse(
                  .data$Indicator == "vergrassing/verruiging",
                  NA,
                  .data$Eenheid
                )
            ),
          Data_soortenKenmerken
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]] %>%
          mutate(
            Index_min_criterium =
              ifelse(
                .data$Criterium == "Verstoring",
                NA,
                .data$Index_min_criterium
              ),
            Index_harm_criterium =
              ifelse(
                .data$Criterium == "Verstoring",
                NA,
                .data$Index_harm_criterium
              )
          ),
        Resultaat_indicator = Resultaatv2[["Resultaat_indicator"]] %>%
          mutate(
            Verschilscore =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$Verschilscore
              )
          ),
        Resultaat_detail = Resultaatv2[["Resultaat_detail"]] %>%
          mutate(
            Voorwaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$Voorwaarde
              ),
            Referentiewaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$Referentiewaarde
              ),
            Operator =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$Operator
              ),
            EenheidRefwaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$EenheidRefwaarde
              ),
            TypeRefwaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$TypeRefwaarde
              ),
            Waarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                "TRUE",
                .data$Waarde
              ),
            TypeWaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$TypeWaarde
              ),
            InvoertypeWaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$InvoertypeWaarde
              ),
            EenheidWaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$EenheidWaarde
              ),
            AfkomstWaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                "beoordeling indicator",
                .data$AfkomstWaarde
              ),
            TheoretischMaximum =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$TheoretischMaximum
              ),
            Status_voorwaarde =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$Status_voorwaarde
              ),
            Verschilscore =
              ifelse(
                .data$Indicator == "vergrassing/verruiging",
                NA,
                .data$Verschilscore
              )
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]] %>%
          mutate(
            Index_min_min = as.double(NA),
            Index_min_harm = as.double(NA),
            Index_harm_harm = as.double(NA)
          )
      )
    )
    expect_equal(
      idsWissen(
        berekenLSVIbasis(
          Versie = "Versie 2.0",
          Kwaliteitsniveau = "1",
          Data_habitat,
          Data_voorwaarden %>%
            bind_rows(
              data.frame(
                ID = c("JR0216", "Ts2036"),
                Criterium = "Vegetatie",
                Indicator = "sleutelsoorten",
                Waarde = "FALSE",
                stringsAsFactors = FALSE
              )
            ),
          Data_soortenKenmerken
        )
      ),
      list(
        Resultaat_criterium = Resultaatv2[["Resultaat_criterium"]] %>%
          mutate(
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
            Verschilscore =
              ifelse(
                .data$Indicator == "sleutelsoorten",
                NA,
                .data$Verschilscore
              )
          ) %>%
          distinct(),
        Resultaat_detail = Resultaatv2[["Resultaat_detail"]] %>%
          filter(.data$Indicator != "sleutelsoorten") %>%
          bind_rows(
            data.frame(
              ID = c("JR0216", "Ts2036"),
              Habitattype = "4030",
              "kwaliteit van onderzoek sleutelsoorten" =
                c(
                  "Zeer goed (Zeker geen soorten over het hoofd gezien)",
                  "Matig?(Waarschijnlijk soorten over het hoofd gezien)"
                ),
              Versie = "Versie 2.0",
              Habitattype.y = "4030",
              Criterium = "Vegetatie",
              Indicator = "sleutelsoorten",
              Beoordeling =
                "B: Struikhei + 1",
              Kwaliteitsniveau = as.integer(1),
              Belang = "b",
              AfkomstWaarde = "beoordeling indicator",
              Waarde = "FALSE",
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
          ) %>%
          arrange(
            .data$ID,
            .data$Habitattype,
            .data$Versie,
            .data$Criterium,
            .data$Indicator
          ),
        Resultaat_globaal = Resultaatv2[["Resultaat_globaal"]] %>%
          mutate(
            Index_min_min = as.double(NA),
            Index_min_harm = as.double(NA),
            Index_harm_harm = as.double(NA)
          )
      )
    )
  })

})

library(pool)
poolClose(ConnectiePool)
#werking childID nog testen!
