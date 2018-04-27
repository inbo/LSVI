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
#   berekenLSVIbasis(
#     Versie = "Versie 3",
#     Kwaliteitsniveau = "1", Data_habitat,
#     Data_voorwaarden, Data_soortenKenmerken
#   )
# 
# save(Resultaat, file = "inst/vbdata/Resultaat_test.Rdata")
# load("inst/vbdata/Resultaat_test.Rdata")

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
      berekenLSVIbasis(
        ConnectieLSVIhabitats = ConnectieLSVIhabitats,
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken
      ),
      Resultaat
    )
  })

  it("parameter versie heeft correct formaat", {
    skip_on_cran()
    expect_equal(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken
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
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = "1",
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken
      ),
      Resultaat
    )
    expect_equal(
      berekenLSVIbasis(
        Versie = "Versie 3",
        Kwaliteitsniveau = 1,
        Data_habitat,
        Data_voorwaarden,
        Data_soortenKenmerken
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
      "Niet alle opgegeven percentages zijn numerieke waarden." #nolint
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
      "Niet alle opgegeven percentages zijn positieve waarden"
    )
    # expect_error(
    #   berekenLSVIbasis(
    #     Versie = "Versie 3",
    #     Kwaliteitsniveau = "1",
    #     Data_habitat,
    #     Data_voorwaarden %>%
    #       mutate(
    #         Type =
    #           ifelse(.data$Waarde == "7,5", "Geheel getal", .data$Type)
    #       ),
    #     Data_soortenKenmerken
    #   ),
    #   "Een kommagetal ingevoerd waar een geheel getal verwacht wordt"
    # )
    expect_equal(
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
      ),
      list(
        Resultaat[[1]] %>%
          mutate(
            Status_criterium =
              ifelse(
                .data$Criterium == "Verstoring" & .data$ID == "Ts2036",
                NA,
                .data$Status_criterium
              )
          ),
        Resultaat[[2]] %>%
          mutate(
            Status_indicator =
              ifelse(
                .data$Indicator == "verbossing" & .data$ID == "Ts2036",
                NA,
                .data$Status_indicator
              )
          ),
        Resultaat[[3]] %>%
          mutate(
            Waarde = ifelse(.data$Waarde == "7,5", NA, .data$Waarde),
            Status_voorwaarde =
              ifelse(
                .data$Indicator == "verbossing" & .data$ID == "Ts2036",
                NA,
                .data$Status_voorwaarde
              )
          )
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
      ),
      list(
        Resultaat[[1]],
        Resultaat[[2]],
        Resultaat[[3]] %>%
          mutate(
            Waarde =
              ifelse(.data$Waarde == "f", "F", .data$Waarde)
          )))
    expect_equal(
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
      ),
      list(
        Resultaat[[1]] %>%
          mutate(
            Status_criterium =
              ifelse(
                .data$Criterium == "Verstoring" & .data$ID == "Ts2036",
                NA,
                .data$Status_criterium
              )
          ),
        Resultaat[[2]] %>%
          mutate(
            Status_indicator =
              ifelse(
                .data$Indicator == "vergrassing" & .data$ID == "Ts2036",
                NA,
                .data$Status_indicator
              ),
            Status_indicator =
              ifelse(
                .data$Indicator == "dwergstruiken" & .data$ID == "JR0216",
                NA,
                .data$Status_indicator
              )
          ),
        Resultaat[[3]] %>%
          mutate(
            Status_voorwaarde =
              ifelse(
                .data$Waarde == "f" & .data$ID == "Ts2036",
                NA,
                .data$Status_voorwaarde
              ),
            Waarde =
              ifelse(
                .data$Waarde == "f" & .data$ID == "Ts2036",
                NA,
                .data$Waarde
              ),
            Status_voorwaarde =
              ifelse(
                .data$Waarde == "f" & .data$ID == "JR0216",
                NA,
                .data$Status_voorwaarde
              ),
            Waarde =
              ifelse(
                .data$Waarde == "f" & .data$ID == "JR0216",
                NA,
                .data$Waarde
              )
          )
      )
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
      "Niet alle opgegeven percentages zijn numerieke waarden."
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
                "INBSYS0000014446",
                .data$Kenmerk
              ),
            TypeKenmerk =
              ifelse(
                .data$Kenmerk == "INBSYS0000014446",
                "soort_nbn",
                .data$TypeKenmerk
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
    #nog testen: "Niet alle te evalueren soorten zijn opgenomen onder Data_soorten\\$Soort_Latijn, er wordt van uitgegaan dat de niet opgenomen soorten niet waargenomen zijn"
    #nog extra tests toevoegen voor genera en habitatsubtypes als de ontwikkeling hiervoor op punt staat

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
      list(
        Resultaat[[1]],
        Resultaat[[2]],
        Resultaat[[3]] %>%
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
              )
          )
      )
    )
  })

})


#werking childID nog testen!
