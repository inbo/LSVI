# dit script genereert files met berekende resultaten die gebruikt worden ter
# controle in de testfuncties

library(LSVI)
library(readr)

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
Resultaat <-
  idsWissen(
    berekenLSVIbasis(
      Versie = "Versie 3",
      Kwaliteitsniveau = "1", Data_habitat,
      Data_voorwaarden, Data_soortenKenmerken
    )
  )
save(Resultaat, file = "inst/vbdata/Resultaat_test4030.Rdata")
load("inst/vbdata/Resultaat_test4030.Rdata")
Resultaatv2 <-
  idsWissen(
    berekenLSVIbasis(
      Versie = "Versie 2.0",
      Kwaliteitsniveau = "1", Data_habitat,
      Data_voorwaarden, Data_soortenKenmerken
    )
  )
save(Resultaatv2, file = "inst/vbdata/Resultaat_test4030v2.Rdata")
load("inst/vbdata/Resultaat_test4030v2.Rdata")

Data_habitat <-
  read_csv2(
    system.file("vbdata/data_habitat2330_dw.csv", package = "LSVI"),
    col_types = list(col_character(), col_character(), col_character())
  )
Data_voorwaarden <-
  read_csv2(
    system.file("vbdata/data_voorwaarden2330_dw.csv", package = "LSVI")
  )
Data_soortenKenmerken <-
  read_csv2(
    system.file("vbdata/data_soortenKenmerken2330_dw.csv", package = "LSVI")
  )
Resultaat <-
  idsWissen(
    berekenLSVIbasis(
      Versie = "Versie 3",
      Kwaliteitsniveau = "1", Data_habitat,
      Data_voorwaarden, Data_soortenKenmerken
    )
  )
save(Resultaat, file = "inst/vbdata/Resultaat_test2330_dw.Rdata")
load("inst/vbdata/Resultaat_test2330_dw.Rdata")

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
Resultaat <-
  idsWissen(
    berekenLSVIbasis(
      Versie = "Versie 3",
      Kwaliteitsniveau = "1", Data_habitat,
      Data_voorwaarden, Data_soortenKenmerken
    )
  )
save(Resultaat, file = "inst/vbdata/Resultaat_test_bos.Rdata")
load("inst/vbdata/Resultaat_test_bos.Rdata")

Resultaatv2 <-
  idsWissen(
    berekenLSVIbasis(
      Versie = "Versie 2.0",
      Kwaliteitsniveau = "1", Data_habitat,
      Data_voorwaarden, Data_soortenKenmerken
    )
  )
write.csv2(
  Resultaatv2[["Resultaat_criterium"]],
  file = "inst/vbdata/Resultaat_test_bosv2/Resultaat_criterium.csv"
)
write.csv2(
  Resultaatv2[["Resultaat_indicator"]],
  file = "inst/vbdata/Resultaat_test_bosv2/Resultaat_indicator.csv"
)
write.csv2(
  Resultaatv2[["Resultaat_detail"]],
  file = "inst/vbdata/Resultaat_test_bosv2/Resultaat_detail.csv"
)
write.csv2(
  Resultaatv2[["Resultaat_globaal"]],
  file = "inst/vbdata/Resultaat_test_bosv2/Resultaat_globaal.csv"
)

