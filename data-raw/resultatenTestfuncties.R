# dit script genereert files met berekende resultaten die gebruikt worden ter
# controle in de testfuncties

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
  file = "inst/vbdata/Resultaat_test_bosv2/Resultaat_criterium.csv"  #nolint
)
write.csv2(
  Resultaatv2[["Resultaat_indicator"]],
  file = "inst/vbdata/Resultaat_test_bosv2/Resultaat_indicator.csv"  #nolint
)
write.csv2(
  Resultaatv2[["Resultaat_detail"]],
  file = "inst/vbdata/Resultaat_test_bosv2/Resultaat_detail.csv"  #nolint
)
write.csv2(
  Resultaatv2[["Resultaat_globaal"]],
  file = "inst/vbdata/Resultaat_test_bosv2/Resultaat_globaal.csv"  #nolint
)

