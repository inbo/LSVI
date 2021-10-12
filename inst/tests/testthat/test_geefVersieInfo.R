context("test geefVersieInfo()")

maakConnectiePool()
describe("selectie volgens habitattype", {
  AlleVersies <- geefVersieInfo()
  it("De versieinfo wordt correct opgehaald", {
    expect_equal(
      AlleVersies,
      data.frame(
        VersieLSVI = c("Versie 2.0", "Versie 3", "RBB v1"),
        Referentie =
          c("T'Jollyn et al., 2009", "Oosterlynck et al., 2016",
            "De Bie et al., 2018"),
        Beschrijving =
          c("gedegradeerde staat (C) is als de staat minder goed is dan de voorwaarde opgegeven onder kwaliteitsniveau 1", #nolint
            NA, NA),
        Kwaliteitsniveau1 =
          c("Voldoende staat (B)", "Gunstige staat", "niet van toepassing"),
        Kwaliteitsniveau2 =
          c("Goede staat (A)", "Streefwaarde", "Streefwaarde")
      )
    )
  })
})

library(pool)
poolClose(ConnectiePool)
