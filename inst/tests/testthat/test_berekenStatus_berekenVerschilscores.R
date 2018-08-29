context("test berekenStatus en berekenVerschilscores")

library(dplyr)

describe("berekeningen gebeuren correct", {
  it("operator = wordt correct berekend", {
    Testdata <-
      data.frame(
        Rijnr = 1, RefMin = 0, RefMax = 0, Operator = "=", WaardeMin = 0,
        WaardeMax = 0, TheoretischMaximum = 1, TypeVariabele = "Percentage",
        stringsAsFactors = FALSE)
    expect_equal(
      berekenStatus(Testdata),
      tibble(Rijnr = 1, Status = TRUE)
    )
    stopifnot(
      all.equal(
        berekenVerschilscores(Testdata),
        data.frame(Rijnr = 1, Verschilscore = 0)
      )
    )
  })
  
  it("invoerwaarde 'ja/nee' wordt correct behandeld", {
    Testdata <-
      data.frame(
        Rijnr = 1, RefMin = 1, RefMax = NA, Operator = "<=",
        WaardeMin = 1,
        WaardeMax = 1, TheoretischMaximum = 1, TypeVariabele = "Ja/nee",
        stringsAsFactors = FALSE)
    expect_equal(
      berekenStatus(Testdata),
      tibble(Rijnr = 1, Status = TRUE)
    )
    stopifnot(
      all.equal(
        berekenVerschilscores(Testdata),
        data.frame(Rijnr = 1, Verschilscore = -1)
      )
    )
  })
  
  it("referentiewaarde 'ja/nee' wordt correct behandeld", {
    Testdata <-
      data.frame(
        Rijnr = 1, RefMin = 1, RefMax = NA, Operator = "<=",
        WaardeMin = 1,
        WaardeMax = NA, TheoretischMaximum = 1, TypeVariabele = "Ja/nee",
        stringsAsFactors = FALSE)
    expect_equal(
      berekenStatus(Testdata),
      tibble(Rijnr = 1, Status = TRUE)
    )
    stopifnot(
      all.equal(
        berekenVerschilscores(Testdata),
        data.frame(Rijnr = 1, Verschilscore = 1)
      )
    )
  })
})
