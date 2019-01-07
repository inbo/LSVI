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
        Rijnr = c(1, 2), RefMin = 1, RefMax = NA, Operator = ">=",
        WaardeMin = c(1, 0),
        WaardeMax = c(1, 0), TheoretischMaximum = 1, TypeVariabele = "Ja/nee",
        stringsAsFactors = FALSE)
    expect_equal(
      berekenStatus(Testdata),
      tibble(Rijnr = c(1, 2), Status = c(TRUE, FALSE))
    )
    stopifnot(
      all.equal(
        berekenVerschilscores(Testdata),
        data.frame(Rijnr = c(1, 2), Verschilscore = c(1, -1))
      )
    )
  })

  it("referentiewaarde 'ja/nee' wordt correct behandeld", {
    Testdata <-
      data.frame(
        Rijnr = 1:2, RefMin = 1, RefMax = NA, Operator = ">=",
        WaardeMin = c(1, 0),
        WaardeMax = NA, TheoretischMaximum = 1, TypeVariabele = "Ja/nee",
        stringsAsFactors = FALSE)
    expect_equal(
      berekenStatus(Testdata),
      tibble(Rijnr = 1:2, Status = c(TRUE, FALSE))
    )
    stopifnot(
      all.equal(
        berekenVerschilscores(Testdata),
        data.frame(Rijnr = 1:2, Verschilscore = c(1, -1))
      )
    )
  })

  it("referentiewaarde 'ja/nee' wordt correct behandeld met operator =", {
    Testdata <-
      data.frame(
        Rijnr = c(1, 2), RefMin = 1, RefMax = NA, Operator = "=",
        WaardeMin = c(1, 0),
        WaardeMax = NA, TheoretischMaximum = 1, TypeVariabele = "Ja/nee",
        stringsAsFactors = FALSE)
    expect_equal(
      berekenStatus(Testdata),
      tibble(Rijnr = c(1, 2), Status = c(TRUE, FALSE))
    )
    stopifnot(
      all.equal(
        berekenVerschilscores(Testdata),
        data.frame(Rijnr = c(1, 2), Verschilscore = c(1, -1))
      )
    )
    Testdata2 <- Testdata %>%
      mutate(RefMin = 0)
    expect_equal(
      berekenStatus(Testdata2),
      tibble(Rijnr = c(1, 2), Status = c(FALSE, TRUE))
    )
    stopifnot(
      all.equal(
        berekenVerschilscores(Testdata2),
        data.frame(Rijnr = c(1, 2), Verschilscore = c(-1, 1))
      )
    )
  })

  it("correcte berekening als TheoretischMaximum niet opgegeven is", {
    Testdata <-
      data.frame(
        Rijnr = c(1, 2), RefMin = 1, RefMax = 1, Operator = "<",
        WaardeMin = c(0.2, 1.2), WaardeMax = c(0.2, 1.2),
        TheoretischMaximum = NA, TypeVariabele = "Decimaal getal",
        stringsAsFactors = FALSE)
    expect_equal(
      berekenStatus(Testdata),
      tibble(Rijnr = c(1, 2), Status = c(TRUE, FALSE))
    )
    stopifnot(
      all.equal(
        berekenVerschilscores(Testdata),
        data.frame(Rijnr = c(1, 2), Verschilscore = c(0.8, NA))
      )
    )
  })

  it("correcte berekening als refwaarde 0 is (bv. exoten moeten afwezig zijn", {
    Testdata <-
      data.frame(
        Rijnr = c(1, 2), RefMin = 0, RefMax = 0, Operator = "<=",
        WaardeMin = c(0, 0.05), WaardeMax = c(0, 0.05),
        TheoretischMaximum = 1, TypeVariabele = "Percentage",
        stringsAsFactors = FALSE)
    expect_equal(
      berekenStatus(Testdata),
      tibble(Rijnr = c(1, 2), Status = c(TRUE, FALSE))
    )
    stopifnot(
      all.equal(
        berekenVerschilscores(Testdata),
        data.frame(Rijnr = c(1, 2), Verschilscore = c(1, -0.05))
      )
    )
  })
})
