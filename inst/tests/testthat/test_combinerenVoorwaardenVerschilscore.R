context("test combinerenVoorwaarden en combinerenVerschilscores")

library(dplyr)

describe("combineren van voorwaarden gebeurt correct", {
  it("AND en Or wordt correct gecombineerd", {
    expect_true(
      combinerenVoorwaarden(
        "(720 AND 721) OR 15",
        c(720, 721, 15),
        c(TRUE, FALSE, TRUE)
      )
    )
  })

  it("1 voorwaarde wordt correct behandeld", {
    expect_true(
      combinerenVoorwaarden(
        "720",
        720,
        TRUE
      )
    )
  })

  it("vergelijking van voorwaarden wordt correct behandeld", {
    expect_true(
      combinerenVoorwaarden(
        "720 <= 721",
        720,
        TRUE
      )
    )
  })
})

describe("combineren van verschilscores gebeurt correct", {
  it("AND en Or wordt correct gecombineerd", {
    expect_equal(
      combinerenVerschilscore(
        "(720 AND 721) OR 15",
        c(720, 721, 15),
        c(0.5, -0.3, 0.8)
      ),
      0.8
    )
  })
  
  it("1 voorwaarde wordt correct behandeld", {
    expect_equal(
      combinerenVerschilscore(
        "720",
        720,
        0.5
      ),
      0.5
    )
  })
  
  it("vergelijking van voorwaarden wordt correct behandeld", {
    expect_equal(
      combinerenVerschilscore(
        "720 <= 721",
        720,
        0.5
      ),
      0.5
    )
  })
})
