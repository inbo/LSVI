context("test selecteerIndicatoren")

library(dplyr)
library(rlang)

maakConnectiePool()
describe("selectie volgens habitattype", {
  AlleIndicatoren <- selecteerIndicatoren() %>%
    select(.data$Habitatsubtype) %>%
    distinct()
  it("Zonder selectie worden alle habitatsubtypen met criteria gegeven", {
    stopifnot(
      all(
        c("1330_hpr", "4030", "9130", "9190", "91E0_va") %in%
          AlleIndicatoren$Habitatsubtype
      )
    )
  })
  it("Zonder selectie worden habitatsubtypen zonder criteria niet gegeven", {
    stopifnot(!"9130_end" %in% AlleIndicatoren$Habitatsubtype)
  })
  it("Geselecteerde habitat(sub)typen worden altijd gegeven", {
    EnkeleIndicatoren <-
      selecteerIndicatoren(Habitattype = c("4010", "9130_end")) %>%
      select(.data$Habitatsubtype) %>%
      distinct()
    expect_equal(
      EnkeleIndicatoren$Habitatsubtype,
      c("4010", "9130_end")
    )
  })

})

library(pool)
poolClose(ConnectiePool)
