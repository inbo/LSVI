context("test invoercontrole")

maakConnectiePool()
describe("invoercontroleData_habitat", {
  Data_habitat <-
    data.frame(
      ID = 1:5,
      Habitattype = c("3130_Aom", "91E0_vo", "91e0_SF", "rbbmr", "RBBmr"),
      stringsAsFactors = FALSE
    )
  Resultaat <-
    data.frame(
      ID = as.character(1:5),
      Habitattype = c("3130_aom", "91E0_vo", "91E0_sf", "rbbmr", "rbbmr"),
      stringsAsFactors = FALSE
    )
  it("Hoofdletters en kleine letters worden correct behandeld", {
    expect_equal(
      invoercontroleData_habitat(Data_habitat, ConnectiePool),
      Resultaat
    )
    
  })

})

library(pool)
poolClose(ConnectiePool)
