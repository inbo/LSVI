context("test parseTaxonnaam")

describe("parseTaxonnaam", {
  it("Taxonnamen worden correct geparst", {
    expect_equal(
      parseTaxonnaam("Salix repens L. subsp. galeifolia Neumann ex Rech. f."),
      "Salix repens subsp. galeifolia"
    )
    expect_equal(
      parseTaxonnaam("Thlaspi alpestre L. non Jacq. var. calaminare Lej."),
      "Thlaspi alpestre var. calaminare"
    )
    expect_equal(
      parseTaxonnaam("Salix sachalinensis 'Sekka' F.Schmidt"),
      "Salix sachalinensis 'Sekka'"
    )
    expect_equal(
      parseTaxonnaam("Populus alba/canescens"),
      "Populus alba groep"
    )
    expect_equal(
      parseTaxonnaam("Dirina repanda auct., non Fr. f. stenhammari (Stenh.) Clauzade & Cl. Roux"), #nolint
      "Dirina repanda f. stenhammari"
    )
    expect_equal(
      parseTaxonnaam("Mentha x velutina Lej."),
      "Mentha x velutina"
    )
    expect_equal(
      parseTaxonnaam("Carex flava L. s.l."),
      "Carex flava groep"
    )
    expect_equal(
      parseTaxonnaam("Taraxacum dunense v. Soest"),
      "Taraxacum dunense"
    )
    expect_equal(
      parseTaxonnaam("Sisymbrium pyrenaicum auct."),
      "Sisymbrium pyrenaicum"
    )
    expect_equal(
      parseTaxonnaam("Setaria ambigua (Guss.) Guss. non (Ten.) Mérat"),
      "Setaria ambigua"
    )
    expect_equal(
      parseTaxonnaam("Rubus planus v.d. Beek"),
      "Rubus planus"
    )
    expect_equal(
      parseTaxonnaam("Euphorbia verrucosa Lam. an L.?"),
      "Euphorbia verrucosa"
    )
    expect_equal(
      parseTaxonnaam("Gyalidea minuta van den Boom & Vezda"),
      "Gyalidea minuta"
    )
    expect_equal(
      parseTaxonnaam("Lepraria alba Ach. nom. superfl."),
      "Lepraria alba"
    )
    expect_equal(
      parseTaxonnaam("Begonia x semperflorens hort. nom. nud."),
      "Begonia x semperflorens cv."
    )
    expect_equal(
      parseTaxonnaam("Centaurea subg. Jacea"),
      "subgen. Jacea"
    )
    expect_equal(
      parseTaxonnaam("Arenaria serpyllifolia L. subsp. serpyllifolia var. viscida (Haller f.) DC."), #nolint
      "Arenaria serpyllifolia var. viscida"
    )
    expect_equal(
      parseTaxonnaam("Caltha palustris L. subsp. araneosa (v. Steenis) v. d. Meijden"), #nolint
      "Caltha palustris subsp. araneosa"
    )
    expect_equal(
      parseTaxonnaam("Carex leporina auct. non L. var. argyroglochin (Hornem.) Koch"), #nolint
      "Carex leporina var. argyroglochin"
    )
    expect_equal(
      parseTaxonnaam("Daucus carota L. subsp. carota cv. Sativus"),
      "Daucus carota cv. carota 'Sativus'"
    )
    expect_equal(
      parseTaxonnaam("Salix babylonica L. var. pekinensis 'Tortuosa'"),
      parseTaxonnaam("Salix babylonica L. var. pekinensis A. Henry cv. Tortuosa") #nolint
    )
  })

})
