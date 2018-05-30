#' @title Berekent de verschilscores voor de records van een een opgegeven tabel
#'
#' @description Deze functie, die bedoeld is als hulpfunctie voor de hoofdfunctie berekenLSVIbasis, berekent de verschilscores van de records van een opgegeven 'statustabel' met velden Rijnr, RefMin, RefMax, Operator, WaardeMin, WaardeMax en TheoretischMaximum. De verschilscores hebben een waarde tussen -1 en +1 en geven negatieve of positieve afwijking ten opzichte van de referentiewaarde.
#'
#' @param Statustabel Dataframe met velden Rijnr, RefMin, RefMax, Operator, WaardeMin, WaardeMax en TheoretischMaximum.
#'
#'
#' @return Deze functie geeft een tabel terug met velden Rijnr en Verschilscore
#'
#'
#' @export
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% mutate select group_by case_when
#' @importFrom rlang .data
#'
#'
berekenVerschilscores <-
  function(Statustabel){

    assert_that(inherits(Statustabel, "data.frame"))
    assert_that(has_name(Statustabel, "Rijnr"))
    assert_that(has_name(Statustabel, "RefMin"))
    assert_that(has_name(Statustabel, "RefMax"))
    assert_that(has_name(Statustabel, "Operator"))
    assert_that(has_name(Statustabel, "WaardeMin"))
    assert_that(has_name(Statustabel, "WaardeMax"))
    assert_that(has_name(Statustabel, "TheoretischMaximum"))


    Verschiltabel <- Statustabel %>%
      mutate(# rekenkundig gemiddelde van min en max
        Ref = (.data$RefMin + .data$RefMax) / 2,
        Waarde = (.data$WaardeMin + .data$WaardeMax) / 2,
        # negatieve indicatoren * -1
        Teken = ifelse(.data$Operator %in% c("<=", "<"), -1, 1),
        BereikGunstig = ifelse(.data$Operator %in% c("<=", "<"),
                               .data$Ref,
                               .data$TheoretischMaximum - .data$Ref),
        BereikOngunstig = .data$TheoretischMaximum - .data$BereikGunstig,
        Verschil = (.data$Waarde - .data$Ref) * .data$Teken,
        Verschilscore = case_when(
          is.na(.data$Verschil) ~ ifelse(.data$Waarde == .data$Ref, 1, -1),
          #gewijzigd naar >= 0 (bereik gunstig is inclusief de grenswaarde zelf)
          .data$Verschil >= 0 ~ .data$Verschil / .data$BereikGunstig,
          .data$Verschil < 0 ~ .data$Verschil / .data$BereikOngunstig
        )
      ) %>%
      select(
        .data$Rijnr,
        .data$Verschilscore
      )


    return(Verschiltabel)
  }
