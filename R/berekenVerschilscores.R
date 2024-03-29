#' @title Berekent de verschilscores voor de records van een een opgegeven tabel
#'
#' @description Deze functie, die bedoeld is als hulpfunctie voor de
#' hoofdfunctie berekenLSVIbasis, berekent de verschilscores van de records van
#' een opgegeven 'statustabel' met velden Rijnr, RefMin, RefMax, Operator,
#' WaardeMin, WaardeMax en TheoretischMaximum. De verschilscores hebben een
#' waarde tussen -1 en +1 en geven negatieve of positieve afwijking ten
#' opzichte van de referentiewaarde.
#'
#' @param Statustabel Dataframe met velden Rijnr, RefMin, RefMax, Operator,
#' WaardeMin, WaardeMax, TheoretischMaximum en TypeVariabele.
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
  function(Statustabel) {

    assert_that(inherits(Statustabel, "data.frame"))
    assert_that(has_name(Statustabel, "Rijnr"))
    assert_that(has_name(Statustabel, "RefMin"))
    assert_that(has_name(Statustabel, "RefMax"))
    assert_that(has_name(Statustabel, "Operator"))
    assert_that(has_name(Statustabel, "WaardeMin"))
    assert_that(has_name(Statustabel, "WaardeMax"))
    assert_that(has_name(Statustabel, "TheoretischMaximum"))
    assert_that(has_name(Statustabel, "TypeVariabele"))

    Statustabel$RefMin <- as.double(Statustabel$RefMin)
    Statustabel$RefMax <- as.double(Statustabel$RefMax)
    #geval ja/nee
    if ("Ja/nee" %in% Statustabel$TypeVariabele) {
      Statustabel[Statustabel$TypeVariabele == "Ja/nee",
                  c("RefMin", "RefMax")] <-
        c(Statustabel[Statustabel$TypeVariabele == "Ja/nee", c("RefMin")] - 0.5,
          Statustabel[Statustabel$TypeVariabele == "Ja/nee", c("RefMin")] - 0.5)
    }
    Statustabel[Statustabel$TypeVariabele == "Ja/nee" &
                  is.na(Statustabel$WaardeMax),
                c("WaardeMax")] <-
      Statustabel[Statustabel$TypeVariabele == "Ja/nee" &
                    is.na(Statustabel$WaardeMax),
                  c("WaardeMin")]
    #geval aanwezigheid specifieke sleutelsoort?
    #afh van hoe ingevoerd in databank, mag misschien weg
    Statustabel[Statustabel$TypeVariabele == "Geheel getal" &
                  Statustabel$RefMin == 1 &
                  Statustabel$RefMax == 1 &
                  !is.na(Statustabel$RefMax) &
                  Statustabel$TheoretischMaximum == 1 &
                  !(is.na(Statustabel$TheoretischMaximum)),
                c("RefMin", "RefMax")] <- list(0.5, 0.5)

    Verschiltabel <- Statustabel %>%
      mutate(# rekenkundig gemiddelde van min en max
        Ref = (.data$RefMin + .data$RefMax) / 2,
        Waarde = (.data$WaardeMin + .data$WaardeMax) / 2,
        # negatieve indicatoren * -1
        Teken = ifelse(.data$Operator %in% c("<=", "<"), -1, 1),
        Teken =
          ifelse(
            .data$Ref <= 0 & .data$Operator == "=", -.data$Teken, .data$Teken
          ),
        Ref = abs(.data$Ref),
        BereikGunstig = ifelse(.data$Operator %in% c("<=", "<"),
                               .data$Ref,
                               .data$TheoretischMaximum - .data$Ref),
        BereikOngunstig = .data$TheoretischMaximum - .data$BereikGunstig,
        BereikOngunstig =
          ifelse(
            is.na(.data$BereikOngunstig) & !.data$Operator %in% c("<=", "<"),
            .data$Ref,
            .data$BereikOngunstig
          ),
        Verschil = (.data$Waarde - .data$Ref) * .data$Teken,
        Verschilscore = case_when(
          is.na(.data$Verschil) ~ ifelse(.data$Waarde == .data$Ref, 1, -1),
          #gewijzigd naar >= 0 (bereik gunstig is inclusief de grenswaarde zelf)
          .data$Verschil >= 0 & .data$BereikGunstig != 0 ~
            .data$Verschil / .data$BereikGunstig,
          .data$Verschil >= 0 & .data$BereikGunstig == 0 ~ 1,
          .data$Verschil < 0 ~ .data$Verschil / .data$BereikOngunstig
        )
      ) %>%
      select(
        "Rijnr",
        "Verschilscore"
      )


    return(Verschiltabel)
  }
