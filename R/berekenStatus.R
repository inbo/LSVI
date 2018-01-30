#' @title Berekent de Status voor de records van een een opgegeven tabel
#'
#' @description Deze functie, die bedoeld is als hulpfunctie voor de hoofdfunctie berekenLSVIbasis, voegt een kolom Status toe aan de opgegeven tabel waarin ze de status evalueert op basis van onder andere de velden Waarde, Referentiewaarde en Operator (zie parameter Statustabel) en ze wist ook enkele velden.
#'
#' @param Statustabel Tabel met velden ...
#'
#'
#' @return Deze functie voegt een extra kolom Status toe aan de opgegeven tabel en verwijdert de velden ...
#'
#'
#' @export
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom dplyr %>% mutate select group_by do
#' @importFrom tidyr unnest
#'
#'
berekenStatus <-
  function(Statustabel){

    assert_that(inherits(Statustabel, "data.frame"))
    assert_that(has_name(Statustabel, "Rijnr"))
    assert_that(has_name(Statustabel, "RefMin"))
    assert_that(has_name(Statustabel, "RefMax"))
    assert_that(has_name(Statustabel, "Operator"))
    assert_that(has_name(Statustabel, "WaardeMin"))
    assert_that(has_name(Statustabel, "WaardeMax"))

    berekenStatusWaarde <- function(Dataset) {
      colnames(Dataset) <- c("Rijnr", "Waarde", "Operator", "Ref")

      Dataset %>%
        mutate(
          Vergelijking =
            paste(.data$Waarde, .data$Operator, .data$Ref, sep = " "),
          Status =
            ifelse(
              !is.na(.data$Waarde),
              sapply(
                evals(.data$Vergelijking),
                function(x) {
                  as.logical(x[2])
                }
              ),
              NA
            )
        ) %>%
        select(
          .data$Rijnr,
          .data$Status
        )
    }

    berekenStatusGelijkheid <- function(Dataset) {
      Dataset %>%
        mutate(
          Status =
            .data$WaardeMin > .data$RefMin & .data$WaardeMax < .data$RefMin
        ) %>%
        select(
          .data$Rijnr,
          .data$Status
        )
    }


    Statustabel2 <- Statustabel %>%
      group_by(.data$Operator) %>%
      do(
        Status =
          switch(unique(.data$Operator),
                 "<" = berekenStatusWaarde(
                   .[c("Rijnr", "WaardeMax", "Operator", "RefMin")]
                 ),
                 "<=" = berekenStatusWaarde(
                   .[c("Rijnr", "WaardeMax", "Operator", "RefMax")]
                 ),
                 ">" = berekenStatusWaarde(
                   .[c("Rijnr", "WaardeMin", "Operator", "RefMax")]
                 ),
                 ">=" = berekenStatusWaarde(
                   .[c("Rijnr", "WaardeMin", "Operator", "RefMin")]
                 ),
                 "=" = berekenStatusGelijkheid(.)
        )
      ) %>%
      unnest(.data$Status) %>%
      select(
        .data$Rijnr,
        .data$Status
      )

    return(Statustabel2)
  }
