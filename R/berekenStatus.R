#' @title Berekent de Status voor de records van een een opgegeven tabel
#'
#' @description Deze functie, die bedoeld is als hulpfunctie voor de hoofdfunctie berekenLSVIbasis, evalueert de status van de records van een opgegeven 'statustabel' met velden Waarde, Referentiewaarde en Operator.
#' 
#' @param Statustabel Dataframe met velden Rijnr, RefMin, RefMax, Operator, WaardeMin en WaardeMax.
#'
#'
#' @return Deze functie geeft een tabel terug met velden Rijnr en Status
#'
#'
#' @export
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom plyr .
#' @importFrom dplyr %>% mutate select group_by do
#' @importFrom tidyr unnest
#' @importFrom rlang .data
#' @importFrom pander evals
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
            ifelse(
              is.na(.data$WaardeMin) == is.na(.data$WaardeMax) &
                is.na(.data$RefMin) == is.na(.data$RefMax),
              .data$WaardeMin >= .data$RefMin & .data$WaardeMax <= .data$RefMax,
              ifelse(
                is.na(.data$WaardeMax),
                ifelse(
                  is.na(.data$RefMax),
                  .data$WaardeMin == .data$RefMin,
                  .data$WaardeMin >= .data$RefMin &
                    .data$WaardeMin <= .data$RefMax
                ),
                .data$WaardeMin == .data$RefMin &
                  .data$WaardeMax == .data$RefMin
              )
            )
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
