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
#' @importFrom dplyr %>% mutate_ select_ group_by_ do_
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
    # assert_that(has_name(Statustabel, "TypeVariabele"))
    # assert_that(has_name(Statustabel, "Eenheid"))
    # assert_that(has_name(Statustabel, "Invoertype"))
    assert_that(has_name(Statustabel, "WaardeMin"))
    assert_that(has_name(Statustabel, "WaardeMax"))
    # assert_that(has_name(Statustabel, "Type"))
    # assert_that(has_name(Statustabel, "Invoertype.vw"))
    # assert_that(has_name(Statustabel, "Eenheid.vw"))

    berekenStatusWaarde <- function(Dataset) {
      colnames(Dataset) <- c("Rijnr", "Waarde", "Operator", "Ref")
      
      Dataset %>%
        mutate_(
          Vergelijking = ~paste(Waarde, Operator, Ref, sep = " "),
          Status = ~ifelse(!is.na(Waarde),
                           sapply(evals(Vergelijking), function(x){as.logical(x[2])}),
                           NA)
        ) %>%
        select_(
          ~Rijnr,
          ~Status
        )
    }
    
    berekenStatusGelijkheid <- function(Dataset) {
      Dataset %>%
        mutate_(
          Status = ~(WaardeMin > RefMin & WaardeMax < RefMin)
        ) %>%
        select_(
          ~Rijnr,
          ~Status
        )
    }
    
    
    Statustabel2 <- Statustabel %>%
      group_by_(~Operator) %>%
      do_(
        Status = 
          ~switch(unique(.$Operator),
                 "<" = berekenStatusWaarde(.[c("Rijnr","WaardeMax", "Operator", "RefMin")]),
                 "<=" = berekenStatusWaarde(.[c("Rijnr","WaardeMax", "Operator", "RefMax")]),
                 ">" = berekenStatusWaarde(.[c("Rijnr","WaardeMin", "Operator", "RefMax")]),
                 ">=" = berekenStatusWaarde(.[c("Rijnr","WaardeMin", "Operator", "RefMin")]),
                 "=" = berekenStatusGelijkheid(.)
        )
      ) %>%
      unnest(Status) %>%
      select_(
        ~Rijnr,
        ~Status
      )

    return(Statustabel2)
  }

