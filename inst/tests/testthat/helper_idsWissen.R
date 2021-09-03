#'@importFrom dplyr %>% mutate

idsWissen <- function(Resultaat) {
  ResultaatDetail <-
    Resultaat[["Resultaat_detail"]] %>%
    mutate(
      BeoordelingID = NULL,
      Combinatie = NULL,
      VoorwaardeID = NULL
    )
  attr(ResultaatDetail, "problems") <- NULL
  Resultaat <-
    list(
      Resultaat_criterium = Resultaat[["Resultaat_criterium"]],
      Resultaat_indicator =
        Resultaat[["Resultaat_indicator"]] %>%
        mutate(
          BeoordelingID = NULL
        ),
      Resultaat_detail = ResultaatDetail,
      Resultaat_globaal = Resultaat[["Resultaat_globaal"]]
    )
  return(Resultaat)
}
