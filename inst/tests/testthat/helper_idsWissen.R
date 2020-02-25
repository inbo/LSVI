#'@importFrom dplyr %>% mutate

idsWissen <- function(Resultaat) {
  Resultaat <-
    list(
      Resultaat_criterium = Resultaat[["Resultaat_criterium"]],
      Resultaat_indicator =
        Resultaat[["Resultaat_indicator"]] %>%
        mutate(
          BeoordelingID = NULL
        ),
      Resultaat_detail =
        Resultaat[["Resultaat_detail"]] %>%
        mutate(
          BeoordelingID = NULL,
          Combinatie = NULL,
          VoorwaardeID = NULL
        ),
      Resultaat_globaal = Resultaat[["Resultaat_globaal"]]
    )
  return(Resultaat)
}
