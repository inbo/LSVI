idsWissen <- function(Resultaat) {
  Resultaat <-
    list(
      Resultaat_criterium = Resultaat[["Resultaat_criterium"]],
      Resultaat_indicator = Resultaat[["Resultaat_indicator"]],
      Resultaat_detail =
        Resultaat[["Resultaat_detail"]] %>%
        mutate(
          BeoordelingID = NULL,
          Combinatie = NULL,
          VoorwaardeID = NULL,
          TheoretischMaximum = NULL, #tijdelijk, tot issue #64 opgelost is, daarna dit en volgende var verwijderen
          Verschilscore = NULL
        ),
      Resultaat_globaal = Resultaat[["Resultaat_globaal"]]
    )
}