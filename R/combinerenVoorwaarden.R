#' @title combineert de Status van voorwaarden via de opgegeven formule
#'
#' @description Technische hulpfunctie die in een formule de ID's vervangt door opgegeven logische waarden en het resultaat van de formule teruggeeft.
#' 
#' @param Formule string van ID's gecombineerd met EN en OF, bijvoorbeeld '(720 EN 721) OF 15'
#' @param VoorwaardeID vector van alle voorwaardeID's die voorkomen in de Formule
#' @param Status vector met voor elke VoorwaardeID een overeenkomstige logische waarde status (TRUE of FALSE)
#' 
#' @return logische waarde TRUE/FALSE die de uitkomst van de Formule is (gecombineerd met VoorwaardeID en Status)
#' 
#' @examples
#' #onderstaand voorbeeld geeft problemen bij het testen van het package door devtools
#' #maar buiten deze context werkt het wel
#' \dontrun{
#' combinerenVoorwaarden(
#'   "(720 AND 721) OR 15",
#'   c(720, 721, 15),
#'   c(TRUE, FALSE, TRUE)
#' )
#' }
#' 
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom stringr str_replace_all str_detect str_extract_all
#' @importFrom pander evals
#' 

combinerenVoorwaarden <-
  function(Formule, VoorwaardeID, Status){

    assert_that(is.character(Formule))
    assert_that(all(sapply(VoorwaardeID, is.numeric)))
    assert_that(all(sapply(Status, is.logical)))
    assert_that(length(VoorwaardeID) == length(Status))
    Formuletest <- str_replace_all(Formule, "\\(", "")
    Formuletest <- str_replace_all(Formuletest, "\\)", "")
    assert_that(
      str_detect(Formuletest, "^(\\d+(( (AND|OR|<=|<|>|>=) \\d+))*)$"),
      msg = "Een van de formules onder CombinerenVoorwaarden bevat andere tekens dan getallen en operatoren. Meld dit probleem aan de beheerder van het package." #nolint
    )
    if (str_detect(Formuletest, "^(\\d+(( (AND|OR) \\d+))*)$")) {
      assert_that(
        all(
          as.integer(str_extract_all(Formule, "\\d+")[[1]]) %in% VoorwaardeID
        ),
        msg = "Een van de formules onder CombinerenVoorwaarden bevat andere getallen dan de overeenkomstige voorwaardeID's. Meld dit probleem aan de beheerder van het package." #nolint
      )
    }

    if (length(Status) == 1) {
      return(Status)
    }
    Formule <- gsub(" AND ", " & ", Formule)
    Formule <- gsub(" OR ", " | ", Formule)
    while (grepl("[[:digit:]]", Formule)) {
      getal <- gsub(".*?([[:digit:]]+).*", "\\1", Formule)
      Formule <-
        gsub(
          "(.*?)([[:digit:]]+)(.*)",
          paste0("\\1", factor(getal, VoorwaardeID, Status), "\\3"),
          Formule
        )
    }
    Resultaat <- as.logical(evals(Formule)[[1]]$result)

    return(Resultaat)

  }
