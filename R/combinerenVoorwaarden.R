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
#' combinerenVoorwaarden(
#'   "(720 AND 721) OR 15",
#'   c(720, 721, 15),
#'   c(TRUE, FALSE, TRUE)
#' )
#' 
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom pander evals
#' 

combinerenVoorwaarden <-
  function(Formule, VoorwaardeID, Status){

    assert_that(is.character(Formule))
    assert_that(all(sapply(VoorwaardeID, is.numeric)))
    assert_that(all(sapply(Status, is.logical)))
    assert_that(length(VoorwaardeID) == length(Status))
    #nog testen of Formule bestaat uit EN, OF, haakjes en VoorwaardeID's
    #(en evt. andere tekens die logische berekening toelaten)

    Formule <- gsub(" AND ", " & ", Formule)
    Formule <- gsub(" OR ", " | ", Formule)
    for (i in seq_along(VoorwaardeID)) {
      Formule <-
        gsub(paste0("(^|\\D)", VoorwaardeID[i], "(\\D|$)"), Status[i], Formule)
    }
    Resultaat <- as.logical(evals(Formule)[[1]]$result)

    return(Resultaat)

  }
