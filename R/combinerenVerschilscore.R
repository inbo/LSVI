#' @title combineert de Verschilscores van voorwaarden die via EN of OF logische operatoren gelinkt zijn
#'
#' @description Technische hulpfunctie die in een formule de ID's vervangt door opgegeven logische waarden en het resultaat van de formule teruggeeft.
#'
#' @param Formule string van ID's gecombineerd met EN en OF, bijvoorbeeld '(720 EN 721) OF 15'
#' @param VoorwaardeID vector van alle voorwaardeID's die voorkomen in de Formule
#' @param Verschilscore vector met voor elke VoorwaardeID een overeenkomstige verschilscore
#'
#' @return gecombineerde verschilscore waarbij EN gecombineerd wordt via het minimum van beide verschilscores en OF gecombineerd wordt via het maximum van beide verschilscores
#'
#' @examples
#' combinerenVerschilscore(
#'   "(720 EN 721) OF 15",
#'   c(720, 721, 15),
#'   c(0.5, -0.3, 0.8)
#' )
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom pander evals
#'

combinerenVerschilscore <-
  function(Formule, VoorwaardeID, Verschilscore){

    assert_that(is.character(Formule))
    assert_that(all(sapply(VoorwaardeID, is.numeric)))
    assert_that(all(sapply(Verschilscore, is.numeric)))
    assert_that(length(VoorwaardeID) == length(Verschilscore))
    #nog testen of Formule bestaat uit EN, OF, haakjes en VoorwaardeID's
    #(en evt. andere tekens die logische berekening toelaten)

    # infix functions voor max en min
    `%max%` <- function(a, b) max(a, b)
    `%min%` <- function(a, b) min(a, b)

    Formule <- gsub(" EN ", " %min% ", Formule)
    Formule <- gsub(" OF ", " %max% ", Formule)
    for (i in seq_along(VoorwaardeID)) {
      Formule <- gsub(VoorwaardeID[i], Verschilscore[i], Formule)
    }
    Resultaat <- as.numeric(evals(Formule, env = new.env())[[1]]$result)

    return(Resultaat)
  }
