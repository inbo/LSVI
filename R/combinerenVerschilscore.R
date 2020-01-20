#' @title combineert de Verschilscores van voorwaarden die via EN of OF
#' logische operatoren gelinkt zijn
#'
#' @description Technische hulpfunctie die in een formule de ID's vervangt door
#' opgegeven logische waarden en het resultaat van de formule teruggeeft.
#'
#' @param Formule string van ID's gecombineerd met EN en OF, bijvoorbeeld
#' '(720 EN 721) OF 15'
#' @param VoorwaardeID vector van alle voorwaardeID's die voorkomen in de
#' Formule
#' @param Verschilscore vector met voor elke VoorwaardeID een overeenkomstige
#' verschilscore
#'
#' @return gecombineerde verschilscore waarbij EN gecombineerd wordt via het
#' minimum van beide verschilscores en OF gecombineerd wordt via het maximum
#' van beide verschilscores
#'
#' @examples
#' #onderstaand voorbeeld geeft problemen bij het testen van het package door
#' #devtools, maar buiten deze context werkt het wel
#' \dontrun{
#' combinerenVerschilscore(
#'   "(720 AND 721) OR 15",
#'   c(720, 721, 15),
#'   c(0.5, -0.3, 0.8)
#' )
#' }
#'
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom stringr str_replace_all str_detect str_extract_all
#' @importFrom pander evals
#'

combinerenVerschilscore <-
  function(Formule, VoorwaardeID, Verschilscore) {

    assert_that(is.character(Formule))
    assert_that(all(sapply(VoorwaardeID, is.numeric)))
    assert_that(all(sapply(Verschilscore, is.numeric)))
    assert_that(length(VoorwaardeID) == length(Verschilscore))
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

    if (length(Verschilscore) == 1) {
      return(Verschilscore)
    }

    # infix functions voor max en min
    assign(
      "%max%",
      function(a, b) max(a, b)
    )
    assign(
      "%min%",
      function(a, b) min(a, b)
    )

    Formule <- gsub(" AND ", " %min% ", Formule)
    Formule <- gsub(" OR ", " %max% ", Formule)
    Formule <- gsub("([[:digit:]]+)", "ID\\1ID", Formule)
    for (i in seq_along(VoorwaardeID)) {
      Formule <-
        gsub(paste0("ID", VoorwaardeID[i], "ID"), Verschilscore[i], Formule)
    }
    Resultaat <- evals(Formule, env = new.env())[[1]]$result

    if (!is.null(Resultaat)) {
      Resultaat <- as.numeric(Resultaat)
    } else {
      Resultaat <- NA
    }

    return(Resultaat)
  }
