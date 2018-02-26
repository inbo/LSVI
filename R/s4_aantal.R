#' S4-klasse die aantal soorten met een bepaalde bedekking berekent
#' 
#' Deze klasse Aantal staat in voor de berekening van waarden voor TypeVariabele Aantal op basis van opgegeven kenmerken
#' 
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#' 
#' @importFrom dplyr %>%
#' @include s4_AnalyseVariabele.R
setClass(
  Class = "aantal", 
  representation =
    representation(),
  contains = "AnalyseVariabele"
)

setMethod(
  f = "berekenWaarde",
  signature = "aantal",
  definition = function(object) {
    
    if (length(object@Soortengroep) > 0) {
      #selecteer soorten uit soortengroep: selecteerSoortenInOpname
    }
    
    if (length(object@Studiegroep) > 0) {
      
      Resultaat <- object@Kenmerken %>%
        filter(.data$TypeKenmerk == "studiegroep") %>%
        left_join(object@Studiegroep,
          by = c("Kenmerk" = "Waarde")
        )
        
    }
    
    if (object@SubAnalyseVariabele == "bedekking") {
      Resultaat <- Resultaat %>%
        mutate(
          RefMin = object@SubRefMin,
          RefMax = object@SubRefMax,
          Operator = object@SubOperator,
          Rijnr = row_number(.data$Kenmerk)
        )
      
      SubStatusberekening <-
        berekenStatus(
          Resultaat[
            , c("Rijnr", "RefMin", "RefMax", "Operator", "WaardeMin", "WaardeMax")
            ]
        )
      
      Resultaat <- Resultaat %>%
        left_join(
          SubStatusberekening,
          by = c("Rijnr")
        ) %>%
        mutate(
          Rijnr = NULL
        ) %>%
        filter(
          .data$Status == TRUE
        )
    }
    
    Aantal <- nrow(Resultaat)
    
    return(Aantal)
  }
)
