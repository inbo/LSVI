#' S4-klasse waarin de generieke 
#' 
#' Deze virtuele klasse Waarde geeft de mogelijkheid om verschillende typen waarden (getallen, categorische variabelen,...) in eenzelfde veld te bewaren en er gelijkbaardige bewerkingen op uit te voeren (bv. met elkaar te vergelijken).
#' 
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#' 
#' @importFrom assertthat assert_that
#' 
setClass(
  Class = "AnalyseVariabele", 
  representation = 
    representation(
      VoorwaardeID = "numeric",
      Soortengroep = "data.frame",
      Soortensubgroep = "data.frame",
      Studiegroep = "data.frame",
      SubAnalyseVariabele = "character",
      SubRefMin = "numeric",
      SubRefMax = "numeric",
      SubOperator = "character",
      Kenmerken = "data.frame",
      "VIRTUAL"
    )
)

setValidity(
  "AnalyseVariabele",
  function(object) {
    
    if (length(object@Kenmerken) > 0) {
      assert_that(has_name(object@Kenmerken, "Kenmerk"))
      assert_that(is.character(object@Kenmerken$Kenmerk))
      assert_that(has_name(object@Kenmerken, "TypeKenmerk"))
      assert_that(is.character(object@Kenmerken$TypeKenmerk))
      assert_that(
        all(
          tolower(object@Kenmerken$TypeKenmerk) %in%
            c("studiegroep", "soort_latijn", "soort_nl", "soort_nbn")
        ),
        msg = "TypeKenmerk moet een van de volgende waarden zijn: studiegroep, soort_nbn, soort_latijn, soort_nl" #nolint
      )
    }
    return(TRUE)
  }
)

# setMethod(
#   f = "initialize",
#   signature = "AnalyseVariabele",
#   definition = 
#     function(
#       .Object,
#       VoorwaardeID,
#       Soortengroep,
#       Studiegroep,
#       SubAnalyseVariabele,
#       SubRefMin,
#       SubRefMax,
#       SubOperator,
#       LIJST,
#       Kenmerken
#     ) {
#       
#       .Object@VoorwaardeID <- VoorwaardeID
#       
#       if (!missing(Studiegroep)) {
#         .Object@Studiegroep <- Studiegroep
#       }
#       
#       if (!missing(SubAnalyseVariabele)) {
#         .Object@SubAnalyseVariabele <- SubAnalyseVariabele
#         .Object@SubRefMin <- SubRefMin
#         .Object@SubRefMax <- SubRefMax
#         .Object@SubOperator <- SubOperator
#       }
#         
#       if (!missing(Kenmerken)) {
#         .Object@Kenmerken <- Kenmerken
#       }
#       validObject(.Object)
#       
#       return(.Object)
#     }
# )

#' @export
setGeneric(
  name = "getKenmerken",
  def = function(object) {
    standardGeneric("getKenmerken")
  }
)

setMethod(
  f = "getKenmerken",
  signature = "AnalyseVariabele",
  definition = function(object) {
    return(object@Kenmerken)
  }
)

#' @export
setGeneric(
  name = "setKenmerken<-",
  def = function(object, value) {
    standardGeneric("setKenmerken<-")
  }
)

setReplaceMethod(
  f = "setKenmerken",
  signature = "AnalyseVariabele",
  definition = function(object, value) {
    object@Kenmerken <- value
    validObject(object)
    return(object)
  }
)

#' @export
setGeneric(
  name = "getVoorwaardeID",
  def = function(object) {
    standardGeneric("getVoorwaardeID")
  }
)

setMethod(
  f = "getVoorwaardeID",
  signature = "AnalyseVariabele",
  definition = function(object) {
    return(object@VoorwaardeID)
  }
)

#' @export
setGeneric(
  name = "setVoorwaardeID<-",
  def = function(object, value) {
    standardGeneric("setVoorwaardeID<-")
  }
)

setReplaceMethod(
  f = "setVoorwaardeID",
  signature = "AnalyseVariabele",
  definition = function(object, value) {
    object@VoorwaardeID <- value
    validObject(object)
    return(object)
  }
)

#' @export
setGeneric(
  name = "getSoortengroep",
  def = function(object) {
    standardGeneric("getSoortengroep")
  }
)

setMethod(
  f = "getSoortengroep",
  signature = "AnalyseVariabele",
  definition = function(object) {
    return(object@Soortengroep)
  }
)

#' @export
setGeneric(
  name = "setSoortengroep<-",
  def = function(object, value) {
    standardGeneric("setSoortengroep<-")
  }
)

setReplaceMethod(
  f = "setSoortengroep",
  signature = "AnalyseVariabele",
  definition = function(object, value) {
    object@Soortengroep <- value
    validObject(object)
    return(object)
  }
)

#' @export
setGeneric(
  name = "getSoortensubgroep",
  def = function(object) {
    standardGeneric("getSoortensubgroep")
  }
)

setMethod(
  f = "getSoortensubgroep",
  signature = "AnalyseVariabele",
  definition = function(object) {
    return(object@Soortensubgroep)
  }
)

#' @export
setGeneric(
  name = "setSoortensubgroep<-",
  def = function(object, value) {
    standardGeneric("setSoortensubgroep<-")
  }
)

setReplaceMethod(
  f = "setSoortensubgroep",
  signature = "AnalyseVariabele",
  definition = function(object, value) {
    object@Soortensubgroep <- value
    validObject(object)
    return(object)
  }
)


#' @export
setGeneric(
  name = "getStudiegroep",
  def = function(object) {
    standardGeneric("getStudiegroep")
  }
)

setMethod(
  f = "getStudiegroep",
  signature = "AnalyseVariabele",
  definition = function(object) {
    return(object@Studiegroep)
  }
)

#' @export
setGeneric(
  name = "setStudiegroep<-",
  def = function(object, value) {
    standardGeneric("setStudiegroep<-")
  }
)

setReplaceMethod(
  f = "setStudiegroep",
  signature = "AnalyseVariabele",
  definition = function(object, value) {
    object@Studiegroep <- value
    validObject(object)
    return(object)
  }
)

#' @export
setGeneric(
  name = "getSubAnalyseVariabele",
  def = function(object) {
    standardGeneric("getSubAnalyseVariabele")
  }
)

setMethod(
  f = "getSubAnalyseVariabele",
  signature = "AnalyseVariabele",
  definition = function(object) {
    return(object@SubAnalyseVariabele)
  }
)

#' @export
setGeneric(
  name = "setSubAnalyseVariabele<-",
  def = function(object, value) {
    standardGeneric("setSubAnalyseVariabele<-")
  }
)

setReplaceMethod(
  f = "setSubAnalyseVariabele",
  signature = "AnalyseVariabele",
  definition = function(object, value) {
    object@SubAnalyseVariabele <- value
    validObject(object)
    return(object)
  }
)

#' @export
setGeneric(
  name = "getSubRefMin",
  def = function(object) {
    standardGeneric("getSubRefMin")
  }
)

setMethod(
  f = "getSubRefMin",
  signature = "AnalyseVariabele",
  definition = function(object) {
    return(object@SubRefMin)
  }
)

#' @export
setGeneric(
  name = "setSubRefMin<-",
  def = function(object, value) {
    standardGeneric("setSubRefMin<-")
  }
)

setReplaceMethod(
  f = "setSubRefMin",
  signature = "AnalyseVariabele",
  definition = function(object, value) {
    object@SubRefMin <- value
    validObject(object)
    return(object)
  }
)

#' @export
setGeneric(
  name = "getSubRefMax",
  def = function(object) {
    standardGeneric("getSubRefMax")
  }
)

setMethod(
  f = "getSubRefMax",
  signature = "AnalyseVariabele",
  definition = function(object) {
    return(object@SubRefMax)
  }
)

#' @export
setGeneric(
  name = "setSubRefMax<-",
  def = function(object, value) {
    standardGeneric("setSubRefMax<-")
  }
)

setReplaceMethod(
  f = "setSubRefMax",
  signature = "AnalyseVariabele",
  definition = function(object, value) {
    object@SubRefMax <- value
    validObject(object)
    return(object)
  }
)

#' @export
setGeneric(
  name = "getSubOperator",
  def = function(object) {
    standardGeneric("getSubOperator")
  }
)

setMethod(
  f = "getSubOperator",
  signature = "AnalyseVariabele",
  definition = function(object) {
    return(object@SubOperator)
  }
)

#' @export
setGeneric(
  name = "setSubOperator<-",
  def = function(object, value) {
    standardGeneric("setSubOperator<-")
  }
)

setReplaceMethod(
  f = "setSubOperator",
  signature = "AnalyseVariabele",
  definition = function(object, value) {
    object@SubOperator <- value
    validObject(object)
    return(object)
  }
)

#' @export
setGeneric(
  name = "berekenWaarde",
  def = function(object) {
    standardGeneric("berekenWaarde")
  }
)