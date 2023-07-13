#' S4-klasse waarin de generieke onderdelen gedefinieerd worden die voor alle
#' afgeleide klassen gelden (bv. aantal, bedekking,...)
#'
#' Deze virtuele klasse Waarde geeft de mogelijkheid om een aantal klassen en
#' methoden die gelijk zijn voor alle verschillende analysevariabelen, maar
#' eenmaal te moeten aanmaken.  Idee is dat voor elke afgeleide klasse minstens
#' de methode berekenWaarde aangemaakt wordt.
#'
#' @slot Kenmerken dataframe met alle opgegeven kenmerken, met velden
#' Vegetatielaag, Kenmerk, TypeKenmerk, WaardeMin en WaardeMax
#'
#' @noRd
#'
#' @importFrom assertthat assert_that has_name
#' @importFrom methods setClass setValidity getGeneric setMethod
#' setReplaceMethod validObject
#'
setClass(
  Class = "AnalyseVariabele",
  representation =
    representation(
      VoorwaardeID = "numeric",
      Soortengroep = "data.frame",
      Studiegroep = "data.frame",
      SubAnalyseVariabele = "character",
      SubRefMin = "numeric",
      SubRefMax = "numeric",
      SubOperator = "character",
      Kenmerken = "data.frame",
      LIJST = "data.frame",
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
            c("studiegroep", "soort_nbn", "doodhout")
        ),
        msg = "TypeKenmerk moet een van de volgende waarden zijn: studiegroep, soort_nbn" #nolint
      )
    }
    return(TRUE)
  }
)

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

setGeneric(
  name = "berekenWaarde",
  def = function(object) {
    standardGeneric("berekenWaarde")
  }
)

setGeneric(
  name = "getLIJST",
  def = function(object) {
    standardGeneric("getLIJST")
  }
)

setMethod(
  f = "getLIJST",
  signature = "AnalyseVariabele",
  definition = function(object) {
    return(object@LIJST)
  }
)

setGeneric(
  name = "setLIJST<-",
  def = function(object, value) {
    standardGeneric("setLIJST<-")
  }
)

setReplaceMethod(
  f = "setLIJST",
  signature = "AnalyseVariabele",
  definition = function(object, value) {
    object@LIJST <- value
    validObject(object)
    return(object)
  }
)

