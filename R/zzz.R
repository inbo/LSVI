# Deze functie geeft bij opstarten een boodschap mee om de gebruiker te wijzen op recente aanpassingen.
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Om de functies van het package LSVI te kunnen gebruiken, moet eerst een connectie gelegd worden met de databank.  Dit kan door na het laden van het package eenmaal de functie 'maakConnectiePool()' te runnen.")  #nolint
}
