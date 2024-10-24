% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/selecteerIndicatoren.R
\name{selecteerIndicatoren}
\alias{selecteerIndicatoren}
\title{Selecteert indicatoren LSVI op basis van de opgegeven parameters}
\usage{
selecteerIndicatoren(
  Versie = "alle",
  Habitatgroep = "alle",
  Habitattype = "alle",
  Criterium = "alle",
  Indicator = "alle",
  HabitatnamenToevoegen = FALSE,
  ConnectieLSVIhabitats = NULL
)
}
\arguments{
\item{Versie}{De versie van het LSVI-rapport, bv. "Versie 2" of "Versie 3".
Bij de default "alle" worden de gegevens voor de verschillende versies
gegeven.  De mogelijke waarden kunnen opgevraagd worden via
geefUniekeWaarden("Versie", "VersieLSVI") of geefVersieInfo().}

\item{Habitatgroep}{Parameter waarmee alle habitats van een bepaalde
habitatgroep kunnen geselecteerd worden, bv. "Bossen", "Heiden",
"(Half-)natuurlijke graslanden", "Zoete en brakke wateren",...   en "alle"
(=default).  Deze waarde moet niet gespecifieerd worden als een bepaald
habitat(sub)type geselecteerd wordt.  De mogelijke waarden kunnen opgevraagd
worden via geefUniekeWaarden("Habitatgroep", "Naam").}

\item{Habitattype}{Parameter waarmee een habitattype of habitatsubtype kan
geselecteerd worden.  Als dit een habitattype betreft met meerdere subtypes,
zullen de gegevens van alle subtypes van dit habitattype weergegeven worden.
De mogelijke waarden kunnen opgevraagd worden via
geefUniekeWaarden("Habitattype", "Code").  Er is voor deze parameter ook de
mogelijkheid om een vector van meerdere habitat(sub)typen op te geven.}

\item{Criterium}{Het LSVI-criterium waarvoor de gegevens geselecteerd
worden: "Vegetatie", "Structuur", "Verstoring" of "alle".}

\item{Indicator}{De indicator waarvoor de gegevens uit de databank gehaald
worden.  De mogelijke waarden kunnen opgevraagd worden via
geefUniekeWaarden("Indicator", "Naam").}

\item{HabitatnamenToevoegen}{Moeten de namen van de habitattypen en
habitatsubtypen toegevoegd worden als extra kolommen?  (Bij FALSE worden
enkel de habitatcodes toegevoegd, niet de volledige namen.)}

\item{ConnectieLSVIhabitats}{Connectie met de databank met indicatoren voor
de LSVI van habitats, in te stellen d.m.v. functie connecteerMetLSVIdb.}
}
\value{
Deze functie geeft een tabel met velden Versie, Habitattype,
Habitatsubtype, Criterium, Indicator, Indicator_habitatID, TaxongroepId en
Indicator_beoordelingID.
}
\description{
Deze hulpfunctie selecteert de indicatoren die gebruikt worden
voor de bepaling van de Lokale Staat van Instandhouding voor de opgegeven
parameters.  Ze is bedoeld om te gebruiken als bouwsteen in andere functies
waar de gegevens voor bijvoorbeeld een welbepaalde versie of welbepaalde
habitattypes geselecteerd moeten kunnen worden.

De parameters kunnen enkel de hieronder gespecifeerde waarden bevatten en moeten als string opgegeven worden.  Default is telkens 'alle', waarbij de soortenlijsten voor alle mogelijke waarden van die parameter weergegeven worden (m.a.w. er is geen selectie voor deze parameter).
}
\examples{
# Omwille van de iets langere lange duurtijd van de commando's staat bij
# onderstaande voorbeelden de vermelding 'dontrun' (om problemen te vermijden
# bij het testen van het package). Maar de voorbeelden werken en kunnen zeker
# uitgetest worden.
\dontrun{
library(LSVI)
maakConnectiePool()
selecteerIndicatoren(Versie = "Versie 2.0", Habitattype = "4030")
selecteerIndicatoren(Versie = "Versie 2.0", Habitatgroep = "Heiden")
library(pool)
poolClose(ConnectiePool)
}

}
