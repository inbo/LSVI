# Geeft tabel met info uit de LSVI-rapporten voor de opgegeven parameters

Deze functie geeft de inhoud van de tabellen habitatkarakteristieken en
beoordelingsmatrix uit de rapporten van de Lokale Staat van
Instandhouding voor de habitattypes die voldoen aan de opgegeven
parameters. Volledigheidshalve geeft ze ook de uitgebreide namen van de
habitattypes en habitatsubtypes. De uitvoer van deze functie kan
gebruikt worden om rapportages op te maken (bv. rapport samenstellen met
LSVI-criteria,...). Een "afgewerkt rapport" kan gegenereerd worden met
de functie [`maakHabitatfiches()`](maakHabitatfiches.md).

De parameters kunnen enkel de hieronder gespecifieerde waarden bevatten
en moeten als string opgegeven worden. Default is telkens "alle",
waarbij de soortenlijsten voor alle mogelijke waarden van die parameter
weergegeven worden (m.a.w. er is geen selectie voor deze parameter).

## Usage

``` r
geefInfoHabitatfiche(
  Versie = "alle",
  Habitatgroep = "alle",
  Habitattype = "alle",
  Criterium = "alle",
  Indicator = "alle",
  Stijl = c("Rmd", "tekst"),
  ConnectieLSVIhabitats = NULL
)
```

## Arguments

- Versie:

  De versie van het LSVI-rapport, bv. "Versie 2" of "Versie 3". Bij de
  default "alle" worden de gegevens voor de verschillende versies
  gegeven. De mogelijke waarden kunnen opgevraagd worden via
  `geefUniekeWaarden("Versie", "VersieLSVI")` of
  [`geefVersieInfo()`](geefVersieInfo.md).

- Habitatgroep:

  Parameter waarmee alle habitats van een bepaalde habitatgroep kunnen
  geselecteerd worden, bv. "Bossen", "Heiden", "(Half-)natuurlijke
  graslanden", "Zoete en brakke wateren",... en "alle" (=default). Deze
  waarde moet niet gespecifieerd worden als een bepaald habitat(sub)type
  geselecteerd wordt. De mogelijke waarden kunnen opgevraagd worden via
  `geefUniekeWaarden("Habitatgroep", "Naam")`.

- Habitattype:

  Parameter waarmee een habitattype of habitatsubtype kan geselecteerd
  worden. Als dit een habitattype betreft met meerdere subtypes, zullen
  de gegevens van alle subtypes van dit habitattype weergegeven worden.
  De mogelijke waarden kunnen opgevraagd worden via
  `geefUniekeWaarden("Habitattype", "Code")`. Er is voor deze parameter
  ook de mogelijkheid om een vector van meerdere habitat(sub)typen op te
  geven.

- Criterium:

  Het LSVI-criterium waarvoor de gegevens geselecteerd worden:
  "Vegetatie", "Structuur", "Verstoring" of "alle".

- Indicator:

  De indicator waarvoor de gegevens uit de databank gehaald worden. De
  mogelijke waarden kunnen opgevraagd worden via
  `geefUniekeWaarden("Indicator", "Naam")`.

- Stijl:

  Keuze uit `"Rmd"` en `"tekst"`. Bij `"Rmd"` (default) worden
  soortgroepnamen voorafgegaan en gevolgd door "\_\_" en Latijnse namen
  van soorten door "\_", waardoor deze bij gebruik van RMarkdown worden
  omgezet naar resp. **vet** en *italics*. Bij `"tekst"` worden deze
  underscores weggelaten.

- ConnectieLSVIhabitats:

  Connectie met de databank met indicatoren voor de LSVI van habitats,
  in te stellen d.m.v. functie
  [`connecteerMetLSVIdb()`](connecteerMetLSVIdb.md).

## Value

Deze functie genereert een tabel met alle gegevens die nodig zijn om de
tabellen habitatkarakteristieken en beoordelingsmatrix uit de
LSVI-rapporten te genereren.

## Examples

``` r
# Omwille van de iets langere lange duurtijd van de commando's staat bij
# onderstaand voorbeeld de vermelding 'dontrun' (om problemen te vermijden
# bij het testen van het package). Maar het voorbeeld werkt en kan zeker
# uitgetest worden.
if (FALSE) { # \dontrun{
library(LSVI)
maakConnectiePool()
geefInfoHabitatfiche(Versie = "Versie 2.0", Habitattype = "4030")
library(pool)
poolClose(ConnectiePool)
} # }
```
