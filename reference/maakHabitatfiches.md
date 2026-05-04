# Genereert habitatfiche(s) van LSVI op basis van de opgegeven parameters

Deze functie genereert habitatfiches die gebruikt worden voor de
bepaling van de Lokale Staat van Instandhouding van de habitattypes die
voldoen aan de opgegeven parameters. (Om een tabel te genereren met deze
informatie om zelf een fiche te kunnen samenstellen, wordt verwezen naar
de functie [`geefInfoHabitatfiche()`](geefInfoHabitatfiche.md). Om een
rapport samen te stellen met alle fiches na elkaar in 1 document, wordt
verwezen naar de functie [`maakLSVIrapport()`](maakLSVIrapport.md))

De parameters kunnen enkel de hieronder gespecifieerde waarden bevatten
en moeten als string opgegeven worden. Default is telkens "alle",
waarbij de soortenlijsten voor alle mogelijke waarden van die parameter
weergegeven worden (m.a.w. er is geen selectie voor deze parameter).

## Usage

``` r
maakHabitatfiches(
  Versie = "alle",
  Habitatgroep = "alle",
  Habitattype = "alle",
  ConnectieLSVIhabitats = NULL,
  verbose = TRUE
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

- ConnectieLSVIhabitats:

  Connectie met de databank met indicatoren voor de LSVI van habitats,
  in te stellen d.m.v. functie
  [`connecteerMetLSVIdb()`](connecteerMetLSVIdb.md).

- verbose:

  geeft de toestand van het systeem aan, om te zorgen dat boodschappen
  niet onnodig gegeven worden

## Value

Deze functie genereert een rapport met habitatfiches in de vorm van een
`html`-bestand dat in de working directory opgeslagen wordt.

## Examples

``` r
# Omwille van de iets langere lange duurtijd van het commando staat bij
# onderstaand voorbeeld de vermelding 'dontrun' (om problemen te vermijden
# bij het testen van het package). Maar het voorbeeld werkt en mag zeker
# uitgetest worden.
if (FALSE) { # \dontrun{
maakConnectiePool()
maakHabitatfiches(Versie = "Versie 2.0", Habitattype = "4030")
library(pool)
poolClose(ConnectiePool)
} # }
```
