# Geef alle unieke waarden van een veld uit de databank met de LSVI-indicatoren voorafgegaan door "alle"

Deze hulpfunctie geeft een vector met alle verschillende waarden die in
een gespecifieerd veld van een gespecificeerde tabel in de databank met
LSVI-indicatoren staan, voorafgegaan door de (toegevoegde) waarde
"alle". Deze functie wordt in verschillende functies van het package
gebruikt om de invoer van parameters te controleren (waar de mogelijke
invoer bestaat uit "alle" of een item uit het veld). In enkele gevallen
wordt dit commando in de documentatie vermeld zodat ook de gebruiker in
die specifieke gevallen de lijst met mogelijke invoerwaarden op een
eenvoudige manier kan opvragen.

Gebruikers die de databankstructuur en bijhorende naamgeving niet
kennen, kunnen dezelfde info het gemakkelijkst bekomen door een tabel
voor de volledige dataset op te vragen, het gewenste veld te selecteren
en hiervan de unieke waarden weer te geven (zie voorbeeld).

## Usage

``` r
geefUniekeWaarden(Tabelnaam, Veldnaam, ConnectieLSVIhabitats = NULL)
```

## Arguments

- Tabelnaam:

  De naam van de tabel waarin het veld zich bevindt (String)

- Veldnaam:

  De naam van het veld (in de bij Tabelnaam opgegeven tabel) waarvan de
  waarden moeten opgezocht worden (String)

- ConnectieLSVIhabitats:

  Connectie met de databank met indicatoren voor de LSVI van habitats,
  in te stellen d.m.v. functie
  [`connecteerMetLSVIdb()`](connecteerMetLSVIdb.md).

## Value

Deze functie geeft een vector bestaande uit "alle" en de verschillende
waarden uit de gespecifieerde tabel.

## Examples

``` r
# Omwille van de iets langere lange duurtijd van de commando's staat bij
# onderstaand voorbeeld de vermelding 'dontrun' (om problemen te vermijden
# bij het testen van het package). Maar het voorbeelden werkt en kan zeker
# uitgetest worden.
if (FALSE) { # \dontrun{
maakConnectiePool()
geefUniekeWaarden("Versie","VersieLSVI")

#alternatieven om deze invoerlijst te bekomen:
unique(geefVersieInfo()$VersieLSVI)

library(dplyr)
geefVersieInfo() %>%
  select(VersieLSVI) %>%
  distinct()

library(pool)
poolClose(ConnectiePool)
} # }
```
