# Handleiding LSVI-package

## Leeswijzer

Deze handleiding is bedoeld voor iedereen die met het package LSVI zal
werken. Ze richt zich zowel op gebruikers die de gegevens uit de
databank willen raadplegen maar nooit met R gewerkt hebben als op
ervaren R-gebruikers die eigen scripts willen ontwikkelen voor de
verwerking van hun veldopnames (berekening van LSVI). Er wordt wel van
uitgegaan dat gebruikers een grondige kennis hebben over LSVI van
habitattypen (terminologie, achtergrond,…).

De handleiding is zo opgesteld dat ze voor beide groepen de relevante
informatie bevat om aan de slag te gaan. Enkele hoofdstukken gaan over
de opbouw en het gebruik van het package, en deze zijn zinvol om te
lezen voor alle gebruikers:

- [Doelstellingen en inhoud package](#doelstellingen_inhoud): de globale
  opbouw van de databank en het package, met extra terminologie die in
  het package gebruikt is en ook een ruw overzicht van de inhoud
  (groepen van functies). In een tweede onderdeel wordt dieper ingegaan
  op de doelstelling: een package van en voor onderzoekers die LSVI van
  habitattypes bestuderen. In feite bevat dit deel alle informatie die
  niet specifiek over het gebruik van de code zelf gaat.
- [Installatie van het package](#installatie_package)
- [Gebruik basisfuncties, onderdeel Basis van
  LSVI-package](#basis_LSVI): hoe de functies in het LSVI-package
  gebruiken

Voor beginnende R-gebruikers is er onder [Gebruik
basisfuncties](#gebruik_basisfuncties) een extra onderdeel [Wegwijs in
R](#wegwijs_in_R) waarin ingegaan wordt op enkele basishandelingen in R
die nuttig kunnen zijn bij het gebruik van dit package (hoe documentatie
opzoeken en hoe de bekomen informatie opslaan voor gebruik in Excel).

## Doelstellingen en inhoud package

### Inhoud package

Doelstellingen van het package LSVI zijn om de informatie in verband met
het bepalen van de lokale staat van instandhouding (LSVI) van
habitattypen digitaal ter beschikking te stellen en de functionaliteit
aan te bieden om deze LSVI te berekenen (zie
[Doelstellingen](#doelstellingen) voor details). Het project bestaat uit
2 onderdelen die onlosmakelijk met elkaar verbonden zijn: een databank
die de informatie bevat en het package dat beide doelstellingen
uitvoert, gebruik makend van de informatie uit de databank.

De **databank** bevat alle informatie uit de LSVI-rapporten van de
habitattypen, waarbij de verschillende versies van de rapporten naast
elkaar opgeslagen zijn en dus afzonderlijk geraadpleegd of gebruikt
kunnen worden. Behalve de teksten die letterlijk uit de rapporten
overgenomen zijn, bevat de databank ook informatie in een vorm die door
het package gebruikt kan worden om de informatie op de juiste manier uit
de databank te halen of om berekeningen te maken. Zo is voor elke
beoordeling van een indicator een opsplitsing gemaakt in “voorwaarden”:
de elementaire onderdelen die deel uitmaken van deze beoordeling en die
herleid kunnen worden tot een statement dat door een computer
geëvalueerd kan worden. Vaak zijn deze voorwaarden de onderdelen die in
de rapporten gescheiden zijn door “en” en/of “of”, soms is de
opsplitsing ook gemaakt uit louter technische noodzaak (bv.
$`10% < x < 50%`$ wordt vervangen door de voorwaarden $`x > 10%`$ en
$`x < 50%`$). Voor elk van deze voorwaarden is gedetailleerde informatie
opgenomen over de variabele die nodig is om deze beoordeling te kunnen
maken, die enerzijds bedoeld is om de gebruiker te informeren en
anderzijds om de LSVI-berekening gemakkelijk te kunnen automatiseren.

De aard (complexiteit) van de informatie in de LSVI-rapporten en de
vereiste om ook als basis te dienen voor de rekenmodule hebben geleid
tot een eerder complexe databankstructuur. We hebben dit opgevangen door
in het **package** functies voorzien die louter dienen om de databank te
bevragen: met de functies
[`geefVersieInfo()`](../reference/geefVersieInfo.md),
[`geefInfoHabitatfiche()`](../reference/geefInfoHabitatfiche.md),
[`geefSoortenlijst()`](../reference/geefSoortenlijst.md) en
[`geefInvoervereisten()`](../reference/geefInvoervereisten.md) samen kan
alle info uit de databank opgevraagd worden. Ze focussen respectievelijk
op informatie over de versies van de rapporten
([`geefVersieInfo()`](../reference/geefVersieInfo.md)), de letterlijke
info uit de LSVI-rapporten
([`geefInfoHabitatfiche()`](../reference/geefInfoHabitatfiche.md)), de
soortenlijsten die aan de habitattypes of indicatoren hangen
([`geefSoortenlijst()`](../reference/geefSoortenlijst.md)) en de info
over de voorwaarden die nodig zijn voor de berekening
([`geefInvoervereisten()`](../reference/geefInvoervereisten.md)).

Daarnaast bevat het package enkele functies die deze informatie meteen
in een rapport giet:
[`maakLSVIrapport()`](../reference/maakLSVIrapport.md) genereert een
rapport waarin de habitatfiches van de geselecteerde gegevens na elkaar
weergegeven worden (inclusief titels), en
[`maakHabitatfiches()`](../reference/maakHabitatfiches.md) slaat elke
habitatfiche op in een apart bestand.

Verder bevat het package functies m.b.t. de berekening van de LSVI. De
hoofdfunctie [`berekenLSVIbasis()`](../reference/berekenLSVIbasis.md)
kan door iedereen gebruikt worden om de LSVI te berekenen op basis van
eigen gegevens. De gebruiker moet wel eerst de gegevens van de
veldopnames omzetten naar het formaat dat beschreven is in de
helpfunctie van de functie zelf. Op termijn zullen er ook functies
ontwikkeld worden op maat van een specifieke databank of project (met
bruikbaarheid afhankelijk van de toegang tot de databank).

Omwille van de complexiteit van de berekening roept
[`berekenLSVIbasis()`](../reference/berekenLSVIbasis.md) een aantal
hulpfuncties op die elk een deel van de controle of berekening
uitvoeren. Voor het gemak bij het ontwikkelen zullen deze voorlopig
beschikbaar zijn; op termijn zullen omwille van de overzichtelijkheid
enkel de functies beschikbaar gesteld blijven die een meerwaarde bieden
voor de gebruiker (en relatief gemakkelijk toegepast kunnen worden).

### Doelstellingen

Momenteel bevat het package enkel een basisfunctionaliteit: zoekfuncties
om alle onderdelen van de databank weer te geven in een naar ons gevoel
logische groepering, en de basisfunctie voor de berekening van de LSVI.
In functie van specifieke noden kan de functionaliteit uitgebreid worden
of extra functionaliteit toegevoegd worden.

De doelstelling is om het package op deze manier stap voor stap verder
uit te breiden met nieuwe functionaliteiten op vraag van gebruikers, in
het kader van projecten, naar aanleiding van nieuwe berekeningswijzen
voor de LSVI,… We verwachten hiervoor input van de gebruikers (hoe moet
de berekening gebeuren,…). Ook hopen we dat alle scripts die in het
kader van de LSVI ontwikkeld worden, uiteindelijk hun weg vinden naar
dit package (zie ook de laatste paragraaf van dit onderdeel). Op die
manier is er één plaats waar iedereen voor LSVI-informatie en
-berekeningen terecht kan, kan iedereen alle eerder ontwikkelde
berekeningsmethoden voor de LSVI op zijn/haar gegevens toepassen en
vermijden we binnen INBO een “wildgroei” van berekeningsmethoden met
licht afwijkende resultaten.

We testen de functies die we ontwikkelen, maar we zien ook wel eens iets
over het hoofd. Het is dus (zeker in het begin) mogelijk dat er nog
foutjes in geslopen zijn, dat de functies niet in alle omstandigheden
een correct resultaat (of correcte foutmelding) geven, of dat een
functie zich “irritant” gedraagt (bv. foutmelding geven als je een
hoofdletter ingeeft i.p.v. een kleine letter op een plaats waar dit
verschil niet relevant is). **Als je deze of andere problemen tegenkomt,
meld ze ons dan a.u.b.** (en ga er niet van uit dat je collega’s dat
doen). Geef dan bij voorkeur het script door waarbij het misloopt (waar
van toepassing inclusief de gegevens of een suggestie voor het
resultaat), dit helpt ons om het probleem te situeren en aan te pakken.
Ook alle andere feedback is trouwens welkom.

Als je zelf een script ontwikkelt dat in aanmerking zou kunnen komen om
in het package opgenomen te worden, wees dan collegiaal met je collega’s
die later een gelijkaardig script nodig hebben en neem contact op met de
beheerder van dit package (`cre` in `DESCRIPTION`-file, zie code
hieronder). We bekijken dan samen op welke manier je het script kan
aanleveren en hoe we eventueel resterende taken verdelen. (*Aan het
berekenscript wordt bij voorkeur een stukje script toegevoegd dat
foutcontrole uitvoert op de argumenten van de functie, en documentatie
over wat de functie doet, en er wordt bij voorkeur ook een aparte
testfunctie geschreven die nagaat of de functie in verschillende
situaties het juiste resultaat geeft. We kunnen dit zelf toevoegen of je
hierbij helpen, maar het is handig als we hiervoor wat bijkomende
informatie krijgen.*) Als bedankje word je uiteraard vermeld in de
auteurslijst van het package.

``` r

packageDescription(pkg = "LSVI")
```

    ## Package: LSVI
    ## Title: Rekenmodule Lokale Staat Van Instandhouding van habitattypen
    ## Version: 0.1.4
    ## Authors@R: c( person("Els", "Lommelen", , "els.lommelen@inbo.be", role
    ##         = c("aut", "cre"), comment = c(ORCID = "0000-0002-3481-5684",
    ##         affiliation = "Instituut voor Natuur- en Bosonderzoek
    ##         (INBO)")), person("Patrik", "Oosterlynck", ,
    ##         "patrik.oosterlynck@inbo.be", role = "aut", comment = c(ORCID =
    ##         "0000-0002-5712-0770", affiliation = "Instituut voor Natuur- en
    ##         Bosonderzoek (INBO)")), person("Gert", "Van Spaendonk", ,
    ##         "gert.vanspaendonk@inbo.be", role = "aut", comment = c(ORCID =
    ##         "0009-0008-6988-1113", affiliation = "Instituut voor Natuur- en
    ##         Bosonderzoek (INBO)")), person("Hans", "Van Calster", ,
    ##         "hans.vancalster@inbo.be", role = "aut", comment = c(ORCID =
    ##         "0000-0001-8595-8426", affiliation = "Instituut voor Natuur- en
    ##         Bosonderzoek (INBO)")), person("Martine", "Van Hove", ,
    ##         "martine.vanhove@inbo.be", role = "aut", comment = c(ORCID =
    ##         "0000-0002-6394-0820", affiliation = "Instituut voor Natuur- en
    ##         Bosonderzoek (INBO)")), person("Toon", "Westra", ,
    ##         "toon.westra@inbo.be", role = "aut", comment = c(ORCID =
    ##         "0000-0003-2478-9459", affiliation = "Instituut voor Natuur- en
    ##         Bosonderzoek (INBO)")), person("An", "Leyssen", ,
    ##         "an.leyssen@inbo.be", role = "ctb", comment = c(ORCID =
    ##         "0000-0003-3537-286X", affiliation = "Instituut voor Natuur- en
    ##         Bosonderzoek (INBO)")), person("Toon", "Spanhove", ,
    ##         "toon.spanhove@inbo.be", role = "ctb", comment = c(ORCID =
    ##         "0000-0001-9194-0193", affiliation = "Instituut voor Natuur- en
    ##         Bosonderzoek (INBO)")), person("Instituut voor Natuur- en
    ##         Bosonderzoek (INBO)", , , "info@inbo.be", role = c("cph",
    ##         "fnd"), comment = c(ROR = "00j54wy13")) )
    ## Description: Het package LSVI bundelt een aantal functies in verband
    ##         met het ophalen van informatie over en het berekenen van de
    ##         Lokale Staat Van Instandhouding (LSVI) van habitattypes.
    ## License: GPL-3
    ## URL: https://github.com/inbo/LSVI
    ## BugReports: https://github.com/inbo/LSVI/issues
    ## Depends: R (>= 3.5.0)
    ## Imports: assertthat, DBI, dplyr, methods, odbc, plyr, pool, rgbif,
    ##         rlang, rmarkdown, RSQLite, stringr, tibble, tidyr, tidyselect,
    ##         utils
    ## Suggests: knitr, readr, testthat, xtable
    ## VignetteBuilder: knitr
    ## Config/checklist/communities: inbo
    ## Config/checklist/keywords: habitats; local conservation status
    ## Encoding: UTF-8
    ## Language: nl-BE
    ## LazyData: true
    ## Roxygen: list(markdown = TRUE)
    ## RoxygenNote: 8.0.0
    ## Collate: 'berekenLSVIbasis.R' 'berekenStatus.R'
    ##         'berekenVerschilscores.R' .....
    ## Author: Els Lommelen [aut, cre] (ORCID:
    ##         <https://orcid.org/0000-0002-3481-5684>, affiliation: Instituut
    ##         voor Natuur- en Bosonderzoek (INBO)), Patrik Oosterlynck [aut]
    ##         (ORCID: <https://orcid.org/0000-0002-5712-0770>, affiliation:
    ##         Instituut voor Natuur- en Bosonderzoek (INBO)), Gert Van
    ##         Spaendonk [aut] (ORCID:
    ##         <https://orcid.org/0009-0008-6988-1113>, affiliation: Instituut
    ##         voor Natuur- en Bosonderzoek (INBO)), Hans Van Calster [aut]
    ##         (ORCID: <https://orcid.org/0000-0001-8595-8426>, affiliation:
    ##         Instituut voor Natuur- en Bosonderzoek (INBO)), Martine Van
    ##         Hove [aut] (ORCID: <https://orcid.org/0000-0002-6394-0820>,
    ##         affiliation: Instituut voor Natuur- en Bosonderzoek (INBO)),
    ##         Toon Westra [aut] (ORCID:
    ##         <https://orcid.org/0000-0003-2478-9459>, affiliation: Instituut
    ##         voor Natuur- en Bosonderzoek (INBO)), An Leyssen [ctb] (ORCID:
    ##         <https://orcid.org/0000-0003-3537-286X>, affiliation: Instituut
    ##         voor Natuur- en Bosonderzoek (INBO)), Toon Spanhove [ctb]
    ##         (ORCID: <https://orcid.org/0000-0001-9194-0193>, affiliation:
    ##         Instituut voor Natuur- en Bosonderzoek (INBO)), Instituut voor
    ##         Natuur- en Bosonderzoek (INBO) [cph, fnd] (ROR:
    ##         <https://ror.org/00j54wy13>)
    ## Maintainer: Els Lommelen <els.lommelen@inbo.be>
    ## Built: R 4.6.0; ; 2026-05-04 13:26:08 UTC; unix
    ## 
    ## -- File: /tmp/RtmpDzix72/temp_libpath2d8d24c0e251/LSVI/Meta/package.rds

## Installatie van het package

Het package kan als volgt geïnstalleerd worden (altijd eerst R
herstarten en deze installatie doen in een sessie waarin nog geen andere
packages geladen zijn!):

``` r

# INBO universe activeren (niet nodig als je de typische INBO-installatie hebt)
options(
  repos = c(
    inbo = "https://inbo.r-universe.dev", CRAN = "https://cloud.r-project.org"
  )
)
# Het package installeren
install.packages("LSVI")
```

Voordeel van bovenstaande installatie-methode is dat het package mee
bijgewerkt wordt bij het commando
[`update.packages()`](https://rdrr.io/r/utils/update.packages.html).
Voor onderstaande methodes moet het commando bij elke nieuwe installatie
van R opnieuw gerund worden (en als dit vergeten wordt, geeft gebruiken
van het package mogelijk problemen.)

Een andere optie is om het package vanaf GitHub te installeren (ook hier
eerst alle R-sessies sluiten en een nieuwe versie starten):

``` r

install.packages("remotes")
remotes::install_github("inbo/LSVI", build_vignettes = TRUE)
```

Een testversie met de nieuwste aanpassingen kan als volgt geïnstalleerd
worden:

``` r

remotes::install_github("inbo/LSVI@develop", build_vignettes = TRUE)
```

## Gebruik basisfuncties

### Wegwijs in R

(Wat extra uitleg voor diegenen die niet vertrouwd zijn met R.)

#### R, RStudio, scripts en projecten

R is de taal die je gebruikt om met je computer te communiceren, RStudio
is een programma dat een aantal tools aanbiedt die het je wat
gemakkelijker maken: allerhande knoppen waardoor je niet voor elke
handeling een commando moet typen, “spellingscontrole” (voor R-code),
een “verkenner” waarin je je mappenstructuur kan raadplegen, een lijst
met wat er in het geheugen opgeslagen is,…

De communicatie met je computer gebeurt via de `console`. Als je vaak
gelijkaardige opdrachten wil geven aan je computer, kan je alle
commando’s ingeven in een script en dit opslaan voor later gebruik (aan
te maken met het knopje linksboven onder `File`). Als je je cursor op
een commando in het script zet en `Ctrl`-`ENTER` drukt, wordt dit
commando naar de console gekopieerd en uitgevoerd; bij selectie van
meerdere commando’s wordt de volledige selectie uitgevoerd.

Aan te raden is om scripts en eventueel bijhorende gegevens te bundelen
in een R-project. Dit maak je aan door
`File > New Project > New Directory > New project` te kiezen, een naam
geven aan je project (`Directory name`) en een map te selecteren om je
project in op te slaan. Enkele voordelen:

- nieuw aangemaakte bestanden worden automatisch in deze map opgeslagen,
  en verwijzingen naar bestanden kunnen gebeuren ten opzichte van de map
  van je project (dus relatieve paden i.p.v. absolute paden)
- in een project blijft je “werkomgeving” behouden: als bij het
  afsluiten nog een aantal (niet opgeslagen) scripts open stonden, dan
  zullen deze bij het heropenen van het project nog in dezelfde staat
  zijn (bv. geopend en niet opgeslagen), ook als je tussendoor aan
  andere R-projecten gewerkt hebt
- enkele instellingen moet je niet meer manueel in orde brengen (bv.
  `working directory` instellen), waardoor je minder snel met problemen
  geconfronteerd wordt

Om scripts gemakkelijk te kunnen hergebruiken met andere waarden, kunnen
ze in de vorm van functies geschreven worden. Die functies kunnen
gebundeld worden in een package en beschikbaar gesteld worden voor alle
R-gebruikers.

#### Packages en documentatie

Om in R packages te kunnen gebruiken, moeten deze telkens (na elke
herstart van de R-console) eerst ingeladen worden met de functie
[`library()`](https://rdrr.io/r/base/library.html). Om de functies in
verband met de LSVI te gebruiken, voer je dus eerst het commando
[`library(LSVI)`](https://github.com/inbo/LSVI) in. Dit kan wel enkel
met reeds geïnstalleerde packages, die in het venster rechtsonder onder
de tab Packages vermeld zijn. Andere packages moet je eerst eenmalig
installeren (via de knop `Install` of het commando
`install.packages("naam_nieuw_package")`).

Als je in de lijst met packages onder de tab `Packages` op een
packagenaam klikt, krijg je de documentatie van het package: eventuele
handleidingen (onder de link
`User guides, package vignettes and other documentation`) en een
klikbare lijst met alle functies die in het package beschikbaar zijn.
Dezelfde documentatie is terug te vinden via de bovenste balk op de
[website](https://inbo.github.io/LSVI/index.html): handleidingen staan
onder [`Articles`](https://inbo.github.io/LSVI/articles/index.html) en
functies onder
[`Reference`](https://inbo.github.io/LSVI/reference/index.html).

Zodra een package ingeladen is, kan je deze documentatie van functies
ook opvragen door een `?` te typen, gevolgd door de naam van de functie,
bijvoorbeeld:

``` r

library(LSVI)
?maakLSVIrapport
```

Deze documentatie geeft aan wat de functie doet en hoe je de functie
moet gebruiken:

- `Description`: beschrijft wat de functie doet
- `Usage`: geeft de argumenten weer en de defaultwaarden die voor deze
  argumenten meegegeven worden (zie ook `Arguments`)
- `Arguments`: de argumenten die je met de functie kan of moet meegeven.
  Hiermee bepaal je dus zelf welke uitvoer je wil, door bijvoorbeeld te
  kiezen voor een bepaalde versie of bepaald habitattype. Of bij
  berekenfuncties geef je hier een tabel mee met de veldobservaties
  waarop de berekening uitgevoerd moet worden. Voor deze argumenten moet
  je een waarde meegeven als er geen waarde vermeld is onder `Usage`, in
  het andere geval mag je een waarde meegeven (als je dit niet doet,
  wordt de onder `Usage` vermelde defaultwaarde gebruikt).
- `Value`: wat de functie teruggeeft. Dit is voor het LSVI-package vaak
  een tabel met gegevens, in bovenstaand voorbeeld wordt een document
  gegenereerd dat automatisch opgeslagen wordt in de map waarin je op
  dat moment werkt.
- `Examples`: een of meerdere voorbeelden die tonen hoe je de functie
  kan gebruiken. Als het package LSVI ingeladen is (dus na intypen van
  [`library(LSVI)`](https://github.com/inbo/LSVI)), kan je deze
  voorbeelden kopiëren en uittesten. Door de argumenten aan te passen
  (of toe te voegen of te wissen), kan je op een gemakkelijke manier je
  eigen script maken.

*(Leeswijzer: info over het gebruik van de functies in het LSVI-package
staat onder [Basis van LSVI-package](#basis_LSVI). Wat hier volgt, is
wat met de gegenereerde gegevens gedaan kan worden. We raden daarom aan
om eerst [Basis van LSVI-package](#basis_LSVI) te lezen.)*

#### Wat met gegenereerde gegevens in tabelvorm?

Vaak genereert een functie gegevens in de vorm van een tabel. Hoe kan je
hier nu mee verdergaan?

We illustreren dit a.d.h.v. de functie
[`geefSoortenlijst()`](../reference/geefSoortenlijst.md). Als we deze
functie op zich gebruiken zoals in het voorbeeld in de documentatie is
weergegeven, dan wordt de uitvoer weergegeven in de R-console:

``` r

library(LSVI)
```

    ## Om de functies van het package LSVI te kunnen gebruiken, moet eerst een connectie gelegd worden met de databank.  Dit kan door na het laden van het package eenmaal de functie 'maakConnectiePool()' te runnen.

``` r

maakConnectiePool()
geefSoortenlijst(Habitattype = "4030", Taxonlijsttype = "LSVIfiche")
```

    ##        Versie Habitattype Habitatsubtype TaxongroepId           Omschrijving
    ## 1  Versie 2.0        4030           4030          531 vergrassing/verruiging
    ## 2  Versie 2.0        4030           4030          531 vergrassing/verruiging
    ## 3  Versie 2.0        4030           4030          531 vergrassing/verruiging
    ## 4  Versie 2.0        4030           4030          531 vergrassing/verruiging
    ## 5  Versie 2.0        4030           4030          531 vergrassing/verruiging
    ## 6  Versie 2.0        4030           4030           99          dwergstruiken
    ## 7  Versie 2.0        4030           4030           99          dwergstruiken
    ## 8  Versie 2.0        4030           4030           99          dwergstruiken
    ## 9  Versie 2.0        4030           4030           99          dwergstruiken
    ## 10   Versie 3        4030           4030          100          dwergstruiken
    ## 11   Versie 3        4030           4030          100          dwergstruiken
    ## 12   Versie 3        4030           4030          100          dwergstruiken
    ## 13   Versie 3        4030           4030          100          dwergstruiken
    ## 14   Versie 3        4030           4030          100          dwergstruiken
    ## 15   Versie 3        4030           4030          517            vergrassing
    ## 16   Versie 3        4030           4030          517            vergrassing
    ## 17   Versie 3        4030           4030          517            vergrassing
    ## 18   Versie 3        4030           4030          517            vergrassing
    ## 19   Versie 3        4030           4030          562             verruiging
    ## 20   Versie 3        4030           4030          562             verruiging
    ## 21   Versie 3        4030           4030          488           invasief mos
    ## 22 Versie 2.0        4030           4030          698         sleutelsoorten
    ## 23 Versie 2.0        4030           4030          698         sleutelsoorten
    ## 24 Versie 2.0        4030           4030          698         sleutelsoorten
    ## 25 Versie 2.0        4030           4030          698         sleutelsoorten
    ## 26 Versie 2.0        4030           4030          698         sleutelsoorten
    ## 27 Versie 2.0        4030           4030          698         sleutelsoorten
    ## 28 Versie 2.0        4030           4030          698         sleutelsoorten
    ## 29 Versie 2.0        4030           4030          698  aanwezigheidstruikhei
    ## 30   Versie 3        4030           4030          699         sleutelsoorten
    ## 31   Versie 3        4030           4030          699         sleutelsoorten
    ## 32   Versie 3        4030           4030          699         sleutelsoorten
    ## 33   Versie 3        4030           4030          699         sleutelsoorten
    ## 34   Versie 3        4030           4030          699         sleutelsoorten
    ## 35   Versie 3        4030           4030          699         sleutelsoorten
    ## 36   Versie 3        4030           4030          699         sleutelsoorten
    ## 37   Versie 3        4030           4030          699         sleutelsoorten
    ## 38   Versie 3        4030           4030          699         sleutelsoorten
    ## 39   Versie 3        4030           4030          699         sleutelsoorten
    ## 40   Versie 3        4030           4030          699  aanwezigheidstruikhei
    ##    NbnTaxonVersionKey                                 WetNaam
    ## 1    NBNSYS0000002031           Pteridium aquilinum (L.) Kuhn
    ## 2    NBNSYS0000002499            Molinia caerulea (L.) Moench
    ## 3    NBNSYS0000002623         Deschampsia flexuosa (L.) Trin.
    ## 4    NBNSYS0000039520                                Agrostis
    ## 5    NBNSYS0000137437                                   Rubus
    ## 6    NBNSYS0000003902              Calluna vulgaris (L.) Hull
    ## 7    NBNSYS0000003903                       Erica tetralix L.
    ## 8    NBNSYS0000003909                        Erica cinerea L.
    ## 9    NBNSYS0000003915                  Vaccinium myrtillus L.
    ## 10   NBNSYS0000003902              Calluna vulgaris (L.) Hull
    ## 11   NBNSYS0000003903                       Erica tetralix L.
    ## 12   NBNSYS0000003909                        Erica cinerea L.
    ## 13   NBNSYS0000003914                Vaccinium vitis-idaea L.
    ## 14   NBNSYS0000003915                  Vaccinium myrtillus L.
    ## 15   NBNSYS0000002499            Molinia caerulea (L.) Moench
    ## 16   NBNSYS0000002623         Deschampsia flexuosa (L.) Trin.
    ## 17   NBNSYS0000002630        Calamagrostis epigejos (L.) Roth
    ## 18   NBNSYS0000039520                                Agrostis
    ## 19   NBNSYS0000002031           Pteridium aquilinum (L.) Kuhn
    ## 20   NBNSYS0000137437                                   Rubus
    ## 21   NBNSYS0000036261    Campylopus introflexus (Hedw.) Brid.
    ## 22   INBSYS0000004851 Diphasiastrum tristachyum (Pursh) Holub
    ## 23   NBNSYS0000002003                  Lycopodium clavatum L.
    ## 24   NBNSYS0000003207                      Genista anglica L.
    ## 25   NBNSYS0000003208                       Genista pilosa L.
    ## 26   NBNSYS0000003909                        Erica cinerea L.
    ## 27   NBNSYS0000003914                Vaccinium vitis-idaea L.
    ## 28   NBNSYS0000004030               Cuscuta epithymum (L.) L.
    ## 29   NBNSYS0000003902              Calluna vulgaris (L.) Hull
    ## 30   INBSYS0000004426         Potentilla erecta (L.) Räuschel
    ## 31   INBSYS0000004851 Diphasiastrum tristachyum (Pursh) Holub
    ## 32   NBNSYS0000002003                  Lycopodium clavatum L.
    ## 33   NBNSYS0000002444                     Carex pilulifera L.
    ## 34   NBNSYS0000002676                       Nardus stricta L.
    ## 35   NBNSYS0000003207                      Genista anglica L.
    ## 36   NBNSYS0000003208                       Genista pilosa L.
    ## 37   NBNSYS0000003909                        Erica cinerea L.
    ## 38   NBNSYS0000004030               Cuscuta epithymum (L.) L.
    ## 39   NHMSYS0000458768               Festuca filiformis Pourr.
    ## 40   NBNSYS0000003902              Calluna vulgaris (L.) Hull
    ##                  NedNaam               WetNaamKort TaxonType
    ## 1          Adelaarsvaren       Pteridium aquilinum   Species
    ## 2         Pijpenstrootje          Molinia caerulea   Species
    ## 3         Bochtige smele      Deschampsia flexuosa   Species
    ## 4         Struisgras (G)                  Agrostis     Genus
    ## 5              Braam (G)                     Rubus     Genus
    ## 6              Struikhei          Calluna vulgaris   Species
    ## 7          Gewone dophei            Erica tetralix   Species
    ## 8            Rode dophei             Erica cinerea   Species
    ## 9          Blauwe bosbes       Vaccinium myrtillus   Species
    ## 10             Struikhei          Calluna vulgaris   Species
    ## 11         Gewone dophei            Erica tetralix   Species
    ## 12           Rode dophei             Erica cinerea   Species
    ## 13           Rode bosbes     Vaccinium vitis-idaea   Species
    ## 14         Blauwe bosbes       Vaccinium myrtillus   Species
    ## 15        Pijpenstrootje          Molinia caerulea   Species
    ## 16        Bochtige smele      Deschampsia flexuosa   Species
    ## 17     Gewoon struisriet    Calamagrostis epigejos   Species
    ## 18        Struisgras (G)                  Agrostis     Genus
    ## 19         Adelaarsvaren       Pteridium aquilinum   Species
    ## 20             Braam (G)                     Rubus     Genus
    ## 21 Grijs kronkelsteeltje    Campylopus introflexus   Species
    ## 22     Kleine wolfsklauw Diphasiastrum tristachyum   Species
    ## 23      Grote wolfsklauw       Lycopodium clavatum   Species
    ## 24            Stekelbrem           Genista anglica   Species
    ## 25             Kruipbrem            Genista pilosa   Species
    ## 26           Rode dophei             Erica cinerea   Species
    ## 27           Rode bosbes     Vaccinium vitis-idaea   Species
    ## 28        Klein warkruid         Cuscuta epithymum   Species
    ## 29             Struikhei          Calluna vulgaris   Species
    ## 30             Tormentil         Potentilla erecta   Species
    ## 31     Kleine wolfsklauw Diphasiastrum tristachyum   Species
    ## 32      Grote wolfsklauw       Lycopodium clavatum   Species
    ## 33              Pilzegge          Carex pilulifera   Species
    ## 34           Borstelgras            Nardus stricta   Species
    ## 35            Stekelbrem           Genista anglica   Species
    ## 36             Kruipbrem            Genista pilosa   Species
    ## 37           Rode dophei             Erica cinerea   Species
    ## 38        Klein warkruid         Cuscuta epithymum   Species
    ## 39      Fijn schapengras        Festuca filiformis   Species
    ## 40             Struikhei          Calluna vulgaris   Species

Omdat de tabel te breed is, wordt deze opgesplitst in 3 delen, die onder
elkaar weergegeven worden. Deze tabel wordt ook enkel weergegeven, niet
opgeslagen, en op deze manier kunnen we er achteraf geen bewerkingen mee
doen. Beter is om de tabel toe te kennen aan een variabele, we geven
hier die variabele de naam `soortenlijst4030`:

``` r

soortenlijst4030 <-
  geefSoortenlijst(Habitattype = "4030", Taxonlijsttype = "LSVIfiche")
```

Als je dit commando in je eigen console runt, verschijnt deze variabele
in het venster rechtsboven onder de tab Environment (dat een overzicht
geeft van alle variabelen). Bij tabellen kan je deze variabele bekijken
door erop te klikken.

In R kan je deze variabele tonen door de variabelenaam in te typen (dan
krijg je dezelfde uitvoer van de tabel in 4 delen). Je kan er ook
allerlei bewerkingen mee uitvoeren (sorteren, selecteren,…), maar
daarvoor verwijzen we naar een basiscursus van R.

Wat we hier wel tonen, is hoe deze tabel opgeslagen kan worden, zodat
hij achteraf in Excel geopend kan worden. Dit kan met de functie
[`write_csv2()`](https://readr.tidyverse.org/reference/write_delim.html)
uit het package `readr`. Om deze tabel met naam `soortenlijst4010.csv`
op te slaan in de map R op de C-schijf, gebruiken we volgende
commando’s:

``` r

library(readr)
write_csv2(soortenlijst4030, "C:/R/soortenlijst4030.csv")
```

We hebben hier het absolute path (`C:/R`) weergegeven, een andere
mogelijkheid is om het path relatief ten opzichte van de “working
directory” weer te geven. Dit is de map waarin je aan ’t werken bent,
bijvoorbeeld de hoofdfolder van je project of een subfolder waarin je je
scripts en data opslaat. Je kan deze working directory zelf instellen
door het commando `setwd("C:/.../mijn_werkmap")` (wat je telkens moet
herhalen na opstarten van R). In RStudio kan je ook een “R-project”
aanmaken voor een nieuwe of bestaande folder (aan te raden als je
meerdere scripts hebt), en na het openen van dit project is de root van
dit project automatisch je working directory.

#### Hoe eigen gegevens inlezen in R?

Als je eigen gegevens wil inlezen in R, is de eenvoudigste methode om
deze in Excel op te slaan als
`csv (gescheiden door lijstscheidingsteken)` en in de
`working directory` te plaatsen (of je kan ook het path specifiëren).
Daarna kan je de gegevens als volgt inlezen:

``` r

library(readr)
Dataset <- read_csv2("Gegevens.csv")
```

Je steekt dus de gegevens uit je bestand `Gegevens.csv` in de variabele
`Dataset`. (Het laden van package `readr` met commando
[`library(readr)`](https://readr.tidyverse.org) is enkel nodig als je
dit nog niet eerder gedaan hebt sinds je R opgestart hebt.)

Gegevens kunnen ook rechtstreeks vanuit een databank ingelezen worden,
en ze kunnen in R naar het gewenste formaat omgezet worden. Een
volledige uitleg hierover zou ons echter te ver leiden. Toch interesse
om hier meer over te leren? Neem dan contact op met een collega van BMK.

#### Wat met een list van tabellen?

De functie [`berekenLSVIbasis()`](../reference/berekenLSVIbasis.md)
geeft gegevens terug in de vorm van een lijst van 4 tabellen.

``` r

# Eerst worden wat voorbeeldgegevens opgehaald uit het package:
library(readr)
data_habitat <-
  read_csv2(
    system.file("vbdata/Opname4030habitat.csv", package = "LSVI"),
    col_types = list(col_character(), col_character(), col_character())
  )
```

    ## ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.

``` r

data_voorwaarden <-
  read_csv2(
    system.file("vbdata/Opname4030voorwaardenv2.csv", package = "LSVI")
  )
```

    ## ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.

    ## Rows: 6 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ";"
    ## chr (8): ID, Criterium, Indicator, Voorwaarde, Waarde, Type, Invoertype, Een...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r

data_soorten_kenmerken <-
  read_csv2(
    system.file("vbdata/Opname4030soortenKenmerken.csv", package = "LSVI")
  )
```

    ## ℹ Using "','" as decimal and "'.'" as grouping mark. Use `read_delim()` for more control.
    ## Rows: 13 Columns: 8── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ";"
    ## chr (8): ID, Kenmerk, TypeKenmerk, Waarde, Type, Invoertype, Eenheid, Vegeta...
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r

# En hier gebeurt de berekening zelf:
berekenLSVIbasis(
  Versie = "Versie 2.0",
  Kwaliteitsniveau = "1", data_habitat,
  data_voorwaarden, data_soorten_kenmerken
)
```

    ## $Resultaat_criterium
    ## # A tibble: 6 × 10
    ##   ID     Habitattype kwaliteit van onderzoek…¹ Versie Criterium Kwaliteitsniveau
    ##   <chr>  <chr>       <chr>                     <chr>  <chr>                <int>
    ## 1 JR0216 4030        Zeer goed (Zeker geen so… Versi… Structuur                1
    ## 2 JR0216 4030        Zeer goed (Zeker geen so… Versi… Vegetatie                1
    ## 3 JR0216 4030        Zeer goed (Zeker geen so… Versi… Verstori…                1
    ## 4 Ts2036 4030        Matig?(Waarschijnlijk so… Versi… Structuur                1
    ## 5 Ts2036 4030        Matig?(Waarschijnlijk so… Versi… Vegetatie                1
    ## 6 Ts2036 4030        Matig?(Waarschijnlijk so… Versi… Verstori…                1
    ## # ℹ abbreviated name: ¹​`kwaliteit van onderzoek sleutelsoorten`
    ## # ℹ 4 more variables: Status_criterium <lgl>, Aggregatiemethode <chr>,
    ## #   Index_min_criterium <dbl>, Index_harm_criterium <dbl>
    ## 
    ## $Resultaat_indicator
    ## # A tibble: 10 × 12
    ##    ID     Habitattype kwaliteit van onderzoek sleut…¹ Versie Criterium Indicator
    ##    <chr>  <chr>       <chr>                           <chr>  <chr>     <chr>    
    ##  1 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Structuur dwergstr…
    ##  2 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Structuur ouderdom…
    ##  3 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Vegetatie sleutels…
    ##  4 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Verstori… verbossi…
    ##  5 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Verstori… vergrass…
    ##  6 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Structuur dwergstr…
    ##  7 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Structuur ouderdom…
    ##  8 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Vegetatie sleutels…
    ##  9 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Verstori… verbossi…
    ## 10 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Verstori… vergrass…
    ## # ℹ abbreviated name: ¹​`kwaliteit van onderzoek sleutelsoorten`
    ## # ℹ 6 more variables: Beoordeling <chr>, Belang <chr>, Kwaliteitsniveau <int>,
    ## #   BeoordelingID <int>, Status_indicator <lgl>, Verschilscore <dbl>
    ## 
    ## $Resultaat_detail
    ## # A tibble: 12 × 26
    ##    ID     Habitattype kwaliteit van onderzoek sleut…¹ Versie Criterium Indicator
    ##    <chr>  <chr>       <chr>                           <chr>  <chr>     <chr>    
    ##  1 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Structuur dwergstr…
    ##  2 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Structuur ouderdom…
    ##  3 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Vegetatie sleutels…
    ##  4 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Vegetatie sleutels…
    ##  5 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Verstori… verbossi…
    ##  6 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Verstori… vergrass…
    ##  7 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Structuur dwergstr…
    ##  8 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Structuur ouderdom…
    ##  9 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Vegetatie sleutels…
    ## 10 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Vegetatie sleutels…
    ## 11 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Verstori… verbossi…
    ## 12 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Verstori… vergrass…
    ## # ℹ abbreviated name: ¹​`kwaliteit van onderzoek sleutelsoorten`
    ## # ℹ 20 more variables: Beoordeling <chr>, Kwaliteitsniveau <int>, Belang <chr>,
    ## #   BeoordelingID <int>, Combinatie <chr>, VoorwaardeID <int>,
    ## #   Voorwaarde <chr>, Referentiewaarde <chr>, Operator <chr>,
    ## #   EenheidRefwaarde <chr>, TypeRefwaarde <chr>, InvoertypeRevwaarde <chr>,
    ## #   Waarde <chr>, TypeWaarde <chr>, InvoertypeWaarde <chr>,
    ## #   EenheidWaarde <chr>, AfkomstWaarde <chr>, TheoretischMaximum <dbl>, …
    ## 
    ## $Resultaat_globaal
    ## # A tibble: 2 × 10
    ##   ID     Habitattype kwaliteit van onderzoek sl…¹ Versie Kwaliteitsniveau Status
    ##   <chr>  <chr>       <chr>                        <chr>             <int> <lgl> 
    ## 1 JR0216 4030        Zeer goed (Zeker geen soort… Versi…                1 FALSE 
    ## 2 Ts2036 4030        Matig?(Waarschijnlijk soort… Versi…                1 FALSE 
    ## # ℹ abbreviated name: ¹​`kwaliteit van onderzoek sleutelsoorten`
    ## # ℹ 4 more variables: Aggregatiemethode <chr>, Index_min_min <dbl>,
    ## #   Index_min_harm <dbl>, Index_harm_harm <dbl>

We gaan weer beginnen met de uitvoer in een variabele (`resultaat`) te
steken om er bewerkingen op te kunnen uitvoeren:

``` r

resultaat <-
  berekenLSVIbasis(
    Versie = "Versie 2.0",
    Kwaliteitsniveau = "1", Data_habitat = data_habitat,
    Data_voorwaarden = data_voorwaarden,
    Data_soortenKenmerken = data_soorten_kenmerken
  )
```

Nu kunnen we de afzonderlijke tabellen als volgt uit het object
`resultaat` halen (je kan met `$` of met `[[]]` werken):

``` r

resultaat$Resultaat_globaal
```

    ## # A tibble: 2 × 10
    ##   ID     Habitattype kwaliteit van onderzoek sl…¹ Versie Kwaliteitsniveau Status
    ##   <chr>  <chr>       <chr>                        <chr>             <int> <lgl> 
    ## 1 JR0216 4030        Zeer goed (Zeker geen soort… Versi…                1 FALSE 
    ## 2 Ts2036 4030        Matig?(Waarschijnlijk soort… Versi…                1 FALSE 
    ## # ℹ abbreviated name: ¹​`kwaliteit van onderzoek sleutelsoorten`
    ## # ℹ 4 more variables: Aggregatiemethode <chr>, Index_min_min <dbl>,
    ## #   Index_min_harm <dbl>, Index_harm_harm <dbl>

``` r

resultaat$Resultaat_criterium
```

    ## # A tibble: 6 × 10
    ##   ID     Habitattype kwaliteit van onderzoek…¹ Versie Criterium Kwaliteitsniveau
    ##   <chr>  <chr>       <chr>                     <chr>  <chr>                <int>
    ## 1 JR0216 4030        Zeer goed (Zeker geen so… Versi… Structuur                1
    ## 2 JR0216 4030        Zeer goed (Zeker geen so… Versi… Vegetatie                1
    ## 3 JR0216 4030        Zeer goed (Zeker geen so… Versi… Verstori…                1
    ## 4 Ts2036 4030        Matig?(Waarschijnlijk so… Versi… Structuur                1
    ## 5 Ts2036 4030        Matig?(Waarschijnlijk so… Versi… Vegetatie                1
    ## 6 Ts2036 4030        Matig?(Waarschijnlijk so… Versi… Verstori…                1
    ## # ℹ abbreviated name: ¹​`kwaliteit van onderzoek sleutelsoorten`
    ## # ℹ 4 more variables: Status_criterium <lgl>, Aggregatiemethode <chr>,
    ## #   Index_min_criterium <dbl>, Index_harm_criterium <dbl>

``` r

resultaat$Resultaat_indicator
```

    ## # A tibble: 10 × 12
    ##    ID     Habitattype kwaliteit van onderzoek sleut…¹ Versie Criterium Indicator
    ##    <chr>  <chr>       <chr>                           <chr>  <chr>     <chr>    
    ##  1 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Structuur dwergstr…
    ##  2 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Structuur ouderdom…
    ##  3 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Vegetatie sleutels…
    ##  4 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Verstori… verbossi…
    ##  5 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Verstori… vergrass…
    ##  6 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Structuur dwergstr…
    ##  7 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Structuur ouderdom…
    ##  8 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Vegetatie sleutels…
    ##  9 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Verstori… verbossi…
    ## 10 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Verstori… vergrass…
    ## # ℹ abbreviated name: ¹​`kwaliteit van onderzoek sleutelsoorten`
    ## # ℹ 6 more variables: Beoordeling <chr>, Belang <chr>, Kwaliteitsniveau <int>,
    ## #   BeoordelingID <int>, Status_indicator <lgl>, Verschilscore <dbl>

``` r

resultaat$Resultaat_detail
```

    ## # A tibble: 12 × 26
    ##    ID     Habitattype kwaliteit van onderzoek sleut…¹ Versie Criterium Indicator
    ##    <chr>  <chr>       <chr>                           <chr>  <chr>     <chr>    
    ##  1 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Structuur dwergstr…
    ##  2 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Structuur ouderdom…
    ##  3 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Vegetatie sleutels…
    ##  4 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Vegetatie sleutels…
    ##  5 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Verstori… verbossi…
    ##  6 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Verstori… vergrass…
    ##  7 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Structuur dwergstr…
    ##  8 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Structuur ouderdom…
    ##  9 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Vegetatie sleutels…
    ## 10 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Vegetatie sleutels…
    ## 11 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Verstori… verbossi…
    ## 12 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Verstori… vergrass…
    ## # ℹ abbreviated name: ¹​`kwaliteit van onderzoek sleutelsoorten`
    ## # ℹ 20 more variables: Beoordeling <chr>, Kwaliteitsniveau <int>, Belang <chr>,
    ## #   BeoordelingID <int>, Combinatie <chr>, VoorwaardeID <int>,
    ## #   Voorwaarde <chr>, Referentiewaarde <chr>, Operator <chr>,
    ## #   EenheidRefwaarde <chr>, TypeRefwaarde <chr>, InvoertypeRevwaarde <chr>,
    ## #   Waarde <chr>, TypeWaarde <chr>, InvoertypeWaarde <chr>,
    ## #   EenheidWaarde <chr>, AfkomstWaarde <chr>, TheoretischMaximum <dbl>, …

``` r

# ofwel:
resultaat[["Resultaat_globaal"]]
```

    ## # A tibble: 2 × 10
    ##   ID     Habitattype kwaliteit van onderzoek sl…¹ Versie Kwaliteitsniveau Status
    ##   <chr>  <chr>       <chr>                        <chr>             <int> <lgl> 
    ## 1 JR0216 4030        Zeer goed (Zeker geen soort… Versi…                1 FALSE 
    ## 2 Ts2036 4030        Matig?(Waarschijnlijk soort… Versi…                1 FALSE 
    ## # ℹ abbreviated name: ¹​`kwaliteit van onderzoek sleutelsoorten`
    ## # ℹ 4 more variables: Aggregatiemethode <chr>, Index_min_min <dbl>,
    ## #   Index_min_harm <dbl>, Index_harm_harm <dbl>

``` r

resultaat[["Resultaat_criterium"]]
```

    ## # A tibble: 6 × 10
    ##   ID     Habitattype kwaliteit van onderzoek…¹ Versie Criterium Kwaliteitsniveau
    ##   <chr>  <chr>       <chr>                     <chr>  <chr>                <int>
    ## 1 JR0216 4030        Zeer goed (Zeker geen so… Versi… Structuur                1
    ## 2 JR0216 4030        Zeer goed (Zeker geen so… Versi… Vegetatie                1
    ## 3 JR0216 4030        Zeer goed (Zeker geen so… Versi… Verstori…                1
    ## 4 Ts2036 4030        Matig?(Waarschijnlijk so… Versi… Structuur                1
    ## 5 Ts2036 4030        Matig?(Waarschijnlijk so… Versi… Vegetatie                1
    ## 6 Ts2036 4030        Matig?(Waarschijnlijk so… Versi… Verstori…                1
    ## # ℹ abbreviated name: ¹​`kwaliteit van onderzoek sleutelsoorten`
    ## # ℹ 4 more variables: Status_criterium <lgl>, Aggregatiemethode <chr>,
    ## #   Index_min_criterium <dbl>, Index_harm_criterium <dbl>

``` r

resultaat[["Resultaat_indicator"]]
```

    ## # A tibble: 10 × 12
    ##    ID     Habitattype kwaliteit van onderzoek sleut…¹ Versie Criterium Indicator
    ##    <chr>  <chr>       <chr>                           <chr>  <chr>     <chr>    
    ##  1 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Structuur dwergstr…
    ##  2 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Structuur ouderdom…
    ##  3 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Vegetatie sleutels…
    ##  4 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Verstori… verbossi…
    ##  5 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Verstori… vergrass…
    ##  6 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Structuur dwergstr…
    ##  7 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Structuur ouderdom…
    ##  8 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Vegetatie sleutels…
    ##  9 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Verstori… verbossi…
    ## 10 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Verstori… vergrass…
    ## # ℹ abbreviated name: ¹​`kwaliteit van onderzoek sleutelsoorten`
    ## # ℹ 6 more variables: Beoordeling <chr>, Belang <chr>, Kwaliteitsniveau <int>,
    ## #   BeoordelingID <int>, Status_indicator <lgl>, Verschilscore <dbl>

``` r

resultaat[["Resultaat_detail"]]
```

    ## # A tibble: 12 × 26
    ##    ID     Habitattype kwaliteit van onderzoek sleut…¹ Versie Criterium Indicator
    ##    <chr>  <chr>       <chr>                           <chr>  <chr>     <chr>    
    ##  1 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Structuur dwergstr…
    ##  2 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Structuur ouderdom…
    ##  3 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Vegetatie sleutels…
    ##  4 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Vegetatie sleutels…
    ##  5 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Verstori… verbossi…
    ##  6 JR0216 4030        Zeer goed (Zeker geen soorten … Versi… Verstori… vergrass…
    ##  7 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Structuur dwergstr…
    ##  8 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Structuur ouderdom…
    ##  9 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Vegetatie sleutels…
    ## 10 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Vegetatie sleutels…
    ## 11 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Verstori… verbossi…
    ## 12 Ts2036 4030        Matig?(Waarschijnlijk soorten … Versi… Verstori… vergrass…
    ## # ℹ abbreviated name: ¹​`kwaliteit van onderzoek sleutelsoorten`
    ## # ℹ 20 more variables: Beoordeling <chr>, Kwaliteitsniveau <int>, Belang <chr>,
    ## #   BeoordelingID <int>, Combinatie <chr>, VoorwaardeID <int>,
    ## #   Voorwaarde <chr>, Referentiewaarde <chr>, Operator <chr>,
    ## #   EenheidRefwaarde <chr>, TypeRefwaarde <chr>, InvoertypeRevwaarde <chr>,
    ## #   Waarde <chr>, TypeWaarde <chr>, InvoertypeWaarde <chr>,
    ## #   EenheidWaarde <chr>, AfkomstWaarde <chr>, TheoretischMaximum <dbl>, …

Met deze tabellen kan verder gewerkt worden zoals hierboven beschreven
is ([Wat met gegenereerde gegevens in tabelvorm?](#gegs_in_tabelvorm))

### Basis van LSVI-package

#### Connecteren met de databank

Om de functies van het LSVI-package te gebruiken, heb je een connectie
nodig met de databank met informatie uit LSVI-rapporten (zie
[Doelstellingen en inhoud package \> Inhoud package](#inhoud_package)).
Deze connectie kan op 2 manieren gelegd worden:

- Na het laden van het package eenmalig een pool van connecties aanmaken
  door de functie
  [`maakConnectiePool()`](../reference/maakConnectiePool.md) te runnen.
  De gegenereerde `ConnectiePool` zal in de achtergrond verbinding
  leggen met de databank telkens wanneer dit nodig is, extra connecties
  aanleggen als dit nodig is, overbodig geworden connecties
  verwijderen,… en je moet niet telkens de connectie als parameter
  vermelden bij elke functie die je gebruikt. Met andere woorden: nadat
  je die `ConnectiePool` aangemaakt hebt, moet je je verder niet meer
  bezighouden met die connectie. Is er toch een functie die vereist dat
  je een connectie meegeeft? Dan kan je simpelweg `ConnectiePool` als
  argument meegeven. Als je geen functies meer nodig hebt uit het
  LSVI-package, kan je deze `ConnectiePool` sluiten met
  [`poolClose()`](http://rstudio.github.io/pool/reference/Pool-class.md)
  uit het package `pool` en eventueel het object verwijderen.

``` r

library(LSVI)
maakConnectiePool()
# Nu kan je alle functies gebruiken zonder expliciet het argument
# ConnectieLSVIhabitats op te geven, bv.
geefVersieInfo()
geefSoortenlijst(Habitattype = "4030")

# En als je geen functies meer nodig hebt uit het LSVI-package, kan je de
# ConnectiePool afsluiten en verwijderen:
library(pool)
poolClose(ConnectiePool)
rm(ConnectiePool)
```

- Een connectie met de databank maken met de functie
  [`connecteerMetLSVIdb()`](../reference/connecteerMetLSVIdb.md) en deze
  connectie voor elke functie meegeven bij de parameter
  `ConnectieLSVIhabitats`.

``` r

library(LSVI)
Connectie <- connecteerMetLSVIdb()
# Nu moet je bij elke functie opnieuw deze connectie meegeven
geefVersieInfo(ConnectieLSVIhabitats = Connectie)
geefSoortenlijst(Habitattype = "4030", ConnectieLSVIhabitats = Connectie)

# De connectie sluiten kan met dbDisconnect uit het DBI-package:
library(DBI)
dbDisconnect(Connectie)
```

#### Gebruik van de functies zelf

De functies kunnen min of meer in groepen ingedeeld worden op basis van
het eerste deel van hun naam:

- `maakXXX`: deze functies genereren een bestand (`.html`) waarin de
  informatie uit de databank in een rapportageformaat weergegeven wordt
- `geefXXX`: deze functies genereren een tabel met informatie uit de
  databank
- `berekenXXX`: deze functies voeren berekeningen uit m.b.t. de LSVI op
  basis van opgegeven veldgegevens
- andere (`selecteerXXX`, `vertaalXXX` en `connecteerXXX`): hulpfuncties
  die eventueel relevant kunnen zijn voor gevorderde R-gebruikers

Voor de functies van voornamelijk de eerste twee groepen zijn er een
aantal argumenten die facultatief meegegeven kunnen worden: `Versie`,
`Habitatgroep`, `Habitattype`, `Criterium` en `Indicator`. Door hiervoor
waarden op te geven, kunnen de relevante gegevens uit de databank
geselecteerd worden. Voor de argumenten waarvoor geen specifieke waarden
opgegeven zijn, wordt de default `"alle"` gebruikt, waardoor alle
beschikbare gegevens voor dat argument uit de databank gehaald worden.

Voor een overzicht van de belangrijkste beschikbare functies verwijzen
we naar een eerder hoofdstuk in deze handleiding ( [Doelstellingen en
inhoud package](#doelstellingen_inhoud)) en voor de gedetailleerde
beschrijving verwijzen we naar de [documentatie van de functies
zelf](https://inbo.github.io/LSVI/reference/index.html). In deze
documentatie wordt verwezen naar hulpfuncties waar het relevant is om
deze te gebruiken.

Enkele functies worden niet of amper vermeld in deze handleiding of de
documentatie van andere functies. Dit zijn technische hulpfuncties die
moeilijk zijn in gebruik (vaak doordat ze als argumenten
identificatienummers uit de databank nodig hebben) en die een meer
gebruiksvriendelijk alternatief hebben. Ze dienen doorgaans als
hulpfunctie voor hun gebruiksvriendelijke alternatieven en/of andere
basisfuncties en zijn in de documentatie duidelijk benoemd als
“hulpfunctie” met vaak de vermelding van het meer gebruiksvriendelijke
alternatief. We willen ervaren R-gebruikers de kans te geven om deze
indien nodig te gebruiken, vandaar dat we ervoor kiezen om deze toch
beschikbaar te stellen.

Vragen of hulp nodig? Spring gerust binnen bij BMK.
