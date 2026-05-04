# Invoercontrole voor dataframe `Data_soortenKenmerken`

Om te vermijden dat we meermaals dezelfde invoercontrole moeten
uitvoeren en om de hoofdscripts overzichtelijk te houden, maken we voor
elke invoercontrole een aparte hulpfunctie aan, die we kunnen aanroepen.
Deze wordt NIET geëxporteerd, dus deze functies kunnen niet als commando
gerund worden (maar worden wel gerund als de functie waarin ze
voorkomen, aangeroepen wordt). Ingeval van `Data_soortenKenmerken` is
ook de omzetting van soortnamen naar een `NbnTaxonVersionKey` en de
omzettingen van bedekkingen naar een interval opgenomen in de functie.

## Usage

``` r
invoercontroleData_soortenKenmerken(
  Data_soortenKenmerken,
  ConnectieLSVIhabitats,
  LIJST
)
```

## Arguments

- Data_soortenKenmerken:

  dataframe waarop invoercontrole moet gebeuren.

- ConnectieLSVIhabitats:

  Connectie met de databank met indicatoren voor de LSVI van habitats,
  in te stellen d.m.v. functie
  [`connecteerMetLSVIdb()`](connecteerMetLSVIdb.md).

- LIJST:

  Dataframe met lijst die weergeeft hoe de vertaling moet gebeuren van
  categorische variabelen naar numerieke waarden (en omgekeerd). Default
  worden deze waarden uit de databank met LSVI-indicatoren gehaald
  d.m.v. de functie
  [`vertaalInvoerInterval()`](vertaalInvoerInterval.md). Aangeraden
  wordt om deze default te gebruiken (dus parameter niet expliciet
  invullen), of deze waar nodig aan te vullen met eigen schalen. Omdat
  er ook een omzetting moet gebeuren voor grenswaarden uit de databank,
  kan het niet doorgeven van een gedeelte van deze lijst problemen
  geven.
