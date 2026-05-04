# Controle van de ingevoerde opname

Deze hulpfunctie voor de s4-klassen `aantal` en `bedekking` selecteert
soorten of kenmerken uit een opname die niet tot de soortgroep of
studiegroep van een bepaalde voorwaarde behoren. Op basis hiervan kunnen
de s4-klassen `bedekkingExcl` en `maxBedekkingExcl` berekend worden (bv.
dominantie van een soort: maximale bedekking van soorten in een opname
exclusief de sleutelsoorten).

## Usage

``` r
deselecteerKenmerkenInOpname(
  Kenmerken,
  Soortengroep,
  Studiegroep,
  SubAnalyseVariabele,
  SubRefMin,
  SubRefMax,
  SubOperator
)
```

## Arguments

- Kenmerken:

  dataframe met alle opgegeven kenmerken, met velden `Kenmerk`,
  `TypeKenmerk`, `WaardeMin` en `WaardeMax`

- Soortengroep:

  dataframe met de soortenlijst die uit Kenmerken gedeselecteerd moet
  worden

- Studiegroep:

  dataframe met de lijst kenmerken die uit Kenmerken gedeselecteerd moet
  worden. Als ook Soortengroep opgegeven is, geeft Studiegroep aan welke
  kenmerken wel behouden moeten blijven na deselecteren van de
  soortengroep.

- SubAnalyseVariabele:

  heeft waarde "bedekking" als er een subvoorwaarde is voor de bedekking
  van de geselecteerde soorten of kenmerken

- SubRefMin:

  minimumwaarde van de grenswaarde voor de bedekking

- SubRefMax:

  maximumwaarde van de grenswaarde voor de bedekking

- SubOperator:

  operator voor deze subvoorwaarde: moet de bedekking hoger of lager
  liggen dan de opgegeven referentiewaarde?

## Value

Deze functie geeft een aangepaste tabel Data_soorten terug waarin enkel
de soorten uit de soortenlijst(en) opgenomen zijn en die bovendien
gekoppeld is aan de gegevens van de soortenlijst.
