# Controle van de ingevoerde opname

Deze hulpfunctie voor de s4-klassen `aantal` en `bedekking` zoekt
soorten of kenmerken uit de voorwaarde in de opname en maakt een lijstje
van de soorten die voldoen en in de opname voorkomen. Op basis hiervan
kunnen de s4-klassen het totale aantal of de bedekking berekenen.

## Usage

``` r
selecteerKenmerkenInOpname(
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

  dataframe met alle opgegeven kenmerken, met velden `Vegetatielaag`,
  `Kenmerk`, `TypeKenmerk`, `WaardeMin` en `WaardeMax`

- Soortengroep:

  dataframe met de soortenlijst die uit Kenmerken gehaald moet worden

- Studiegroep:

  dataframe met de lijst kenmerken die uit Kenmerken gehaald moet worden

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
