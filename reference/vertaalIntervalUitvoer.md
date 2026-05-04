# zet een interval om naar een waarde in de gevraagde eenheid

Deze functie zet een interval bestaande uit minimumwaarde en
maximumwaarde om naar een uitvoerwaarde in de opgegeven eenheid. De
functie gebruikt `Type`, `Eenheid` en `Invoertype` om te bepalen welke
omzetting eventueel nodig is. Als minimum en maximum niet dezelfde
waarde hebben, geeft ze beide waarden weer, gescheiden door een "-".

## Usage

``` r
vertaalIntervalUitvoer(Dataset, LIJST, ConnectieLSVIhabitats)
```

## Arguments

- Dataset:

  dataframe met velden `Rijnr`, `Type`, `Min`, `Max`, `Eenheid` en
  `Invoertype`

- LIJST:

  Dataframe met lijst die weergeeft hoe de vertaling moet gebeuren van
  numerieke waarden naar categorische variabelen. Verschillend van
  andere functies die dezelfde lijst gebruiken, mogen hier geen
  overlappende categorieën voorkomen binnen eenzelfde schaal. Om zulke
  lijst te bekomen, moeten uit de lijst gegenereerd door de functie
  [`vertaalInvoerInterval()`](vertaalInvoerInterval.md) de records met
  Basisschaal 1 gefilterd worden.

- ConnectieLSVIhabitats:

  Connectie met de databank met indicatoren voor de LSVI van habitats,
  in te stellen d.m.v. functie
  [`connecteerMetLSVIdb()`](connecteerMetLSVIdb.md).

## Value

Dataframe met velden Min
