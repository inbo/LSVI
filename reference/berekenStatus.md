# Berekent de Status voor de records van een een opgegeven tabel

Deze functie, die bedoeld is als hulpfunctie voor de hoofdfunctie
[`berekenLSVIbasis()`](berekenLSVIbasis.md), evalueert de status van de
records van een opgegeven `Statustabel` met velden `Waarde`,
`Referentiewaarde` en `Operator`.

## Usage

``` r
berekenStatus(Statustabel)
```

## Arguments

- Statustabel:

  Dataframe met velden `Rijnr`, `RefMin`, `RefMax`, `Operator`,
  `WaardeMin` en `WaardeMax`.

## Value

Deze functie geeft een tabel terug met velden `Rijnr` en `Status`
