# Berekent de verschilscores voor de records van een een opgegeven tabel

Deze functie, die bedoeld is als hulpfunctie voor de hoofdfunctie
[`berekenLSVIbasis()`](berekenLSVIbasis.md), berekent de verschilscores
van de records van een opgegeven `Statustabel` met velden `Rijnr`,
`RefMin`, `RefMax`, `Operator`, `WaardeMin`, `WaardeMax` en
`TheoretischMaximum`. De verschilscores hebben een waarde tussen `-1` en
`+1` en geven negatieve of positieve afwijking ten opzichte van de
referentiewaarde.

## Usage

``` r
berekenVerschilscores(Statustabel)
```

## Arguments

- Statustabel:

  Dataframe met velden `Rijnr`, `RefMin`, `RefMax`, `Operator`,
  `WaardeMin`, `WaardeMax`, `TheoretischMaximum` en `TypeVariabele`.

## Value

Deze functie geeft een tabel terug met velden `Rijnr` en `Verschilscore`
