# combineert de Verschilscores van voorwaarden die via EN of OF logische operatoren gelinkt zijn

Technische hulpfunctie die in een formule de `VoorwaardeID`'s vervangt
door opgegeven waarden en het resultaat van de formule teruggeeft.

## Usage

``` r
combinerenVerschilscore(Formule, VoorwaardeID, Verschilscore)
```

## Arguments

- Formule:

  string van `VoorwaardeID`'s gecombineerd met EN en OF, bijvoorbeeld
  "(720 EN 721) OF 15"

- VoorwaardeID:

  vector van alle `VoorwaardeID`'s die voorkomen in de `Formule`

- Verschilscore:

  vector met voor elke `VoorwaardeID` een overeenkomstige verschilscore

## Value

gecombineerde verschilscore waarbij EN gecombineerd wordt via het
minimum van beide verschilscores en OF gecombineerd wordt via het
maximum van beide verschilscores

## Examples

``` r
#onderstaand voorbeeld geeft problemen bij het testen van het package door
#devtools, maar buiten deze context werkt het wel
if (FALSE) { # \dontrun{
combinerenVerschilscore(
  "(720 AND 721) OR 15",
  c(720, 721, 15),
  c(0.5, -0.3, 0.8)
)
} # }
```
