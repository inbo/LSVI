# LSVI 0.1.4

* veld `habitattype.y` verwijderd in output van tabellen van functie
  `berekenLSVIbasis()`
* toevoeging van functionaliteit om taxa in 1 laag te deselecteren in functie
  `deselecteerSoortenKenmerken()` (issue #231)
* bij `berekenLSVIbasis()` worden de extra kolommen uit `Data_habitat`
  toegevoegd aan de uitvoer van de functie
* bij duinhabitats (2120 en 2130) worden de bedekkingen berekend t.o.v. het
  begroeide deel van de duin (issue #240), waarbij invoer van de bedekking van
  naakte bodem of totale vegetatiebedekking nodig is
* ten behoeve van deze berekening is extra analysevariabele `aandeelLaagExcl`
  toegevoegd (die de functionaliteit van `aandeelKruidlaag` en
  `bedekkingLaagExcl` combineert)
* ook is er een extra `SubAnalyseVariabele` `aandeelLaag` toegevoegd die
  voor soorten de relatieve bedekking t.o.v. een vegetatielaag berekent,
  om bv. het aantal soorten te tellen dat minimum de opgegeven bedekking heeft
  in het begroeide deel van de duin
* correcties en updates in databank (#227, #228, #230, #231, #240, #241, #243)

# LSVI 0.1.3

* toevoeging van schaal `BEHEERMONITORINGSCHAAL2021`

# LSVI 0.1.2

* toevoeging van regionaal belangrijke biotopen (rbb)
* toevoeging van habitattype 2190_a

# LSVI 0.1.1

* `NEWS.md` toegevoegd om aanpassingen in het package weer te geven.
* aanpassing in de installatie-instructies
