% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vertaalIntervalUitvoer.R
\name{vertaalIntervalUitvoer}
\alias{vertaalIntervalUitvoer}
\title{zet een interval om naar een waarde in de gevraagde eenheid}
\usage{
vertaalIntervalUitvoer(Dataset, LIJST, ConnectieLSVIhabitats)
}
\arguments{
\item{Dataset}{dataframe met velden Rijnr, Type, Min, Max, Eenheid en Invoertype}

\item{LIJST}{Dataframe met lijst die weergeeft hoe de vertaling moet gebeuren van categorische variabelen naar numerieke waarden (en omgekeerd).  Default worden deze waarden uit de databank met LSVI-indicatoren gehaald d.m.v. de functie vertaalInvoerInterval().  Aangeraden wordt om deze default te gebruiken (dus parameter niet expliciet invullen), of deze waar nodig aan te vullen met eigen schalen.  Omdat er ook een omzetting moet gebeuren voor grenswaarden uit de databank, kan het niet doorgeven van een gedeelte van deze lijst problemen geven.}

\item{ConnectieLSVIhabitats}{Connectie met de databank met indicatoren voor de LSVI van habitats, in te stellen d.m.v. functie connecteerMetLSVIdb.}
}
\value{
Dataframe met velden Min
}
\description{
Deze functie zet een interval bestaande uit minimumwaarde en maximumwaarde om naar een uitvoerwaarde in de opgegeven eenheid.  De functie gebruikt Type, Eenheid en Invoertype om te bepalen welke omzetting eventueel nodig is.  Als minimum en maximum niet dezelfde waarde hebben, geeft ze beide waarden weer, gescheiden door een '-'.
}