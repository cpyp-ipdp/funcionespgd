\name{intervals}
\alias{intervals}
\title{Crea intervalos superior e inferior}
\description{
A partir de una estimación central, genera intervalos ajustando una desviación relativa.
}
\usage{
intervals(df, variable, desviacion)
}
\arguments{
  \item{df}{Data frame con columna de estimaciones.}
  \item{variable}{Nombre de la columna base como string.}
  \item{desviacion}{Margen proporcional (por ejemplo, \code{0.01} para 1\%).}
}
\value{
Un data frame con columnas \code{year}, \code{estimado}, \code{superior} e \code{inferior}.
}
\author{cpyp-ipdp}
