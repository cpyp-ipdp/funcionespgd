\name{modifica_tendencia}
\alias{modifica_tendencia}
\title{Proyecta una variable con tendencia lineal}
\description{
Genera una estimación de valores futuros a partir de una regresión lineal sobre datos observados.
}
\usage{
modifica_tendencia(tabla, anio_inicio, anio_fin, paso, variable)
}
\arguments{
  \item{tabla}{Data frame con las columnas \code{year} y la variable a estimar.}
  \item{anio_inicio}{Año desde el cual comienza la estimación (numérico).}
  \item{anio_fin}{Año final observado para usar en la regresión (numérico).}
  \item{paso}{Número de años futuros que se quieren proyectar (numérico).}
  \item{variable}{Nombre de la columna a estimar, como cadena.}
}
\value{
Un data frame con columnas \code{year} y \code{porcentaje} (o el nombre de la variable estimada).
}
\author{cpyp-ipdp}
