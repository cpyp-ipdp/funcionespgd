#' Añade columnas estimado/superior/inferior, con o sin intervalos
#'
#' @param data Data frame con columna `year` y una variable numérica
#' @param var Nombre de la columna con los valores base
#' @param margen Margen porcentual para los intervalos (ej. 0.01 = ±1%)
#' @param aplicar_intervalos Lógico. Si FALSE, copia el mismo valor a todas las columnas.
#' @return Data frame con columnas: year, estimado, superior, inferior
#' @export
intervals <- function(data, var = "porcentaje", margen = 0.05, aplicar_intervalos = TRUE) {
  valor <- data[[var]]
  
  if (aplicar_intervalos) {
    estimado  <- valor
    superior  <- valor * (1 + margen)
    inferior  <- valor * (1 - margen)
  } else {
    estimado  <- valor
    superior  <- valor
    inferior  <- valor
  }

  return(dplyr::tibble(
    year = as.numeric(data$year),
    estimado = estimado,
    superior = superior,
    inferior = inferior
  ))
}

