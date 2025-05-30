#' Añade intervalos superior e inferior a una estimación
#'
#' @param data Un data.frame con columnas 'year' y una variable estimada
#' @param var Nombre de la columna de estimación (como string)
#' @param margen Margen proporcional (por ejemplo 0.05 para ±5%)
#' @return El mismo data.frame con columnas adicionales: 'superior' e 'inferior'
#' @export
añade_intervalos <- function(data, var = "porcentaje", margen = 0.05) {
  data$superior <- data[[var]] * (1 + margen)
  data$inferior <- data[[var]] * (1 - margen)
  return(data)
}
