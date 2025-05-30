#' Añade intervalos superior e inferior a toda la serie (observados + estimados)
#'
#' @param data Un data.frame con columnas year y una variable estimada
#' @param var Nombre de la columna de estimación
#' @param margen Porcentaje de intervalo (por ejemplo 0.05 para ±5%)
#' @return El mismo data.frame con columnas: 'estimado', 'superior', 'inferior'
#' @export
intervals <- function(data, var = "porcentaje", margen = 0.05) {
  data <- data %>%
    dplyr::mutate(
      estimado = .data[[var]],
      superior = estimado * (1 + margen),
      inferior = estimado * (1 - margen)
    ) %>%
    dplyr::select(year, estimado, superior, inferior)

  return(data)
}
