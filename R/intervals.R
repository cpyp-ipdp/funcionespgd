#' Intervalos que se expanden año con año desde un punto base
#'
#' @param data Data frame con year y variable base
#' @param var Variable base para el valor estimado
#' @param margen Margen de crecimiento anual (ej. 0.01 = ±1% acumulativo por año)
#' @param año_base Año a partir del cual se empiezan a expandir los intervalos
#' @return Data frame con columnas: year, estimado, superior, inferior
#' @export
intervals <- function(data, var = "porcentaje", margen = 0.01, año_base = 2022) {
  data <- data %>%
    dplyr::mutate(
      year = as.numeric(year),
      estimado = .data[[var]],
      diferencia = pmax(0, year - año_base),  # 0 si es año base o anterior
      factor = (1 + margen) ^ diferencia,
      superior = estimado * factor,
      inferior = estimado / factor
    ) %>%
    dplyr::select(year, estimado, superior, inferior)

  return(data)
}
