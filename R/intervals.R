#' Crea intervalos superior e inferior a partir de una variable base
#'
#' @param df Data frame con variable base
#' @param variable Nombre de la variable a usar como base (string)
#' @param desviacion Margen proporcional (por ejemplo, 0.01)
#'
#' @return Un data frame con columnas `year`, `estimado`, `superior`, `inferior`
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
