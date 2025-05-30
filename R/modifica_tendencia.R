#' Modifica tendencia con ajuste lineal
#'
#' @param data Un data.frame
#' @param x Columna x (como string)
#' @param y Columna y (como string)
#' @return Un data.frame con la tendencia ajustada
#' @export
modifica_tendencia <- function(data, x, y) {
  data <- dplyr::mutate(data, .fitted = stats::lm(as.formula(paste(y, '~', x)), data = data)$fitted.values)
  return(data)
}
