#' Gráfica bonita (línea)
#'
#' @param data Un data.frame
#' @param x Variable en el eje x (como string)
#' @param y Variable en el eje y (como string)
#' @param titulo Título del gráfico
#' @return Un objeto ggplot
#' @export
grafica_bonita <- function(data, x, y, titulo = "Mi gráfico de línea bonito") {
  ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_line(color = "steelblue", size = 1) +
    ggplot2::labs(title = titulo, x = x, y = y) +
    ggplot2::theme_minimal()
}
