#' Gráfica de línea personalizada para series temporales
#'
#' @param data Un data.frame con columnas numéricas
#' @param x Variable del eje X (como string, usualmente "year")
#' @param y Variable principal del eje Y (como string)
#' @param titulo Título del gráfico
#' @param linea_vertical Valor opcional para dibujar una línea vertical (ej. un año)
#' @param mostrar_intervalo Qué intervalos mostrar: "ninguno", "ambos", "superior", "inferior"
#' @param fuente Nombre de la fuente a aplicar (ej. "pop" para font_add_google("Roboto", "pop"))
#' @param etiqueta_x Etiqueta personalizada para el eje X
#' @param etiqueta_y Etiqueta personalizada para el eje Y
#' @return Un objeto ggplot
#' @export
grafica_bonita <- function(data, x, y,
                           titulo = "Mi gráfico de línea bonito",
                           linea_vertical = NULL,
                           mostrar_intervalo = c("ninguno", "ambos", "superior", "inferior"),
                           fuente = NULL,
                           etiqueta_x = NULL,
                           etiqueta_y = NULL) {

  mostrar_intervalo <- match.arg(mostrar_intervalo)

  # Etiquetas por default si no se especifican
  if (is.null(etiqueta_x)) etiqueta_x <- x
  if (is.null(etiqueta_y)) etiqueta_y <- y

  theme_fuente <- if (!is.null(fuente)) ggplot2::element_text(family = fuente) else ggplot2::element_text()

  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_line(color = "#691c32", size = 1.5) +
    ggplot2::labs(title = titulo, x = etiqueta_x, y = etiqueta_y) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, size = 30, face = "bold", family = fuente),
      text = theme_fuente,
      axis.text.x = ggplot2::element_text(size = 12, angle = 90, family = fuente)
    )

  # Línea vertical
  if (!is.null(linea_vertical)) {
    p <- p + ggplot2::geom_vline(xintercept = linea_vertical, linetype = "dashed", color = "red", size = 1)
  }

  # Intervalos
  if (mostrar_intervalo %in% c("ambos", "superior") && "superior" %in% names(data)) {
    p <- p + ggplot2::geom_line(ggplot2::aes_string(y = "superior"), linetype = "dotted", color = "gray50", size, 1.5)
  }
  if (mostrar_intervalo %in% c("ambos", "inferior") && "inferior" %in% names(data)) {
    p <- p + ggplot2::geom_line(ggplot2::aes_string(y = "inferior"), linetype = "dotted", color = "gray50", size=1.5)
  }

  # Si el eje x es numérico, poner saltos de 2 en 2
  if (is.numeric(data[[x]])) {
    anios <- sort(unique(data[[x]]))
    p <- p + ggplot2::scale_x_continuous(breaks = seq(min(anios), max(anios), by = 2))
  }

  return(p)
}
