#' Gráfica de línea personalizada para series temporales
#'
#' @param data Data frame con columnas x (eje), y (estimado), superior, inferior
#' @param x, y Variables del gráfico (como strings)
#' @param titulo Título del gráfico
#' @param linea_vertical Año de referencia (línea roja)
#' @param mostrar_intervalo "ninguno", "ambos", "superior", "inferior"
#' @param fuente Nombre de la fuente tipográfica
#' @param etiqueta_x, etiqueta_y Etiquetas personalizadas para los ejes
#' @param mostrar_leyenda Booleano: incluir leyenda o no
#' @param nombre_estimado Etiqueta de la línea estimada (por ejemplo, "Deseable")
#' @param nombre_superior, nombre_inferior Etiquetas para las líneas punteadas
#' @return ggplot
#' @export
grafica_bonita <- function(data, x, y,
                           titulo = "Mi gráfico de línea bonito",
                           linea_vertical = NULL,
                           mostrar_intervalo = c("ninguno", "ambos", "superior", "inferior"),
                           fuente = NULL,
                           etiqueta_x = NULL,
                           etiqueta_y = NULL,
                           mostrar_leyenda = FALSE,
                           nombre_estimado = "Deseable",
                           nombre_superior = "Escenario alto",
                           nombre_inferior = "Escenario bajo") {

  mostrar_intervalo <- match.arg(mostrar_intervalo)

  if (is.null(etiqueta_x)) etiqueta_x <- x
  if (is.null(etiqueta_y)) etiqueta_y <- y

  theme_fuente <- if (!is.null(fuente)) ggplot2::element_text(family = fuente) else ggplot2::element_text()

  # Plot base
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x)) +
    ggplot2::geom_line(ggplot2::aes_string(y = y, color = shQuote(nombre_estimado)), linewidth = 1.5) +
    ggplot2::labs(title = titulo, x = etiqueta_x, y = etiqueta_y) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, size = 30, face = "bold", family = fuente),
      text = theme_fuente,
      axis.text.x = ggplot2::element_text(size = 12, angle = 90, family = fuente),
      legend.title = ggplot2::element_blank()
    )

  # Línea vertical
  if (!is.null(linea_vertical)) {
    p <- p + ggplot2::geom_vline(xintercept = linea_vertical, linetype = "dashed", color = "red", linewidth = 1)
  }

  # Intervalos
  if (mostrar_intervalo == "ambos" && "superior" %in% names(data) && "inferior" %in% names(data)) {
    p <- p +
      ggplot2::geom_line(
        data = dplyr::filter(data, !is.na(superior)),
        ggplot2::aes_string(y = "superior", color = shQuote("Escenarios")),
        linetype = "dotted") +
      ggplot2::geom_line(
        data = dplyr::filter(data, !is.na(inferior)),
        ggplot2::aes_string(y = "inferior", color = shQuote("Escenarios")),
        linetype = "dotted")
  }

  if (mostrar_intervalo == "superior" && "superior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = dplyr::filter(data, !is.na(superior)),
      ggplot2::aes_string(y = "superior", color = shQuote(nombre_superior)),
      linetype = "dotted")
  }

  if (mostrar_intervalo == "inferior" && "inferior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = dplyr::filter(data, !is.na(inferior)),
      ggplot2::aes_string(y = "inferior", color = shQuote(nombre_inferior)),
      linetype = "dotted")
  }

  # Eje X de años
  if (is.numeric(data[[x]])) {
    anios <- sort(unique(data[[x]]))
    p <- p + ggplot2::scale_x_continuous(breaks = seq(min(anios), max(anios), by = 2))
  }

  # Leyenda
  if (!mostrar_leyenda) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}
