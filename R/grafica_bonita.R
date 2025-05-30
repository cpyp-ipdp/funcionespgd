#' Gráfica de línea personalizada
#'
#' @param data Un data.frame con columnas numéricas
#' @param x Variable del eje X (como string)
#' @param y Variable principal del eje Y (como string)
#' @param titulo Título del gráfico
#' @param linea_vertical Valor opcional para dibujar una línea vertical (ej. un año)
#' @param mostrar_intervalo Qué intervalos mostrar: "ninguno", "ambos", "superior", "inferior"
#' @return Un objeto ggplot
#' @export
grafica_bonita <- function(data, x, y, titulo = "Mi gráfico de línea bonito",
                           linea_vertical = NULL,
                           mostrar_intervalo = c("ninguno", "ambos", "superior", "inferior")) {
  
  mostrar_intervalo <- match.arg(mostrar_intervalo)

  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x, y = y)) +
    ggplot2::geom_line(color = "#691c32", size = 1.5) +
    ggplot2::labs(title = titulo, x = x, y = y) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, size = 30, face = "bold"),
      text = ggplot2::element_text(size = 20),
      axis.text.x = ggplot2::element_text(size = 12, angle = 90)
    )
  
  # Línea vertical
  if (!is.null(linea_vertical)) {
    p <- p + ggplot2::geom_vline(xintercept = linea_vertical, linetype = "dashed", color = "gray40", size = 1)
  }

  # Agregar intervalos si existen
  if (mostrar_intervalo %in% c("ambos", "superior") && "superior" %in% names(data)) {
    p <- p + ggplot2::geom_line(ggplot2::aes_string(y = "superior"), linetype = "dotted", color = "gray50")
  }

  if (mostrar_intervalo %in% c("ambos", "inferior") && "inferior" %in% names(data)) {
    p <- p + ggplot2::geom_line(ggplot2::aes_string(y = "inferior"), linetype = "dotted", color = "gray50")
  }

  return(p)
}
