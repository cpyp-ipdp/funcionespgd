#' Gráfica bonita para series estimadas con intervalos
#'
#' @param data Data frame que contiene la serie.
#' @param x Variable para el eje X.
#' @param y Variable para el eje Y.
#' @param titulo Título del gráfico.
#' @param linea_vertical Valor para colocar una línea vertical (por ejemplo, un año).
#' @param mostrar_intervalo "ninguno", "superior", "inferior" o "ambos".
#' @param nombre_observado Etiqueta para los datos observados.
#' @param nombre_estimado_futuro Etiqueta para la proyección estimada.
#' @param nombre_intervalo_superior Etiqueta para el límite superior.
#' @param nombre_intervalo_inferior Etiqueta para el límite inferior.
#' @param mostrar_leyenda Lógico, si se desea mostrar leyenda.
#' @param fuente Fuente de letra.
#' @param etiqueta_x Etiqueta del eje X.
#' @param etiqueta_y Etiqueta del eje Y.
#'
#' @return Objeto ggplot2
#' @export
grafica_bonita <- function(data, x, y,
                           titulo = "Mi gráfico de línea bonito",
                           linea_vertical = NULL,
                           mostrar_intervalo = c("ninguno", "ambos", "superior", "inferior"),
                           nombre_observado = "Observado",
                           nombre_estimado_futuro = "Deseable",
                           nombre_intervalo_superior = "Escenario alto",
                           nombre_intervalo_inferior = "Escenario bajo",
                           mostrar_leyenda = FALSE,
                           fuente = NULL,
                           etiqueta_x = NULL,
                           etiqueta_y = NULL) {
  mostrar_intervalo <- match.arg(mostrar_intervalo)

  if (is.null(etiqueta_x)) etiqueta_x <- x
  if (is.null(etiqueta_y)) etiqueta_y <- y

  if (!is.null(fuente)) {
    theme_fuente <- ggplot2::element_text(family = fuente)
  } else {
    theme_fuente <- ggplot2::element_text()
  }

  # Asegurar que el eje x sea numérico
  data <- data %>% dplyr::mutate({{x}} := as.numeric(.data[[x]]))

  # Crear variable tipo_linea
  data_plot <- dplyr::mutate(data, tipo_linea = nombre_observado)

  # Tramo futuro
  if (!is.null(linea_vertical)) {
    data_plot <- dplyr::mutate(
      data_plot,
      tipo_linea = ifelse(.data[[x]] > linea_vertical,
                          paste0(nombre_estimado_futuro),
                          nombre_observado
      )
    )
  }

  # Base plot
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = .data[[x]], y = .data[[y]], color = tipo_linea)) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::labs(title = titulo, x = etiqueta_x, y = etiqueta_y) +
    ggplot2::scale_x_continuous(breaks = seq(min(data[[x]], na.rm = TRUE), max(data[[x]], na.rm = TRUE), by = 2)) +
    ggplot2::theme_minimal(base_family = fuente) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, size = 30, face = "bold"),
      text = theme_fuente,
      axis.text.x = ggplot2::element_text(size = 12, angle = 90),
      legend.title = ggplot2::element_blank(),
      legend.position = if (mostrar_leyenda) "right" else "none"
    )

  # Línea vertical
  if (!is.null(linea_vertical)) {
    p <- p + ggplot2::geom_vline(xintercept = linea_vertical,
                                 linetype = "dashed", color = "red", linewidth = 1)
  }

  # Agregar líneas de intervalos si existen
  colores <- setNames(c(), c())

  if (mostrar_intervalo %in% c("superior", "ambos") && "superior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = data,
      ggplot2::aes(x = .data[[x]], y = .data[["superior"]], color = nombre_intervalo_superior),
      linetype = "dotted", linewidth = 1.5
    )
    colores[nombre_intervalo_superior] <- "#027a35"
  }

  if (mostrar_intervalo %in% c("inferior", "ambos") && "inferior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = data,
      ggplot2::aes(x = .data[[x]], y = .data[["inferior"]], color = nombre_intervalo_inferior),
      linetype = "dotted", linewidth = 1.5
    )
    colores[nombre_intervalo_inferior] <- "#027a35"
  }

  # Colores principales
  colores[nombre_observado] <- "#9F2241"
  colores[nombre_estimado_futuro] <- "#027a35"

  p <- p + ggplot2::scale_color_manual(values = colores)

  return(p)
}
