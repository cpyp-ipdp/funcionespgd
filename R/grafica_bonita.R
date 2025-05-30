#' Gráfica de línea personalizada y estilizada
#'
#' @param data Data frame que contiene los datos
#' @param x Variable para el eje X (como texto)
#' @param y Variable principal a graficar (como texto)
#' @param titulo Título del gráfico
#' @param linea_vertical Año o valor donde se dibuja línea vertical (opcional)
#' @param mostrar_intervalo Indica qué intervalo mostrar: "ninguno", "ambos", "superior", "inferior"
#' @param fuente Nombre de la fuente (como "Roboto", "pop", etc., si se cargó con showtext)
#' @param etiqueta_x Etiqueta personalizada para eje X
#' @param etiqueta_y Etiqueta personalizada para eje Y
#' @param mostrar_leyenda Booleano para incluir leyenda
#' @param nombre_estimado Nombre para la línea estimada en la leyenda
#' @param nombre_superior Nombre del intervalo superior en la leyenda
#' @param nombre_inferior Nombre del intervalo inferior en la leyenda
#'
#' @return Un objeto ggplot
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

  theme_fuente <- if (!is.null(fuente)) {
    ggplot2::element_text(family = fuente)
  } else {
    ggplot2::element_text()
  }

  # Asegurar que el eje x sea numérico
  data <- data %>% dplyr::mutate(!!x := as.numeric(.data[[x]]))

  # Crear columna de tipo para tramos futuros
  data_plot <- dplyr::mutate(data, tipo_linea = nombre_estimado)

  if (!is.null(linea_vertical)) {
    data_plot <- dplyr::mutate(
      data_plot,
      tipo_linea = ifelse(
        .data[[x]] > linea_vertical,
        paste0(nombre_estimado, " (futuro)"),
        nombre_estimado
      )
    )
  }

  p <- ggplot2::ggplot(data_plot, ggplot2::aes_string(x = x, y = y, colour = "tipo_linea")) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::scale_x_continuous(breaks = seq(min(data_plot[[x]], na.rm = TRUE),
                                             max(data_plot[[x]], na.rm = TRUE), by = 2)) +
    ggplot2::labs(title = titulo, x = etiqueta_x, y = etiqueta_y, colour = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, size = 30, face = "bold"),
      text = theme_fuente,
      axis.text.x = ggplot2::element_text(size = 12, angle = 90),
      legend.position = if (mostrar_leyenda) "right" else "none"
    )

  # Línea vertical opcional
  if (!is.null(linea_vertical)) {
    p <- p + ggplot2::geom_vline(xintercept = linea_vertical,
                                  linetype = "dashed",
                                  color = "red",
                                  linewidth = 1)
  }

  # Agregar líneas de intervalo
  if (mostrar_intervalo %in% c("ambos", "superior") && "superior" %in% names(data_plot)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes_string(y = "superior"),
      data = data_plot,
      color = "gray50",
      linetype = "dotted",
      linewidth = 1.5,
      inherit.aes = FALSE
    )
    if (mostrar_leyenda) {
      p <- p + ggplot2::geom_line(
        ggplot2::aes_string(x = x, y = "superior", colour = shQuote(nombre_superior)),
        data = data_plot,
        linetype = "dotted",
        linewidth = 1.5,
        inherit.aes = FALSE
      )
    }
  }

  if (mostrar_intervalo %in% c("ambos", "inferior") && "inferior" %in% names(data_plot)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes_string(y = "inferior"),
      data = data_plot,
      color = "gray50",
      linetype = "dotted",
      linewidth = 1.5,
      inherit.aes = FALSE
    )
    if (mostrar_leyenda) {
      p <- p + ggplot2::geom_line(
        ggplot2::aes_string(x = x, y = "inferior", colour = shQuote(nombre_inferior)),
        data = data_plot,
        linetype = "dotted",
        linewidth = 1.5,
        inherit.aes = FALSE
      )
    }
  }

  return(p)
}
