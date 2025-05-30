#' Gráfica de línea personalizada con intervalos
#'
#' @param data Data frame con los datos
#' @param x Variable para eje x (como string)
#' @param y Variable principal del eje y (como string)
#' @param titulo Título del gráfico
#' @param linea_vertical Año para separar observado/estimado (opcional)
#' @param mostrar_intervalo Opciones: "ninguno", "ambos", "superior", "inferior"
#' @param mostrar_leyenda Mostrar leyenda (TRUE/FALSE)
#' @param nombre_estimado Nombre a mostrar para la línea estimada
#' @param nombre_superior Nombre a mostrar para la línea superior
#' @param nombre_inferior Nombre a mostrar para la línea inferior
#' @param fuente Tipo de letra (nombre de Google Font cargada con showtext)
#' @param etiqueta_x Etiqueta eje x
#' @param etiqueta_y Etiqueta eje y
#'
#' @return Un objeto ggplot
#' @export
grafica_bonita <- function(data,
                           x,
                           y,
                           titulo = "Mi gráfico de línea bonito",
                           linea_vertical = NULL,
                           mostrar_intervalo = c("ninguno", "ambos", "superior", "inferior"),
                           mostrar_leyenda = FALSE,
                           nombre_estimado = "Deseable",
                           nombre_superior = "Escenario alto",
                           nombre_inferior = "Escenario bajo",
                           fuente = NULL,
                           etiqueta_x = NULL,
                           etiqueta_y = NULL) {

  mostrar_intervalo <- match.arg(mostrar_intervalo)

  if (is.null(etiqueta_x)) etiqueta_x <- x
  if (is.null(etiqueta_y)) etiqueta_y <- y

  theme_fuente <- if (!is.null(fuente)) ggplot2::element_text(family = fuente) else ggplot2::element_text()

  # Asegurar que año sea numérico
  data <- data %>% dplyr::mutate({{ x }} := as.numeric(.data[[x]]))

  # Crear tipo_linea para distinguir estimado/observado
  data_plot <- dplyr::mutate(data, tipo_linea = nombre_estimado)

  if (!is.null(linea_vertical)) {
    data_plot <- data_plot %>%
      dplyr::mutate(
        tipo_linea = dplyr::if_else(
          .data[[x]] > linea_vertical,
          nombre_estimado,
          "Observado"
        )
      )
  }

  # Gráfico base
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = .data[[x]], y = .data[[y]], colour = tipo_linea)) +
    ggplot2::geom_line(linewidth = 1.5) +
    ggplot2::scale_color_manual(
      values = c(
        "Observado" = "#9F2241",
        "Deseable" = "#027a35",
        "Escenario bajo" = "#C77CFF",
        "Escenario alto" = "#00BFC4"
      ),
      name = NULL
    ) +
    ggplot2::labs(title = titulo, x = etiqueta_x, y = etiqueta_y) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, size = 30, face = "bold"),
      text = theme_fuente,
      axis.text.x = ggplot2::element_text(size = 12, angle = 90),
      axis.title = ggplot2::element_text(size = 16),
      legend.position = if (mostrar_leyenda) "right" else "none"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(min(data[[x]], na.rm = TRUE),
                                             max(data[[x]], na.rm = TRUE), by = 2))

  # Línea vertical
  if (!is.null(linea_vertical)) {
    p <- p + ggplot2::geom_vline(xintercept = linea_vertical, linetype = "dashed", color = "red", linewidth = 1)
  }

  # Intervalo superior
  if (mostrar_intervalo %in% c("ambos", "superior") && "superior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(x = .data[[x]], y = .data[["superior"]], colour = nombre_superior),
      linetype = "dotted", linewidth = 1.5, show.legend = mostrar_leyenda
    )
  }

  # Intervalo inferior
  if (mostrar_intervalo %in% c("ambos", "inferior") && "inferior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(x = .data[[x]], y = .data[["inferior"]], colour = nombre_inferior),
      linetype = "dotted", linewidth = 1.5, show.legend = mostrar_leyenda
    )
  }

  return(p)
}
