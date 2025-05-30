#' Gráfico de línea con intervalos personalizados
#'
#' @param data Un data.frame con los datos a graficar
#' @param x Nombre de la variable del eje x (como string)
#' @param y Nombre de la variable de valores principales (como string)
#' @param titulo Título del gráfico
#' @param linea_vertical Valor numérico en x donde colocar una línea vertical (opcional)
#' @param mostrar_intervalo "ninguno", "superior", "inferior" o "ambos"
#' @param nombre_estimado Etiqueta del estimado deseable
#' @param nombre_superior Etiqueta del intervalo superior
#' @param nombre_inferior Etiqueta del intervalo inferior
#' @param mostrar_leyenda TRUE/FALSE para mostrar leyenda
#' @param fuente Fuente para el texto (opcional, Google Fonts)
#' @param etiqueta_x Etiqueta del eje x
#' @param etiqueta_y Etiqueta del eje y
#'
#' @return Objeto ggplot
#' @export
grafica_bonita <- function(data, x, y,
                           titulo = "Mi gráfico de línea bonito",
                           linea_vertical = NULL,
                           mostrar_intervalo = c("ninguno", "ambos", "superior", "inferior"),
                           nombre_estimado = "Observado",
                           nombre_superior = "Escenario alto",
                           nombre_inferior = "Escenario bajo",
                           mostrar_leyenda = FALSE,
                           fuente = NULL,
                           etiqueta_x = NULL,
                           etiqueta_y = NULL) {

  mostrar_intervalo <- match.arg(mostrar_intervalo)

  if (is.null(etiqueta_x)) etiqueta_x <- x
  if (is.null(etiqueta_y)) etiqueta_y <- y

  theme_fuente <- if (!is.null(fuente)) ggplot2::element_text(family = fuente) else ggplot2::element_text()

  # Forzar año como numérico
  data <- data %>% dplyr::mutate(!!x := as.numeric(.data[[x]]))

  # Crear tipo_linea
  data_plot <- dplyr::mutate(data, tipo_linea = nombre_estimado)

  if (!is.null(linea_vertical)) {
    data_plot <- dplyr::mutate(
      data_plot,
      tipo_linea = ifelse(.data[[x]] > linea_vertical,
                          paste0(nombre_estimado, " (futuro)"),
                          nombre_estimado)
    )
  }

  # Base del gráfico
  p <- ggplot2::ggplot(data_plot, ggplot2::aes_string(x = x, y = y, colour = "tipo_linea")) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::labs(title = titulo, x = etiqueta_x, y = etiqueta_y) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, size = 30, face = "bold"),
      text = theme_fuente,
      axis.text.x = ggplot2::element_text(size = 12, angle = 90),
      legend.title = ggplot2::element_blank(),
      legend.position = ifelse(mostrar_leyenda, "right", "none")
    ) +
    ggplot2::scale_x_continuous(breaks = seq(min(data_plot[[x]], na.rm = TRUE),
                                             max(data_plot[[x]], na.rm = TRUE),
                                             by = 2)) +
    ggplot2::scale_color_manual(
      values = c(
        nombre_estimado = "#9F2241",
        `Deseable` = "#9F2241",
        !!nombre_superior := "#027a35",
        !!nombre_inferior := "#027a35"
      )
    )

  # Línea vertical opcional
  if (!is.null(linea_vertical)) {
    p <- p + ggplot2::geom_vline(xintercept = linea_vertical,
                                 linetype = "dashed", color = "red", linewidth = 1)
  }

  # Intervalos
  if (mostrar_intervalo %in% c("ambos", "superior") && "superior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = data,
      ggplot2::aes_string(x = x, y = "superior", colour = shQuote(nombre_superior)),
      linetype = "dotted", linewidth = 1.5
    )
  }

  if (mostrar_intervalo %in% c("ambos", "inferior") && "inferior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = data,
      ggplot2::aes_string(x = x, y = "inferior", colour = shQuote(nombre_inferior)),
      linetype = "dotted", linewidth = 1.5
    )
  }

  # Filtrar niveles no usados en la leyenda
  if (!mostrar_leyenda) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}
