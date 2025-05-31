grafica_bonita <- function(data, x, y,
                           titulo = "Mi gráfico de línea bonito",
                           linea_vertical = NULL,
                           mostrar_intervalo = c("ninguno", "ambos", "superior", "inferior"),
                           fuente = NULL,
                           etiqueta_x = NULL,
                           etiqueta_y = NULL,
                           mostrar_leyenda = FALSE,
                           nombre_observado = "Observado",
                           nombre_estimado_futuro = "Deseable",
                           nombre_intervalo_superior = "Escenario alto",
                           nombre_intervalo_inferior = "Escenario bajo",
                           titulo_leyenda = "Escenarios",
                           limite_inferior_y = 0) {

  mostrar_intervalo <- match.arg(mostrar_intervalo)

  if (is.null(etiqueta_x)) etiqueta_x <- x
  if (is.null(etiqueta_y)) etiqueta_y <- y

  data[[x]] <- as.numeric(data[[x]])

  data_plot <- dplyr::mutate(data, tipo_linea = nombre_observado)

  if (!is.null(linea_vertical)) {
    data_plot <- dplyr::mutate(
      data_plot,
      tipo_linea = dplyr::case_when(
        .data[[x]] < linea_vertical ~ nombre_observado,
        .data[[x]] == linea_vertical ~ paste0(nombre_observado, "+"),
        .data[[x]] > linea_vertical ~ nombre_estimado_futuro
      )
    )
  }

  p <- ggplot2::ggplot()

  # Observado
  p <- p + ggplot2::geom_line(
    data = data_plot[data_plot$tipo_linea %in% c(nombre_observado, paste0(nombre_observado, "+")), ],
    ggplot2::aes_string(x = x, y = y, color = sprintf('"%s"', nombre_observado)),
    size = 1.5
  )

  # Estimado futuro
  p <- p + ggplot2::geom_line(
    data = data_plot[data_plot$tipo_linea == nombre_estimado_futuro & data_plot[[x]] >= linea_vertical - 1, ],
    ggplot2::aes_string(x = x, y = y, color = sprintf('"%s"', nombre_estimado_futuro)),
    size = 1.5
  )

  # Intervalos
  if (mostrar_intervalo %in% c("ambos", "superior") && "superior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = data[data[[x]] >= linea_vertical - 1, ],
      ggplot2::aes_string(x = x, y = "superior", color = sprintf('"%s"', nombre_intervalo_superior)),
      size = 1.5, linetype = "dotted"
    )
  }

  if (mostrar_intervalo %in% c("ambos", "inferior") && "inferior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = data[data[[x]] >= linea_vertical - 1, ],
      ggplot2::aes_string(x = x, y = "inferior", color = sprintf('"%s"', nombre_intervalo_inferior)),
      size = 1.5, linetype = "dotted"
    )
  }

  if (!is.null(linea_vertical)) {
    p <- p + ggplot2::geom_vline(xintercept = linea_vertical,
                                 linetype = "dashed",
                                 color = "red",
                                 linewidth = 1)
  }

  valores_color <- setNames(
    c("#9F2241", "#027a35", "#BC955C", "#969696"),
    c(nombre_observado, nombre_estimado_futuro, nombre_intervalo_superior, nombre_intervalo_inferior)
  )

  p <- p + ggplot2::labs(
      title = titulo,
      x = etiqueta_x,
      y = etiqueta_y,
      color = titulo_leyenda
    ) +
    ggplot2::scale_x_continuous(breaks = sort(unique(c(seq(min(data[[x]]), max(data[[x]]), 2), 2022)))) +
    ggplot2::scale_y_continuous(limits = c(limite_inferior_y, NA)) +
    ggplot2::scale_color_manual(values = valores_color) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 28, face = "bold", hjust = 0),
      text = ggplot2::element_text(size = 16, family = fuente),
      axis.text.x = ggplot2::element_text(size = 12, angle = 90),
      legend.position = if (mostrar_leyenda) "right" else "none"
    )

  # Etiquetas en puntos clave del estimado futuro
  etiquetas_anos <- c(linea_vertical, 2030, 2035, 2045)
  data_etiquetas <- data_plot[data_plot[[x]] %in% etiquetas_anos &
                                data_plot$tipo_linea == nombre_estimado_futuro, ]

  p <- p + ggplot2::geom_text(
    data = data_etiquetas,
    ggplot2::aes_string(x = x, y = y, label = sprintf("round(%s, 1)", y)),
    vjust = -1,
    size = 4,
    color = "#027a35"
  )

  # Etiquetas en puntos clave del escenario inferior
  if (mostrar_intervalo %in% c("ambos", "inferior") && "inferior" %in% names(data)) {
    data_etiquetas_inf <- data[data[[x]] %in% etiquetas_anos, ]
    p <- p + ggplot2::geom_text(
      data = data_etiquetas_inf,
      ggplot2::aes_string(x = x, y = "inferior", label = "round(inferior, 1)"),
      vjust = 1.8,
      size = 4,
      color = "#969696"
    )
  }

  # Etiquetas en puntos clave del escenario superior
  if (mostrar_intervalo %in% c("ambos", "superior") && "superior" %in% names(data)) {
    data_etiquetas_sup <- data[data[[x]] %in% etiquetas_anos, ]
    p <- p + ggplot2::geom_text(
      data = data_etiquetas_sup,
      ggplot2::aes_string(x = x, y = "superior", label = "round(superior, 1)"),
      vjust = -1.8,
      size = 4,
      color = "#BC955C"
    )
  }

  return(p)
}
