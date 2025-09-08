grafica_bonita <- function(data, x, y,
                           titulo = "Mi gráfico de línea bonito",
                           linea_vertical = NULL,
                           mostrar_intervalo = c("ninguno", "ambos", "superior", "inferior"),
                           fuente = NULL,
                           etiqueta_x = NULL,
                           etiqueta_y = NULL,
                           mostrar_leyenda = FALSE,
                           ano_base = NULL,
                           mostrar_etiqueta_ano_base = TRUE,
                           desplazamiento_ano_base = c(0, 0),  # ← nuevo parámetro
                           anios_etiquetas = c(2030, 2035, 2045),
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
  
  # Línea observada
  p <- p + ggplot2::geom_line(
    data = data_plot[data_plot$tipo_linea %in% c(nombre_observado, paste0(nombre_observado, "+")), ],
    ggplot2::aes_string(x = x, y = y, color = sprintf('"%s"', nombre_observado)),
    size = 2
  )
  
  # Línea futura estimada
  p <- p + ggplot2::geom_line(
    data = data_plot[data_plot$tipo_linea == nombre_estimado_futuro & data_plot[[x]] >= linea_vertical - 1, ],
    ggplot2::aes_string(x = x, y = y, color = sprintf('"%s"', nombre_estimado_futuro)),
    size = 2
  )
  
  # Intervalo superior
  if (mostrar_intervalo %in% c("ambos", "superior") && "superior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = data[data[[x]] >= linea_vertical - 1, ],
      ggplot2::aes_string(x = x, y = "superior", color = sprintf('"%s"', nombre_intervalo_superior)),
      size = 2, linetype = "dotted"
    )
  }
  
  # Intervalo inferior
  if (mostrar_intervalo %in% c("ambos", "inferior") && "inferior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = data[data[[x]] >= linea_vertical - 1, ],
      ggplot2::aes_string(x = x, y = "inferior", color = sprintf('"%s"', nombre_intervalo_inferior)),
      size = 2, linetype = "dotted"
    )
  }
  
  # Línea vertical
  if (!is.null(linea_vertical)) {
    p <- p + ggplot2::geom_vline(xintercept = linea_vertical,
                                 linetype = "dashed",
                                 color = "red",
                                 linewidth = 1)
  }
  
  # Asignación de colores
  valores_color <- setNames(
    c(
      "#9F2241",  # Observado
      "#027a35",  # Deseable
      ifelse(nombre_intervalo_superior == "Transformador", "#BC955C", "#969696"),  # Superior
      ifelse(nombre_intervalo_inferior == "Transformador", "#BC955C", "#969696")   # Inferior
    ),
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
    ggplot2::theme_classic()() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 28, face = "bold", hjust = 0),
      text = ggplot2::element_text(size = 16, family = fuente),
      axis.text.x = ggplot2::element_text(size = 16, angle = 90),
      axis.text.y= ggplot2::element_text(size = 16),
      legend.ticks = ggplot2::element_text(size=16),
      legend.position = if (mostrar_leyenda) "right" else "none"
    )
  
  # Etiquetas: deseable
  data_etiquetas <- data_plot[data_plot[[x]] %in% anios_etiquetas & data_plot$tipo_linea == nombre_estimado_futuro, ]
  p <- p + ggplot2::geom_text(
    data = data_etiquetas,
    ggplot2::aes_string(x = x, y = y, label = sprintf("round(%s, 2)", y)),
    vjust = -1,
    size = 6.5,
    color = "#027a35",
      fontface = "bold"
  )
  
  # Etiquetas: inferior
  if (mostrar_intervalo %in% c("ambos", "inferior") && "inferior" %in% names(data)) {
    data_etiquetas_inf <- data[data[[x]] %in% anios_etiquetas, ]
    p <- p + ggplot2::geom_text(
      data = data_etiquetas_inf,
      ggplot2::aes_string(x = x, y = "inferior", label = "round(inferior, 2)"),
      vjust = 1.8,
      size = 6.5,
        fontface = "bold",
      color = ifelse(nombre_intervalo_inferior == "Transformador", "#BC955C", "#969696")
    )
  }
  
  # Etiquetas: superior
  if (mostrar_intervalo %in% c("ambos", "superior") && "superior" %in% names(data)) {
    data_etiquetas_sup <- data[data[[x]] %in% anios_etiquetas, ]
    p <- p + ggplot2::geom_text(
      data = data_etiquetas_sup,
      ggplot2::aes_string(x = x, y = "superior", label = "round(superior, 2)"),
      vjust = -1.8,
      size = 6.5,
        fontface = "bold",
      color = ifelse(nombre_intervalo_superior == "Transformador", "#BC955C", "#969696")
    )
  }
  
  # Etiqueta del año base con desplazamiento
  if (mostrar_etiqueta_ano_base) {
    data_etiqueta_base <- data_plot[data_plot[[x]] == ano_base & data_plot$tipo_linea %in% c(nombre_observado, paste0(nombre_observado, "+")), ]
    
    if (nrow(data_etiqueta_base) > 0) {
      data_etiqueta_base_desplazada <- data_etiqueta_base
      data_etiqueta_base_desplazada[[x]] <- ano_base + desplazamiento_ano_base[1]
      data_etiqueta_base_desplazada[[y]] <- data_etiqueta_base_desplazada[[y]] + desplazamiento_ano_base[2]
      
      p <- p + ggplot2::geom_text(
        data = data_etiqueta_base_desplazada,
        ggplot2::aes_string(x = x, y = y, label = sprintf("round(%s, 2)", y)),
        size = 6.5,
        color = "#9F2241",
        fontface = "bold"
      )
    }
  }
  
  return(p)
}
