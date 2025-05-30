grafica_bonita <- function(data, x, y,
                           titulo = "Mi gráfico de línea bonito",
                           linea_vertical = NULL,
                           mostrar_intervalo = c("ninguno", "ambos", "superior", "inferior"),
                           mostrar_leyenda = FALSE,
                           fuente = NULL,
                           etiqueta_x = NULL,
                           etiqueta_y = NULL,
                           nombre_observado = "Observado",
                           nombre_estimado = "Estimado",
                           nombre_estimado_futuro = "Estimado (futuro)",
                           nombre_intervalo_superior = "Intervalo superior",
                           nombre_intervalo_inferior = "Intervalo inferior") {
  
  mostrar_intervalo <- match.arg(mostrar_intervalo)
  
  if (is.null(etiqueta_x)) etiqueta_x <- x
  if (is.null(etiqueta_y)) etiqueta_y <- y
  
  theme_fuente <- if (!is.null(fuente)) ggplot2::element_text(family = fuente) else ggplot2::element_text()
  
  data <- data %>%
    dplyr::mutate(!!x := as.numeric(.data[[x]]))
  
  # Línea base
  data_plot <- data %>%
    dplyr::mutate(tipo_linea = nombre_estimado)
  
  # Separar futuro si hay línea vertical
  if (!is.null(linea_vertical)) {
    data_plot <- dplyr::mutate(
      data_plot,
      tipo_linea = ifelse(.data[[x]] > linea_vertical, paste0(nombre_estimado, " (futuro)"), nombre_observado)
    )
  }
  
  p <- ggplot2::ggplot(data_plot, ggplot2::aes(x = .data[[x]], y = .data[[y]], color = tipo_linea)) +
    ggplot2::geom_line(size = 1.5) +
    ggplot2::scale_color_manual(
      values = c(
        !!nombre_observado := "#9F2241",
        !!nombre_estimado_futuro := "#027a35",
        !!nombre_estimado := "#9F2241"
      ),
      breaks = c(nombre_estimado, nombre_estimado_futuro, nombre_observado)
    )
  
  if (mostrar_intervalo %in% c("ambos", "superior") && "superior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = dplyr::filter(data, .data[[x]] > linea_vertical),
      ggplot2::aes(x = .data[[x]], y = .data[["superior"]], color = nombre_intervalo_superior),
      linetype = "dotted", size = 1.5
    )
  }
  
  if (mostrar_intervalo %in% c("ambos", "inferior") && "inferior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = dplyr::filter(data, .data[[x]] > linea_vertical),
      ggplot2::aes(x = .data[[x]], y = .data[["inferior"]], color = nombre_intervalo_inferior),
      linetype = "dotted", size = 1.5
    )
  }
  
  if (!is.null(linea_vertical)) {
    p <- p + ggplot2::geom_vline(xintercept = linea_vertical, linetype = "dashed", color = "red", size = 1)
  }
  
  p <- p +
    ggplot2::labs(title = titulo, x = etiqueta_x, y = etiqueta_y, color = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, size = 30, face = "bold", family = fuente),
      text = ggplot2::element_text(size = 20, family = fuente),
      axis.text.x = ggplot2::element_text(size = 12, angle = 90),
      legend.position = ifelse(mostrar_leyenda, "right", "none")
    ) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 15))
  
  return(p)
}
