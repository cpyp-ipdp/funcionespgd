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
  
  # Separar tramos antes y después del año de corte
  if (!is.null(linea_vertical)) {
    data_pasado <- dplyr::filter(data, !!rlang::sym(x) <= linea_vertical)
    data_futuro <- dplyr::filter(data, !!rlang::sym(x) > linea_vertical)
  } else {
    data_pasado <- data
    data_futuro <- NULL
  }
  
  p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x)) +
    
    # Línea estimada antes del año vertical
    ggplot2::geom_line(data = data_pasado,
                       ggplot2::aes_string(y = y, color = shQuote(nombre_estimado)),
                       linewidth = 1.5) +
    
    # Línea estimada después del año vertical (si hay)
    if (!is.null(data_futuro) && nrow(data_futuro) > 0)
      ggplot2::geom_line(data = data_futuro,
                         ggplot2::aes_string(y = y, color = shQuote(paste0(nombre_estimado, " (futuro)"))),
                         linewidth = 1.5)
  else NULL +

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
  if (mostrar_intervalo == "ambos") {
    if ("superior" %in% names(data)) {
      p <- p + ggplot2::geom_line(
        data = dplyr::filter(data, !is.na(superior)),
        ggplot2::aes_string(y = "superior", color = shQuote("Escenarios")),
        linetype = "dotted", linewidth = 1.5
      )
    }
    if ("inferior" %in% names(data)) {
      p <- p + ggplot2::geom_line(
        data = dplyr::filter(data, !is.na(inferior)),
        ggplot2::aes_string(y = "inferior", color = shQuote("Escenarios")),
        linetype = "dotted", linewidth = 1.5
      )
    }
  } else if (mostrar_intervalo == "superior" && "superior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = dplyr::filter(data, !is.na(superior)),
      ggplot2::aes_string(y = "superior", color = shQuote(nombre_superior)),
      linetype = "dotted", linewidth = 1.5
    )
  } else if (mostrar_intervalo == "inferior" && "inferior" %in% names(data)) {
    p <- p + ggplot2::geom_line(
      data = dplyr::filter(data, !is.na(inferior)),
      ggplot2::aes_string(y = "inferior", color = shQuote(nombre_inferior)),
      linetype = "dotted", linewidth = 1.5
    )
  }
  
  # Eje X cada 2 años
  if (is.numeric(data[[x]])) {
    anios <- sort(unique(data[[x]]))
    p <- p + ggplot2::scale_x_continuous(breaks = seq(min(anios), max(anios), by = 2))
  }
  
  # Ocultar leyenda si no se solicita
  if (!mostrar_leyenda) {
    p <- p + ggplot2::theme(legend.position = "none")
  }
  
  return(p)
}
