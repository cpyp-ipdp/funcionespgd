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

  # Asegurar que año sea numérico
  data <- data %>% dplyr::mutate(!!x := as.numeric(.data[[x]]))

  # Crear tipo_linea para estimado
  data_plot <- dplyr::mutate(data, tipo_linea = nombre_estimado)

  # Separar tramo futuro si hay línea vertical
  if (!is.null(linea_vertical)) {
    data_plot <- dplyr::mutate(
      data_plot,
      tipo_linea = ifelse(.data[[x]] > linea_vertical,
                          paste0(nombre_estimado, " (futuro)"),
                          nombre_estimado)
    )
  }

  # Agregar líneas de intervalo
  if (mostrar_intervalo %in% c("ambos", "superior") && "superior" %in% names(data)) {
    data_plot <- dplyr::bind_rows(
      data_plot,
      data %>% dplyr::filter(!is.na(superior)) %>%
        dplyr::transmute(!!x := .data[[x]], valor = superior, tipo_linea = nombre_superior)
    )
  }

  if (mostrar_intervalo %in% c("ambos", "inferior") && "inferior" %in% names(data)) {
    data_plot <- dplyr::bind_rows(
      data_plot,
      data %>% dplyr::filter(!is.na(inferior)) %>%
        dplyr::transmute(!!x := .data[[x]], valor = inferior, tipo_linea = nombre_inferior)
    )
  }

  # Definir valor principal si no está
  if (!"valor" %in% names(data_plot)) {
    data_plot$valor <- data_plot[[y]]
  }

  # Asignar colores y estilos de línea
  colores <- c(
    nombre_estimado = "#691c32",
    paste0(nombre_estimado, " (futuro)") = "#1b7837",
    nombre_superior = "#888888",
    nombre_inferior = "#888888"
  )
  names(colores) <- c(nombre_estimado,
                      paste0(nombre_estimado, " (futuro)"),
                      nombre_superior,
                      nombre_inferior)

  tipos_linea <- c(
    nombre_estimado = "solid",
    paste0(nombre_estimado, " (futuro)") = "solid",
    nombre_superior = "dotted",
    nombre_inferior = "dotted"
  )
  names(tipos_linea) <- c(nombre_estimado,
                          paste0(nombre_estimado, " (futuro)"),
                          nombre_superior,
                          nombre_inferior)

  # Crear gráfico
  p <- ggplot2::ggplot(data_plot, ggplot2::aes_string(x = x, y = "valor", color = "tipo_linea", linetype = "tipo_linea")) +
    ggplot2::geom_line(linewidth = 1.5) +
    ggplot2::scale_color_manual(values = colores) +
    ggplot2::scale_linetype_manual(values = tipos_linea) +
    ggplot2::labs(title = titulo, x = etiqueta_x, y = etiqueta_y) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0, size = 30, face = "bold", family = fuente),
      text = theme_fuente,
      axis.text.x = ggplot2::element_text(size = 12, angle = 90, family = fuente),
      legend.title = ggplot2::element_blank()
    )

  # Eje X con saltos de 2 años
  if (is.numeric(data[[x]])) {
    p <- p + ggplot2::scale_x_continuous(breaks = seq(min(data[[x]]), max(data[[x]]), by = 2))
  }

  # Línea vertical
  if (!is.null(linea_vertical)) {
    p <- p + ggplot2::geom_vline(xintercept = linea_vertical, linetype = "dashed", color = "red", linewidth = 1)
  }

  # Ocultar leyenda si no se solicita
  if (!mostrar_leyenda) {
    p <- p + ggplot2::theme(legend.position = "none")
  }

  return(p)
}
