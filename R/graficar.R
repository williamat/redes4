# =====================================================

# 3. FUNCION PARA GRAFICAR EL PERCEPTRON

# =====================================================
library(ggplot2)
graficar_perceptron <- function(modelo, X, y, predicciones) {

  # Preparar los datos para la gráfica

  datos_grafica <- data.frame(

    horas = X[, 1],

    asistencia = X[, 2],

    Real = factor(

      y,

      levels = c(0, 1),

      labels = c("No aprueba", "Aprueba")

    ),

    Prediccion = predicciones

  )

  # Identificar clasificaciones correctas e incorrectas

  datos_grafica$Resultado <- ifelse(

    y == predicciones,

    "Correcta",

    "Incorrecta"

  )

  # Crear una cuadrícula para dibujar la frontera

  rejilla <- expand.grid(

    horas = seq(

      min(X[, 1]) - 0.5,

      max(X[, 1]) + 0.5,

      length.out = 150

    ),

    asistencia = seq(

      min(X[, 2]) - 2,

      max(X[, 2]) + 2,

      length.out = 150

    )

  )

  # Calcular la suma ponderada en cada punto

  rejilla_estandarizada <- scale(

    as.matrix(rejilla[, c("horas", "asistencia")]),

    center = modelo$medias,

    scale = modelo$desviaciones

  )

  rejilla$z <- as.vector(

    rejilla_estandarizada %*% modelo$pesos +

      modelo$sesgo

  )

  # Construir la gráfica

  grafica <- ggplot() +

    # Frontera de decisión del perceptrón

    geom_contour(

      data = rejilla,

      aes(x = horas, y = asistencia, z = z),

      breaks = 0,

      color = "black",

      linewidth = 1.1

    ) +

    # Puntos de los estudiantes

    geom_point(

      data = datos_grafica,

      aes(

        x = horas,

        y = asistencia,

        fill = Real,

        shape = Resultado

      ),

      size = 3.5,

      color = "black",

      stroke = 0.8

    ) +

    scale_fill_manual(

      values = c(

        "No aprueba" = "#3498DB",

        "Aprueba" = "#2ECC71"

      ),

      name = "Resultado real"

    ) +

    scale_shape_manual(

      values = c(

        "Correcta" = 21,

        "Incorrecta" = 24

      ),

      name = "Clasificación"

    ) +

    labs(

      title = "Clasificación con un perceptrón simple",

      subtitle = "La línea negra representa la frontera de decisión",

      x = "Horas de estudio",

      y = "Porcentaje de asistencia"

    ) +

    theme_minimal(base_size = 13) +

    theme(

      plot.title = element_text(

        face = "bold",

        size = 16

      ),

      legend.position = "bottom"

    )

  print(grafica)

  # Devolver también los datos y la gráfica

  invisible(list(

    grafica = grafica,

    datos = datos_grafica,

    frontera = rejilla

  ))

}
