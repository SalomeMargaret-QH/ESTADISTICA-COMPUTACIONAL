# ==========================
# Librería: RandomLCG
# Autor: SALOME MARGARET QUISPE HILASACA
# Descripción: Librería simple para generar números aleatorios usando Congruencia Lineal
# ==========================

# Función general: Generador Congruencia Lineal
lcg_random <- function(n, seed = 1, a = 1664525, c = 1013904223, m = 2^32, min = 0, max = 1) {
  x <- numeric(n)
  x[1] <- seed
  for (i in 2:n) {
    x[i] <- (a * x[i - 1] + c) %% m
  }
  scaled <- min + (x / m) * (max - min)
  return(scaled)
}

# Generar Notas (0 a 20)
generar_notas <- function(n, seed = 42) {
  round(lcg_random(n, seed, min = 0, max = 20), 2)
}

# Generar Tallas (1.4 a 1.99 m)
generar_tallas <- function(n, seed = 100) {
  round(lcg_random(n, seed, min = 1.4, max = 1.99), 2)
}

# Generar Edades (0 a 99)
generar_edades <- function(n, seed = 7) {
  round(lcg_random(n, seed, min = 0, max = 99))
}

# Exportar como CSV si se desea
exportar_datos <- function(n = 100, archivo = "datos_simulados.csv") {
  datos <- data.frame(
    Edad = generar_edades(n),
    Talla = generar_tallas(n),
    Nota = generar_notas(n)
  )
  write.csv(datos, archivo, row.names = FALSE)
  message("✅ Archivo CSV generado: ", archivo)
}

# Demostración (puedes comentar estas líneas si solo deseas las funciones)
if (interactive()) {
  cat("Notas simuladas:\n")
  print(generar_notas(5))
  
  cat("\nTallas simuladas:\n")
  print(generar_tallas(5))
  
  cat("\nEdades simuladas:\n")
  print(generar_edades(5))
  
  # Exportar CSV
  exportar_datos(20, "simulacion_usuarios.csv")
}

