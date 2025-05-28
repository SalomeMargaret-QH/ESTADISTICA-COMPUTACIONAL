
# ------------------------
# Instalar paquetes si faltan
# ------------------------
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("car")) install.packages("car")
library(dplyr)
library(ggplot2)
library(car)

# ------------------------
# Simular datos de dos grupos
# ------------------------
set.seed(123)
datos <- data.frame(
  grupo = rep(c("Grupo A", "Grupo B"), each = 50),
  valor = c(rnorm(50, mean = 100, sd = 10),
            rnorm(50, mean = 110, sd = 12))
)

# ------------------------
# Estadística descriptiva
# ------------------------
resumen <- datos %>%
  group_by(grupo) %>%
  summarise(
    media = mean(valor),
    mediana = median(valor),
    sd = sd(valor),
    n = n()
  )
print("📊 Estadística descriptiva:")
print(resumen)

# ------------------------
# Gráficos comparativos
# ------------------------
boxplot(valor ~ grupo, data = datos, col = c("skyblue", "lightgreen"),
        main = "Comparación de valores por grupo", ylab = "Valor")

ggplot(datos, aes(x = grupo, y = valor, fill = grupo)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  labs(title = "Gráfico de violín + boxplot", y = "Valor")

# ------------------------
# Pruebas de normalidad
# ------------------------
cat("\n🔍 Prueba de normalidad (Shapiro-Wilk):\n")
grupo_A <- datos$valor[datos$grupo == "Grupo A"]
grupo_B <- datos$valor[datos$grupo == "Grupo B"]
print(shapiro.test(grupo_A))
print(shapiro.test(grupo_B))

# ------------------------
# Prueba de igualdad de varianzas
# ------------------------
cat("\n⚖️ Prueba de Levene:\n")
print(leveneTest(valor ~ grupo, data = datos))

# ------------------------
# Prueba de hipótesis (t-test o Wilcoxon)
# ------------------------
norm_A <- shapiro.test(grupo_A)$p.value > 0.05
norm_B <- shapiro.test(grupo_B)$p.value > 0.05
igual_varianza <- leveneTest(valor ~ grupo, data = datos)$`Pr(>F)`[1] > 0.05

cat("\n📌 Prueba de hipótesis:\n")
if (norm_A && norm_B) {
  cat("✅ Ambos grupos tienen distribución normal.\n")
  prueba_t <- if (igual_varianza) {
    t.test(valor ~ grupo, data = datos, var.equal = TRUE)
  } else {
    t.test(valor ~ grupo, data = datos, var.equal = FALSE)
  }
  print(prueba_t)
} else {
  cat("🚨 No hay normalidad en los datos. Se aplica prueba de Wilcoxon (Mann–Whitney).\n")
  print(wilcox.test(valor ~ grupo, data = datos))
}

