# HW02 - Contraste de medias: Petal.Length (versicolor vs virginica)
# Autor: Joana
# Basado en HW02.pdf (Asignación 3)

# 0. Paquetes ----
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
if (!requireNamespace("effsize", quietly = TRUE)) install.packages("effsize")
if (!requireNamespace("ggpubr", quietly = TRUE)) install.packages("ggpubr")
library(tidyverse)
library(effsize)
library(ggpubr)

# 1. Cargar datos ----
data_sub <- subset(iris, Species %in% c("versicolor", "virginica"))

# 2. Subset: solo versicolor y virginica ----
data_sub <- iris %>%
  filter(Species %in% c("versicolor", "virginica")) %>%
  select(Species, Petal.Length)

# 3. Estadística descriptiva ----
desc <- data_sub %>%
  group_by(Species) %>%
  summarise(
    n = n(),
    mean = mean(Petal.Length),
    sd = sd(Petal.Length),
    median = median(Petal.Length),
    IQR = IQR(Petal.Length),
    min = min(Petal.Length),
    max = max(Petal.Length)
  )
print(desc)

# 4. Normalidad (Shapiro-Wilk) ----
shap <- data_sub %>%
  group_by(Species) %>%
  summarise(shapiro_p = shapiro.test(Petal.Length)$p.value)
print(shap)

# 5. Homogeneidad de varianzas (F-test) ----
vtest <- var.test(Petal.Length ~ Species, data = data_sub)
print(vtest)

# 6. T-test (clásico o Welch según varianzas) ----
if (vtest$p.value < 0.05) {
  message("Varianzas diferentes -> Welch t-test")
  t_res <- t.test(Petal.Length ~ Species, data = data_sub, var.equal = FALSE)
} else {
  message("Varianzas iguales -> t-test clásico")
  t_res <- t.test(Petal.Length ~ Species, data = data_sub, var.equal = TRUE)
}
print(t_res)

# 7. Tamaño del efecto (Cohen's d) ----
d_res <- cohen.d(Petal.Length ~ Species, data = data_sub,
                 pooled = TRUE, hedges.correction = TRUE)
print(d_res)

# Cálculo manual por confirmación
group_stats <- data_sub %>% group_by(Species) %>%
  summarise(n = n(), m = mean(Petal.Length), s2 = var(Petal.Length))
n1 <- group_stats$n[1]; n2 <- group_stats$n[2]
m1 <- group_stats$m[1]; m2 <- group_stats$m[2]
s1sq <- group_stats$s2[1]; s2sq <- group_stats$s2[2]
pooled_sd <- sqrt(((n1-1)*s1sq + (n2-1)*s2sq)/(n1+n2-2))
d_manual <- (m1 - m2) / pooled_sd
cat("Cohen's d (manual, pooled):", d_manual, "\n")

# 8. Visualización ----
p <- ggplot(data_sub, aes(x = Species, y = Petal.Length)) +
  geom_violin(trim = FALSE, fill = "lightblue", alpha = 0.4) +
  geom_boxplot(width = 0.12, outlier.shape = NA) +
  geom_jitter(width = 0.12, alpha = 0.6, color = "darkblue") +
  labs(title = "Petal.Length: Versicolor vs Virginica",
       subtitle = "Iris dataset",
       y = "Petal.Length (cm)", x = "Species") +
  theme_minimal()
print(p)