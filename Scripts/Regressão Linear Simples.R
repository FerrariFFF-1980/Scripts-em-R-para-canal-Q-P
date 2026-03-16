# Correlação e Regressão Linear Simples ----
## Arquivo: usinagem.xlsx ----

# Evita notação científica ----
options(scipen = 999, digits = 4)

# Carrega os pacotes necessários ----
library(ggplot2)
library(patchwork)
library(car)

# Paleta de cores Okabe-Ito ----
okabe_ito <- c(
  "#E69F00", # laranja
  "#56B4E9", # azul claro
  "#009E73", # verde
  "#F0E442", # amarelo
  "#0072B2", # azul
  "#D55E00", # vermelho
  "#CC79A7", # roxo
  "#000000"  # preto
)

# Estatísticas descritivas ----
summary(usinagem)

# Calcula a matriz de correlação de Pearson ----
matriz_correlacao <- cor(usinagem, method = "pearson")
matriz_correlacao

# Ajusta o modelo linear simples ----
modelo_linear_simples <- lm(Rugosidade_Ra ~ Avanco_mm_min,
                            data = usinagem)

# R² e R² ajustado do modelo linear simples ----
r2_simples <- round(summary(modelo_linear_simples)$r.squared, 4)
r2aj_simples <- round(summary(modelo_linear_simples)$adj.r.squared, 4)

# Gráfico de dispersão ----
ggplot(usinagem, aes(x = Avanco_mm_min, y = Rugosidade_Ra)) +
  geom_point(color = okabe_ito[5], size = 3, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, color = okabe_ito[6],
    fill = okabe_ito[2], linewidth = 1) +
  theme_light() +
  labs(
    title = "Rugosidade vs Avanço",
    subtitle = paste("Dispersão com reta de regressão linear | R² =",
                     r2_simples, "| R² ajustado =", r2aj_simples),
    x = "Avanço (mm/min)", y = "Rugosidade Ra") +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# Resumo do modelo ----
summary(modelo_linear_simples)

# Coeficientes do modelo ----
coef(modelo_linear_simples)

# Intervalos de confiança dos coeficientes ----
confint(modelo_linear_simples)

# Valores ajustados pelo modelo simples ----
usinagem$Ajustado_Simples <- fitted(modelo_linear_simples)

# Resíduos do modelo simples ----
usinagem$Residuo_Simples <- resid(modelo_linear_simples)

# Gráfico resíduos vs ajustados do modelo simples ----
ggplot(usinagem, aes(x = Ajustado_Simples, y = Residuo_Simples)) +
  geom_point(color = okabe_ito[2], size = 3, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = okabe_ito[8],
    linewidth = 0.8) +
  theme_light() +
  labs(
    title = "Resíduos vs Ajustados",
    subtitle = "Modelo linear simples",
    x = "Valores ajustados",
    y = "Resíduos"
  ) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# Predição de valores usando o modelo ajustado ----
predict(modelo_linear_simples,
        newdata = data.frame(Avanco_mm_min = c(120, 150, 180)))