# Correlação e Regressão Linear Multipla ----
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

# Modelo linear múltiplo ----

# Ajusta o modelo linear múltiplo ----
modelo_linear_multiplo <- lm(Rugosidade_Ra ~ Avanco_mm_min +
                               Rotacao_rpm + Temperatura_C,
                             data = usinagem)

# Resumo do modelo ----
summary(modelo_linear_multiplo)

# Coeficientes do modelo ----
coef(modelo_linear_multiplo)

# Intervalos de confiança dos coeficientes ----
confint(modelo_linear_multiplo)

# ANOVA do modelo ----
anova(modelo_linear_multiplo)

# Valores ajustados pelo modelo múltiplo ----
usinagem$Ajustado_Multiplo <- fitted(modelo_linear_multiplo)

# Resíduos do modelo múltiplo ----
usinagem$Residuo_Multiplo <- resid(modelo_linear_multiplo)

# Fator de Inflação da Variância (VIF) ----
vif(modelo_linear_multiplo)

# R² e R² ajustado do modelo linear múltiplo ----
r2_multiplo <- round(summary(modelo_linear_multiplo)$r.squared, 4)
r2aj_multiplo <- round(summary(modelo_linear_multiplo)$adj.r.squared, 4)

# Gráfico observado vs ajustado ----
ggplot(usinagem, aes(x = Rugosidade_Ra, y = Ajustado_Multiplo)) +
  geom_point(color = okabe_ito[3], size = 3, alpha = 0.85) +
  geom_abline(intercept = 0, slope = 1, color = okabe_ito[6],
    linetype = "dashed", linewidth = 1) +
  theme_light() +
  labs(
    title = "Observado vs Ajustado",
    subtitle = paste("Modelo linear múltiplo | R² =", r2_multiplo,
                     "| R² ajustado =", r2aj_multiplo),
    x = "Rugosidade observada", y = "Rugosidade ajustada")

# Gráfico resíduos vs ajustados do modelo múltiplo ----
ggplot(usinagem, aes(x = Ajustado_Multiplo, y = Residuo_Multiplo)) +
  geom_point(color = okabe_ito[1], size = 3, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = okabe_ito[8],
    linewidth = 0.8) +
  theme_light() +
  labs(
    title = "Resíduos vs Ajustados",
    subtitle = "Modelo linear múltiplo",
    x = "Valores ajustados",
    y = "Resíduos")

# Cria um data frame com medidas de diagnóstico do modelo múltiplo ----
diagnosticos <- data.frame(
  Ajustado = fitted(modelo_linear_multiplo),
  Residuo = resid(modelo_linear_multiplo),
  Residuo_Padronizado = rstandard(modelo_linear_multiplo),
  Leverage = hatvalues(modelo_linear_multiplo),
  Distancia_Cook = cooks.distance(modelo_linear_multiplo))

# Gráfico 1: resíduos vs ajustados ----
grafico_diag_1 <- ggplot(diagnosticos, aes(x = Ajustado, y = Residuo)) +
  geom_point(color = okabe_ito[5], size = 3, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed", color = okabe_ito[8],
    linewidth = 0.8) +
  theme_light() +
  labs(title = "Resíduos vs Ajustados",
       x = "Valores ajustados", y = "Resíduos")

# Gráfico 2: Q-Q plot dos resíduos padronizados ----
grafico_diag_2 <- ggplot(diagnosticos, aes(sample = Residuo_Padronizado)) +
  stat_qq(color = okabe_ito[3], size = 2.5, alpha = 0.85) +
  stat_qq_line(color = okabe_ito[6], linewidth = 1) +
  theme_light() +
  labs(title = "Q-Q Plot",
       x = "Quantis teóricos",
       y = "Quantis observados")

# Gráfico 3: histograma dos resíduos padronizados com curva normal ----
grafico_diag_3 <- ggplot(diagnosticos, aes(x = Residuo_Padronizado)) +
  geom_histogram(aes(y = after_stat(density)), bins = 10, fill = okabe_ito[2],
    color = okabe_ito[8], alpha = 0.85) +
  stat_function(fun = dnorm, args = list(
      mean = mean(diagnosticos$Residuo_Padronizado),
      sd = sd(diagnosticos$Residuo_Padronizado)),
    color = okabe_ito[6], linewidth = 1) +
  theme_light() +
  labs(title = "Histograma dos Resíduos Padronizados",
       x = "Resíduo padronizado", y = "Densidade")

# Gráfico 4: resíduos padronizados vs leverage ----
grafico_diag_4 <- ggplot(diagnosticos,
                         aes(x = Leverage, y = Residuo_Padronizado)) +
  geom_point(aes(size = Distancia_Cook), color = okabe_ito[7], alpha = 0.85) +
  geom_hline(yintercept = c(-2, 0, 2),
             linetype = c("dashed", "solid", "dashed"),
             color = okabe_ito[8],
             linewidth = c(0.8, 0.6, 0.8)) +
  theme_light() +
  labs(title = "Resíduos vs Leverage",
       x = "Leverage", y = "Resíduo padronizado", size = "Cook")

# Combina os quatro gráficos em um painel único ----
painel_diagnosticos <- (grafico_diag_1 + grafico_diag_2 +
                          grafico_diag_3 + grafico_diag_4) +
  plot_annotation(
    title = "Diagnósticos do Modelo Linear Múltiplo",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 14)))

painel_diagnosticos

summary(modelo_linear_multiplo)$r.squared
summary(modelo_linear_multiplo)$adj.r.squared

# Previsões de novos valores usando o modelo ----
novos_dados <- data.frame(
  Avanco_mm_min           = c(175,  190,  205),
  Rotacao_rpm             = c(2500, 2400, 2300),
  Temperatura_C           = c(41.5, 45.0, 48.5))

# Previsões pontuais ----
predict(modelo_linear_multiplo, newdata = novos_dados)

# Intervalos de confiança para a média ----
predict(modelo_linear_multiplo,
        newdata = novos_dados,
        interval = "confidence")

# Intervalos de predição para novas observações ----
predict(modelo_linear_multiplo,
        newdata = novos_dados,
        interval = "prediction")
