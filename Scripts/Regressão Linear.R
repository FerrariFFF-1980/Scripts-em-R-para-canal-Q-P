# Correlação e Regressão Linear ----
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
  "#000000")# preto

# Estatísticas descritivas ----
summary(usinagem)

# Calcula a matriz de correlação de Pearson ----
matriz_correlacao <- cor(usinagem, method = "pearson")
matriz_correlacao

# Ajusta o modelo linear simples ----
modelo_linear_simples <- lm(Rugosidade_Ra ~ Avanco_mm_min,
                            data = usinagem)

# R² - coeficiente de determinação do modelo linear simples ----
r2_simples <- round(summary(modelo_linear_simples)$r.squared, 4)

# Gráfico de Dispersao ----
ggplot(usinagem, aes(x = Avanco_mm_min, y = Rugosidade_Ra)) +
  geom_point(color = okabe_ito[5], size = 3, alpha = 0.85) +
  geom_smooth(method = "lm", se = TRUE, color = okabe_ito[6],
    fill = okabe_ito[2], linewidth = 1) +
  theme_light() +
  labs(
    title = "Rugosidade vs Avanço",
    subtitle = paste("Dispersão com reta de regressão linear | R² =", r2_simples),
    x = "Avanço (mm/min)",
    y = "Rugosidade Ra"
  ) +
  theme(plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    axis.title = element_text(face = "bold"))

# Resumo do modelo ----
summary(modelo_linear_simples)

# Coeficientes do modelo ----
coef(modelo_linear_simples)

# Intervalos de confiança dos coeficientes ----
confint(modelo_linear_simples)

# Valores ajustados ----
usinagem$Ajustado_Simples <- fitted(modelo_linear_simples)

# Resíduos ----
usinagem$Residuo_Simples <- resid(modelo_linear_simples)

# Gráfico dos Resíduos do modelo simples ----
ggplot(usinagem, aes(x = Ajustado_Simples, y = Residuo_Simples)) +
  geom_point(color = okabe_ito[2], size = 3, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = okabe_ito[8], linewidth = 0.8) +
  theme_light() +
  labs(
    title = "Resíduos vs Ajustados",
    subtitle = "Modelo linear simples",
    x = "Valores ajustados",
    y = "Resíduos") +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    axis.title = element_text(face = "bold"))

# Modelo Linear Múltiplo ----

# Ajusta o modelo linear múltiplo ----
modelo_linear_multiplo <- lm(
  Rugosidade_Ra ~
    Avanco_mm_min +
    Rotacao_rpm +
    Desgaste_Ferramenta_mm +
    Temperatura_C +
    Concentracao_Fluido_pct,
  data = usinagem)

# Resumo do modelo ----
summary(modelo_linear_multiplo)

# Coeficientes ----
coef(modelo_linear_multiplo)

# Intervalos de confiança ----
confint(modelo_linear_multiplo)

# ANOVA do modelo ----
anova(modelo_linear_multiplo)

# Valores ajustados ----
usinagem$Ajustado_Multiplo <- fitted(modelo_linear_multiplo)

# Resíduos ----
usinagem$Residuo_Multiplo <- resid(modelo_linear_multiplo)

# Variance Inflaction Factor (VIF) ----
vif(modelo_linear_multiplo)

# R2 - coeficiente de determinação do modelo linear múltiplo
r2_multiplo <- round(summary(modelo_linear_multiplo)$r.squared, 4)

# Gráfico Observados vs. Ajustados ----
ggplot(usinagem, aes(x = Rugosidade_Ra, y = Ajustado_Multiplo)) +
  geom_point(color = okabe_ito[3], size = 3, alpha = 0.85) +
  geom_abline(intercept = 0, slope = 1, color = okabe_ito[6],
    linetype = "dashed", linewidth = 1) +
  theme_light() +
  labs(
    title = "Observado vs Ajustado",
    subtitle = paste("Modelo linear múltiplo | R² =", r2_multiplo),
    x = "Rugosidade observada",
    y = "Rugosidade ajustada") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# Resíduos do Modelo Múltiplo ----
ggplot(usinagem, aes(x=Ajustado_Multiplo, y=Residuo_Multiplo)) +
  geom_point(color = okabe_ito[1], size = 3, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = okabe_ito[8],
             linewidth = 0.8) +
  theme_light() +
  labs(
    title = "Resíduos vs Ajustados",
    subtitle = "Modelo linear múltiplo",
    x = "Valores ajustados",
    y = "Resíduos") +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    axis.title = element_text(face = "bold"))

# Cria um data frame com medidas de diagnóstico do modelo múltiplo ----
diagnosticos <- data.frame(
  Ajustado = fitted(modelo_linear_multiplo),
  Residuo = resid(modelo_linear_multiplo),
  Residuo_Padronizado = rstandard(modelo_linear_multiplo),
  Leverage = hatvalues(modelo_linear_multiplo),
  Distancia_Cook = cooks.distance(modelo_linear_multiplo)
)

# Cria a raiz do valor absoluto do resíduo padronizado ----
diagnosticos$Raiz_Residuo_Padronizado <- sqrt(abs(diagnosticos$Residuo_Padronizado))

# Gráfico 1: resíduos vs ajustados ----
grafico_diag_1 <- ggplot(diagnosticos,
  aes(x = Ajustado, y = Residuo)) +
  geom_point(color = okabe_ito[5], size = 3, alpha = 0.85) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = okabe_ito[8],
             linewidth = 0.8) +
  geom_smooth(method = "loess", se = FALSE, # LOESS = Locally Estimated Scatterplot Smoothing ----
              color = okabe_ito[6],
              linewidth = 1) +
  theme_light() +
  labs(
    title = "Resíduos vs Ajustados",
    x = "Valores ajustados",
    y = "Resíduos") +
  theme(plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# Gráfico 2: Q-Q plot ----
grafico_diag_2 <- ggplot(diagnosticos,
                         aes(sample = Residuo_Padronizado)) +
  stat_qq(color = okabe_ito[3], size = 2.5, alpha = 0.85) +
  stat_qq_line(color = okabe_ito[6], linewidth = 1) +
  theme_light() +
  labs(
    title = "Q-Q Plot",
    x = "Quantis teóricos",
    y = "Quantis observados") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"))

# Gráfico 3: Scale-Location ----
grafico_diag_3 <- ggplot(diagnosticos,
  aes(x = Ajustado, y = Raiz_Residuo_Padronizado)) +
  geom_point(color = okabe_ito[2], size = 3, alpha = 0.85) +
  geom_smooth(method = "loess", se = FALSE,
              color = okabe_ito[6], linewidth = 1) +
  theme_light() +
  labs(
    title = "Scale-Location",
    x = "Valores ajustados",
    y = "√|Resíduo padronizado|") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"))

# Gráfico 4: resíduos padronizados vs leverage ----
grafico_diag_4 <- ggplot(diagnosticos,
  aes(x = Leverage, y = Residuo_Padronizado)) +
  geom_point(aes(size = Distancia_Cook), color = okabe_ito[7],
                 alpha = 0.85) +
  geom_hline(yintercept = c(-2, 0, 2),
             linetype = c("dashed", "solid", "dashed"),
             color = okabe_ito[8],
             linewidth = c(0.8, 0.6, 0.8)) +
  theme_light() +
  labs(
    title = "Resíduos vs Leverage",
    x = "Leverage",
    y = "Resíduo padronizado",
    size = "Cook") +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"))

# Combina os quatro gráficos em um painel único ----
painel_diagnosticos <- (grafico_diag_1 + grafico_diag_2 +
    grafico_diag_3 + grafico_diag_4) +
  plot_annotation(
    title = "Diagnósticos do Modelo Linear Múltiplo",
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5,
                                size = 14)))
painel_diagnosticos

# Comparação entre modelos ----
summary(modelo_linear_simples)$r.squared
summary(modelo_linear_simples)$adj.r.squared

summary(modelo_linear_multiplo)$r.squared
summary(modelo_linear_multiplo)$adj.r.squared

# Previsões de novos valores usando o modelo ----
novos_dados <- data.frame(
  Avanco_mm_min = c(175, 190, 205),
  Rotacao_rpm = c(2500, 2400, 2300),
  Desgaste_Ferramenta_mm = c(0.12, 0.18, 0.24),
  Temperatura_C = c(41.5, 45.0, 48.5),
  Concentracao_Fluido_pct = c(6.4, 6.0, 5.8))

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

