#################################
# CORRELAÇÃO E REGRESSÃO LINEAR #
# Arquivo: usinagem.xlsx        #
#################################

# Evita notação científica
options(scipen = 999, digits = 4)

# Carrega os pacotes necessários
library(readxl)
library(ggplot2)
library(patchwork)
library(car)

# Importa o arquivo Excel
dados <- read_excel("Dados/usinagem.xlsx")

# Paleta amigável para daltônicos
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

# Converte para data frame base
dados <- as.data.frame(dados)

# Mostra as primeiras linhas
head(dados)

# Mostra a estrutura dos dados
str(dados)

# Estatísticas descritivas
summary(dados)

# Nomes das colunas
names(dados)

# Converte explicitamente as colunas para numéricas
# Isso evita problemas caso o Excel traga números como texto
dados$Rugosidade_Ra <- as.numeric(dados$Rugosidade_Ra)
dados$Avanco_mm_min <- as.numeric(dados$Avanco_mm_min)
dados$Rotacao_rpm <- as.numeric(dados$Rotacao_rpm)
dados$Desgaste_Ferramenta_mm <- as.numeric(dados$Desgaste_Ferramenta_mm)
dados$Temperatura_C <- as.numeric(dados$Temperatura_C)
dados$Concentracao_Fluido_pct <- as.numeric(dados$Concentracao_Fluido_pct)

# Confere novamente a estrutura
str(dados)

# Seleciona apenas as colunas numéricas relevantes
dados_correlacao <- dados[, c(
  "Rugosidade_Ra",
  "Avanco_mm_min",
  "Rotacao_rpm",
  "Desgaste_Ferramenta_mm",
  "Temperatura_C",
  "Concentracao_Fluido_pct"
)]

# Calcula a matriz de correlação de Pearson
matriz_correlacao <- cor(
  dados_correlacao,
  method = "pearson"
)

# Exibe a matriz
matriz_correlacao

# Correlação entre rugosidade e avanço
teste_cor_avanco <- cor.test(
  dados$Rugosidade_Ra,
  dados$Avanco_mm_min,
  method = "pearson"
)
teste_cor_avanco

# Correlação entre rugosidade e rotação
teste_cor_rotacao <- cor.test(
  dados$Rugosidade_Ra,
  dados$Rotacao_rpm,
  method = "pearson"
)
teste_cor_rotacao

# Correlação entre rugosidade e desgaste
teste_cor_desgaste <- cor.test(
  dados$Rugosidade_Ra,
  dados$Desgaste_Ferramenta_mm,
  method = "pearson"
)
teste_cor_desgaste

# Correlação entre rugosidade e temperatura
teste_cor_temperatura <- cor.test(
  dados$Rugosidade_Ra,
  dados$Temperatura_C,
  method = "pearson"
)
teste_cor_temperatura

# Correlação entre rugosidade e concentração do fluido
teste_cor_fluido <- cor.test(
  dados$Rugosidade_Ra,
  dados$Concentracao_Fluido_pct,
  method = "pearson"
)
teste_cor_fluido

# Gráfico de Dispersao
ggplot(
  dados,
  aes(x = Avanco_mm_min, y = Rugosidade_Ra)
) +
  geom_point(
    color = okabe_ito[5],
    size = 3,
    alpha = 0.85
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = okabe_ito[6],
    fill = okabe_ito[2],
    linewidth = 1
  ) +
  theme_light() +
  labs(
    title = "Rugosidade vs Avanço",
    subtitle = "Dispersão com reta de regressão linear",
    x = "Avanço (mm/min)",
    y = "Rugosidade Ra"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# Ajusta o modelo linear simples
modelo_linear_simples <- lm(
  Rugosidade_Ra ~ Avanco_mm_min,
  data = dados
)

# Resumo do modelo
summary(modelo_linear_simples)

# Coeficientes do modelo
coef(modelo_linear_simples)

# Intervalos de confiança dos coeficientes
confint(modelo_linear_simples)

# Valores ajustados
dados$Ajustado_Simples <- fitted(modelo_linear_simples)

# Resíduos
dados$Residuo_Simples <- resid(modelo_linear_simples)

#Resíduos do modelo simples
ggplot(
  dados,
  aes(x = Ajustado_Simples, y = Residuo_Simples)
) +
  geom_point(
    color = okabe_ito[7],
    size = 3,
    alpha = 0.85
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = okabe_ito[8],
    linewidth = 0.8
  ) +
  theme_light() +
  labs(
    title = "Resíduos vs Ajustados",
    subtitle = "Modelo linear simples",
    x = "Valores ajustados",
    y = "Resíduos"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )


# MODELO LINEAR MÚLTIPLO
##########################

# Ajusta o modelo linear múltiplo
modelo_linear_multiplo <- lm(
  Rugosidade_Ra ~
    Avanco_mm_min +
    Rotacao_rpm +
    Desgaste_Ferramenta_mm +
    Temperatura_C +
    Concentracao_Fluido_pct,
  data = dados
)

# Resumo do modelo
summary(modelo_linear_multiplo)

# Coeficientes
coef(modelo_linear_multiplo)

# Intervalos de confiança
confint(modelo_linear_multiplo)

# ANOVA do modelo
anova(modelo_linear_multiplo)

# Valores ajustados
dados$Ajustado_Multiplo <- fitted(modelo_linear_multiplo)

# Resíduos
dados$Residuo_Multiplo <- resid(modelo_linear_multiplo)

# Variance Inflaction Factor (VIF)
vif(modelo_linear_multiplo)

# Gráfico Observados vs. Ajustados
ggplot(
  dados,
  aes(x = Rugosidade_Ra, y = Ajustado_Multiplo)
) +
  geom_point(
    color = okabe_ito[3],
    size = 3,
    alpha = 0.85
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = okabe_ito[6],
    linetype = "dashed",
    linewidth = 1
  ) +
  theme_light() +
  labs(
    title = "Observado vs Ajustado",
    subtitle = "Modelo linear múltiplo",
    x = "Rugosidade observada",
    y = "Rugosidade ajustada"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# Resíduos do Modelo Múltiplo
ggplot(
  dados,
  aes(x = Ajustado_Multiplo, y = Residuo_Multiplo)
) +
  geom_point(
    color = okabe_ito[1],
    size = 3,
    alpha = 0.85
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = okabe_ito[8],
    linewidth = 0.8
  ) +
  theme_light() +
  labs(
    title = "Resíduos vs Ajustados",
    subtitle = "Modelo linear múltiplo",
    x = "Valores ajustados",
    y = "Resíduos"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# Cria um data frame com medidas de diagnóstico do modelo múltiplo
diagnosticos <- data.frame(
  Ajustado = fitted(modelo_linear_multiplo),
  Residuo = resid(modelo_linear_multiplo),
  Residuo_Padronizado = rstandard(modelo_linear_multiplo),
  Leverage = hatvalues(modelo_linear_multiplo),
  Distancia_Cook = cooks.distance(modelo_linear_multiplo)
)

# Cria a raiz do valor absoluto do resíduo padronizado
diagnosticos$Raiz_Residuo_Padronizado <- sqrt(abs(diagnosticos$Residuo_Padronizado))

# Gráfico 1: resíduos vs ajustados
grafico_diag_1 <- ggplot(
  diagnosticos,
  aes(x = Ajustado, y = Residuo)
) +
  geom_point(
    color = okabe_ito[5],
    size = 3,
    alpha = 0.85
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = okabe_ito[8],
    linewidth = 0.8
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = okabe_ito[6],
    linewidth = 1
  ) +
  theme_light() +
  labs(
    title = "Resíduos vs Ajustados",
    x = "Valores ajustados",
    y = "Resíduos"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# Gráfico 2: Q-Q plot
grafico_diag_2 <- ggplot(
  diagnosticos,
  aes(sample = Residuo_Padronizado)
) +
  stat_qq(
    color = okabe_ito[3],
    size = 2.5,
    alpha = 0.85
  ) +
  stat_qq_line(
    color = okabe_ito[6],
    linewidth = 1
  ) +
  theme_light() +
  labs(
    title = "Q-Q Plot",
    x = "Quantis teóricos",
    y = "Quantis observados"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# Gráfico 3: Scale-Location
grafico_diag_3 <- ggplot(
  diagnosticos,
  aes(x = Ajustado, y = Raiz_Residuo_Padronizado)
) +
  geom_point(
    color = okabe_ito[2],
    size = 3,
    alpha = 0.85
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = okabe_ito[6],
    linewidth = 1
  ) +
  theme_light() +
  labs(
    title = "Scale-Location",
    x = "Valores ajustados",
    y = "√|Resíduo padronizado|"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# Gráfico 4: resíduos padronizados vs leverage
grafico_diag_4 <- ggplot(
  diagnosticos,
  aes(x = Leverage, y = Residuo_Padronizado)
) +
  geom_point(
    aes(size = Distancia_Cook),
    color = okabe_ito[7],
    alpha = 0.85
  ) +
  geom_hline(
    yintercept = c(-2, 0, 2),
    linetype = c("dashed", "solid", "dashed"),
    color = okabe_ito[8],
    linewidth = c(0.8, 0.6, 0.8)
  ) +
  theme_light() +
  labs(
    title = "Resíduos vs Leverage",
    x = "Leverage",
    y = "Resíduo padronizado",
    size = "Cook"
  ) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold")
  )

# Combina os quatro gráficos em um painel único
painel_diagnosticos <- (
  grafico_diag_1 +
    grafico_diag_2 +
    grafico_diag_3 +
    grafico_diag_4
) +
  plot_annotation(
    title = "Diagnósticos do Modelo Linear Múltiplo",
    theme = theme(
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        size = 14
      )
    )
  )

painel_diagnosticos

# Comparação entre modelos
summary(modelo_linear_simples)$r.squared
summary(modelo_linear_simples)$adj.r.squared

summary(modelo_linear_multiplo)$r.squared
summary(modelo_linear_multiplo)$adj.r.squared

# Previsões de novos valores usando o modelo
novos_dados <- data.frame(
  Avanco_mm_min = c(175, 190, 205),
  Rotacao_rpm = c(2500, 2400, 2300),
  Desgaste_Ferramenta_mm = c(0.12, 0.18, 0.24),
  Temperatura_C = c(41.5, 45.0, 48.5),
  Concentracao_Fluido_pct = c(6.4, 6.0, 5.8)
)

# Previsões pontuais
predict(modelo_linear_multiplo, newdata = novos_dados)

# Intervalos de confiança para a média
predict(
  modelo_linear_multiplo,
  newdata = novos_dados,
  interval = "confidence"
)

# Intervalos de predição para novas observações
predict(
  modelo_linear_multiplo,
  newdata = novos_dados,
  interval = "prediction"
)

