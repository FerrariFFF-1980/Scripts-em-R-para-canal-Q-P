#################
# TWO-WAY ANOVA #
#################

# Evita notação científica
options(scipen = 999, digits = 6)

# Pacotes
library(ggplot2)
library(car)
library(readxl)

# Importa os dados
ANOVA2 <- read_excel("Dados/ANOVA2.xlsx")

# Preparação dos dados
ANOVA2$CentroUsinagem <- as.factor(ANOVA2$CentroUsinagem)
ANOVA2$VelocidadeCorte <- as.factor(ANOVA2$VelocidadeCorte)

# Paleta de cores
okabe_ito <- c(
  "#E69F00",
  "#56B4E9",
  "#009E73",
  "#F0E442",
  "#0072B2",
  "#D55E00",
  "#CC79A7",
  "#000000"
)

# Boxplots
ggplot(
  ANOVA2,
  aes(
    x = CentroUsinagem,
    y = Diametro_mm,
    fill = VelocidadeCorte
  )
) +
  geom_boxplot(
    alpha = 0.8,
    position = position_dodge(width = 1)
  ) +
  scale_fill_manual(
    values = c(
      okabe_ito[2],
      okabe_ito[6])
    ) +
  
  labs(
    title = "Diâmetro por Centro de Usinagem e Velocidade de Corte",
    x = "Centro de Usinagem",
    y = "Diâmetro (mm)"
  ) +
  theme_light()

# Gráfico de Interações
ggplot(
  ANOVA2,
  aes(
    x = VelocidadeCorte,
    y = Diametro_mm,
    color = CentroUsinagem,
    group = CentroUsinagem
  )
) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3
  ) +
  stat_summary(
    fun = mean,
    geom = "line",
    linewidth = 1
  ) +
  scale_color_manual(
    values = okabe_ito[1:3]
  ) +
  
  labs(
    title = "Gráfico de Interação",
    x = "Velocidade de Corte",
    y = "Média do Diâmetro (mm)"
  ) +
  theme_light()

# Modelo Two-Way Anova
modelo_two_way <- aov(
  Diametro_mm ~ CentroUsinagem * VelocidadeCorte,
  data = ANOVA2
)

summary(modelo_two_way)

# Resíduos
residuos <- residuals(modelo_two_way)
ajustados <- fitted(modelo_two_way)

dados_residuos <- data.frame(
  Ajustados = ajustados,
  Residuos = residuos
)

# Resíduos vs. Valores Ajustados
ggplot(
  dados_residuos,
  aes(
    x = Ajustados,
    y = Residuos
  )
) +
  geom_point(size = 3, color = okabe_ito[5]) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Resíduos vs Ajustados",
    x = "Valores Ajustados",
    y = "Resíduos"
  ) +
  theme_light()

# Gráfico de Normalidade de Resíduos (Q-Q)
ggplot(
  data.frame(residuos),
  aes(sample = residuos)
) +
  stat_qq(
    color = okabe_ito[6],
    size = 3
  ) +
  stat_qq_line(color = "black") +
  labs(
    title = "Gráfico Q-Q dos Resíduos"
  ) +
  theme_light()

# Teste de Normalidade dos Resíduos
shapiro.test(residuos)
AndersonDarlingTest(residuos, "pnorm")


# Homoscedasticidade (Igualdade de Variâncias)
leveneTest(
  Diametro_mm ~ interaction(CentroUsinagem, VelocidadeCorte),
  data = ANOVA2
)

bartlett.test(
  Diametro_mm ~ interaction(CentroUsinagem, VelocidadeCorte),
  data = ANOVA2
)

# Pós-Teste: Tukey HSD
TukeyHSD(modelo_two_way)
