########################
# Análise de Variância #
# Usar: ANOVA.xlsx     #
########################

# Evita notação científica
options(scipen = 999, digits = 6)

# Carrega pacotes necessários
library(ggplot2)
library(car)
library(rstatix)
library(readxl)
library(DescTools)

# Importa os dados se ainda nao estiverem presentes
ANOVA <- read_excel("Dados/ANOVA.xlsx")

# Paleta Okabe-Ito (colorblind safe)
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

# Converte grupo para fator
ANOVA$CentroUsinagem <- as.factor(ANOVA$CentroUsinagem)

# Estatísticas descritivas
aggregate(Diametro_mm ~ CentroUsinagem, ANOVA, mean)
aggregate(Diametro_mm ~ CentroUsinagem, ANOVA, sd)
aggregate(Diametro_mm ~ CentroUsinagem, ANOVA, length)

# Boxplot (visualização principal para ANOVA)
ggplot(
  ANOVA,
  aes(
    x = CentroUsinagem,
    y = Diametro_mm,
    fill = CentroUsinagem
  )
) +

  geom_boxplot(alpha = 0.8) +

  geom_jitter(
    width = 0.08,
    size = 2,
    alpha = 0.2,
    color = "black"
  ) +

  scale_fill_manual(values = okabe_ito) +
  labs(
    title = "Diâmetro por Centro de Usinagem",
    x = "Centro de Usinagem",
    y = "Diâmetro (mm)"
  ) +
  theme_light() +
  theme(
    legend.position = "none"
  )


# Ajuste da ANOVA de Fisher
modelo_anova <- aov(Diametro_mm ~ CentroUsinagem, data = ANOVA)
summary(modelo_anova)

# Gráfico de resíduos vs valores ajustados
residuos <- residuals(modelo_anova)
ajustados <- fitted(modelo_anova)

dados_residuos <- data.frame(
  Ajustados = ajustados,
  Residuos = residuos
)

ggplot(
  dados_residuos,
  aes(
    x = Ajustados,
    y = Residuos
  )
) +

  geom_point(
    size = 3,
    color = okabe_ito[5]
  ) +

  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +

  labs(
    title = "Resíduos vs Valores Ajustados",
    x = "Valores Ajustados",
    y = "Resíduos"
  ) +

  theme_light()

# Gráfico QQ de normalidade usando ggplot
ggplot(
  data.frame(residuos),
  aes(sample = residuos)
) +

  stat_qq(color = okabe_ito[6], size = 3) +

  stat_qq_line(color = "black") +

  labs(
    title = "Gráfico Q-Q dos Resíduos",
    x = "Quantis Teóricos",
    y = "Quantis Observados"
  ) +

  theme_light()

# Teste de normalidade
shapiro.test(residuos)

# Teste de homogeneidade das variâncias
bartlett.test(Diametro_mm ~ CentroUsinagem, data = ANOVA)
leveneTest(Diametro_mm ~ CentroUsinagem, data = ANOVA)

# ANOVA de Welch (robusta para variâncias diferentes)
oneway.test(
  Diametro_mm ~ CentroUsinagem,
  data = ANOVA,
  var.equal = FALSE
)

# Pós-teste apropriado para Welch
games_howell_test(
  data = ANOVA,
  Diametro_mm ~ CentroUsinagem
)

# Gráfico das médias com intervalo de confiança
ggplot(
  ANOVA,
  aes(
    x = CentroUsinagem,
    y = Diametro_mm,
    fill = CentroUsinagem
  )
) +

  stat_summary(
    fun = mean,
    geom = "point",
    shape = 21,
    size = 4,
    stroke = 1.2,
    color = "black"
  ) +

  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.15,
    linewidth = 0.8,
    color = "black"
  ) +

  scale_fill_manual(values = okabe_ito) +

  labs(
    title = "Média do Diâmetro por Centro de Usinagem",
    x = "Centro de Usinagem",
    y = "Diâmetro médio (mm)"
  ) +

  theme_light() +

  theme(legend.position = "none")

# Tukey HSD para comparar médias entre si
TukeyHSD(
  modelo_anova,
  conf.level = 0.95  
)

# Bonferroni para comparações múltiplas
pairwise_t_test(
  data = ANOVA,
  formula = Diametro_mm ~ CentroUsinagem,
  p.adjust.method = "bonferroni",
  pool.sd = TRUE
)

# Dunnett para comparar todos os grupos contra um grupo de controle
# O grupo de referência será o primeiro nível do fator.
# Se quiser definir explicitamente o controle, use relevel antes.

ANOVA$CentroUsinagem <- relevel(ANOVA$CentroUsinagem, ref = "Centro 1")

DunnettTest(
  x = ANOVA$Diametro_mm,
  g = ANOVA$CentroUsinagem,
  control = "Centro 1",
  conf.level = 0.95
)