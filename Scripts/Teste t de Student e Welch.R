##########################################
# Teste de Hipótese t de Student / Welch #
##########################################

# Evita notação científica
options(scipen = 999)

# Pacotes
library(rstatix)
library(ggplot2)

# 1) TESTE t PARA UMA AMOSTRA
# ============================
# Pergunta:
# A média do diâmetro é igual a 10 mm?

# Hipóteses:
# H0: mu = 10
# H1: mu != 10
t.test(
  x = teste_t$Diametro_mm,
  mu = 10,
  conf.level = 0.95,
  alternative = "two.sided"
)

# Teste unilateral à direita
# H0: mu <= 10
# H1: mu > 10
# --------------------------------------------------
t.test(
  x = teste_t$Diametro_mm,
  mu = 10,
  conf.level = 0.95,
  alternative = "greater"
)

# Teste unilateral à esquerda
# H0: mu >= 10
# H1: mu < 10
# --------------------------------------------------
t.test(
  x = teste_t$Diametro_mm,
  mu = 10,
  conf.level = 0.95,
  alternative = "less"
)

# ==================================================
# 2) TESTE t PARA DUAS AMOSTRAS INDEPENDENTES
# MÉTODO DE WELCH (PADRÃO DO R)
# ==================================================
# Comparar o diâmetro produzido pelos centros de usinagem

# Hipóteses:
# H0: mu_CU1 = mu_CU2
# H1: mu_CU1 != mu_CU2
t.test(
  Diametro_mm ~ Centro_Usinagem,
  data = teste_t,
  conf.level = 0.95,
  alternative = "two.sided"
)

# Welch unilateral à direita
t.test(
  Diametro_mm ~ Centro_Usinagem,
  data = teste_t,
  conf.level = 0.95,
  alternative = "greater"
)

# Welch unilateral à esquerda
t.test(
  Diametro_mm ~ Centro_Usinagem,
  data = teste_t,
  conf.level = 0.95,
  alternative = "less"
)

# ==================================================
# 3) TESTE t PARA DUAS AMOSTRAS INDEPENDENTES
# MÉTODO DE STUDENT (VARIÂNCIAS IGUAIS)
# ==================================================

# Hipótese adicional assumida:
# variância populacional dos dois grupos é igual
t.test(
  Diametro_mm ~ Centro_Usinagem,
  data = teste_t,
  var.equal = TRUE,
  conf.level = 0.95,
  alternative = "two.sided"
)

# Student unilateral à direita
t.test(
  Diametro_mm ~ Centro_Usinagem,
  data = teste_t,
  var.equal = TRUE,
  conf.level = 0.95,
  alternative = "greater"
)

# Student unilateral à esquerda
t.test(
  Diametro_mm ~ Centro_Usinagem,
  data = teste_t,
  var.equal = TRUE,
  conf.level = 0.95,
  alternative = "less"
)

# ==================================================
# 4) TESTE t PAREADO
# ==================================================
# Comparar medições antes e depois do ajuste da máquina

# Hipóteses:
# H0: média das diferenças = 0
# H1: média das diferenças != 0

t.test(
  x = teste_t$Antes_Ajuste,
  y = teste_t$Depois_Ajuste,
  conf.level = 0.95,
  paired = TRUE,
  alternative = "two.sided"
)

# Pareado unilateral à direita
# H1: média(Antes - Depois) > 0
t.test(
  x = teste_t$Antes_Ajuste,
  y = teste_t$Depois_Ajuste,
  paired = TRUE,
  conf.level = 0.95,
  alternative = "greater"
)

# Pareado unilateral à esquerda
# H1: média(Antes - Depois) < 0
t.test(
  x = teste_t$Antes_Ajuste,
  y = teste_t$Depois_Ajuste,
  paired = TRUE,
  conf.level = 0.95,
  alternative = "less"
)

# ==================================================
# 5) VERIFICAÇÃO DE NORMALIDADE
# ==================================================

# Teste por grupos
teste_t %>%
  group_by(Centro_Usinagem) %>%
  shapiro_test(Diametro_mm)

ggplot(teste_t, aes(sample = Diametro_mm)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~ Centro_Usinagem) +
  theme_light() +
  labs (
    title = "Gráfico Q-Q Plot",
    x = "Quantis Teoricos",
    y = "Quantis Observados"
  )

# Diferenças para o teste de Shapiro-Wilk (para o teste pareado)
diferencas <- teste_t$Antes_Ajuste - teste_t$Depois_Ajuste
shapiro.test(diferencas)

ggplot(teste_t,
       aes(sample = diferencas)) +
  stat_qq() +
  stat_qq_line() +
  theme_light() +
  labs(
    title = "Gráfico Q-Q Plot",
    subtitle = "das diferenças entre CU1 e CU2",
    x = "Quantis Teóricos",
    y = "Quantis Observados"
  )
