##############################
# TESTES DE VARIÂNCIA EM R
##############################

# Evita notação científica
options(scipen = 999, digits = 6)

# Carrega os pacotes necessários
library(EnvStats)
library(car)

# Garante que a coluna de grupos seja tratada como fator
CentrosUsinagem$`Centro de Usinagem` <- as.factor(CentrosUsinagem$`Centro de Usinagem`)

# Cria vetores separados por centro de usinagem
centro_1 <- subset(CentrosUsinagem, `Centro de Usinagem` == "Centro 1")$`Diâmetro (mm)`
centro_2 <- subset(CentrosUsinagem, `Centro de Usinagem` == "Centro 2")$`Diâmetro (mm)`
centro_3 <- subset(CentrosUsinagem, `Centro de Usinagem` == "Centro 3")$`Diâmetro (mm)`

# Exibe as variâncias amostrais de cada grupo
var(centro_1)
var(centro_2)
var(centro_3)

# TESTE QUI-QUADRADO PARA UMA VARIÂNCIA
#######################################
# Objetivo:
# Testar se a variância de um único grupo é igual a um valor-alvo

# Hipóteses:
# H0: sigma^2 = sigma0^2
# H1: sigma^2 != sigma0^2

# Define a variância hipotética de referência
# Ajuste este valor conforme sua meta, especificação ou estudo anterior
variancia_hipotetica <- 0.0004

# Aplica o teste no Centro 1
# Teste Chi-quadrado para Variancia
# Exige normalidade dos dados
varTest(    # do pacote EnvStats
  x = centro_1,
  sigma.squared = variancia_hipotetica,
  alternative = "two.sided",
  conf.level = 0.95
)

# TESTE F PARA DUAS VARIÂNCIAS
##############################
# Objetivo:
# Comparar as variâncias de dois grupos

# Hipóteses:
# H0: sigma1^2 = sigma2^2
# H1: sigma1^2 != sigma2^2

# Exemplo: comparação entre Centro 1 e Centro 2
# Teste F Clássico
# Assume normalidade nas duas populações
# Sensível a outliers de desvios da normalidade
var.test(
  x = centro_1,
  y = centro_2,
  alternative = "two.sided",
  conf.level = 0.95
)

# TESTE DE LEVENE
##############################
# Objetivo:
# Comparar a homogeneidade das variâncias entre 3 ou mais grupos
# É mais robusto à não-normalidade.

# Hipóteses:
# H0: todas as variâncias são iguais
# H1: pelo menos uma variância é diferente

leveneTest(
  `Diâmetro (mm)` ~ `Centro de Usinagem`,
  data = CentrosUsinagem
)

# TESTE DE BARTLETT
##############################
# Objetivo:
# Comparar a homogeneidade das variâncias entre 3 ou mais grupos
# Muito sensível à não-normalidade.
# Ideal quando os dados são aproximadamente normais.

# Hipóteses:
# H0: todas as variâncias são iguais
# H1: pelo menos uma variância é diferente

bartlett.test(
  `Diâmetro (mm)` ~ `Centro de Usinagem`,
  data = CentrosUsinagem
)
