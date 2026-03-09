######################################################
# TESTE DE UMA PROPORÇÃO (1 Sample Proportions Test) #
######################################################

# Versao sem correção de continuidade
# Use quando n < 30

# Dados
numero_defeitos <- 18     # x = número de sucessos
tamanho_amostra <- 200    # n = tamanho da amostra
proporcao_hipotese <- 0.10  # p0 = proporção da hipótese

# Teste de proporção
prop.test(
  x = numero_defeitos,
  n = tamanho_amostra,
  p = proporcao_hipotese,
  alternative = "two.sided",   # two.sided, less, greater
  correct = FALSE              # remove correção de continuidade de Yates
)

########################################################
# TESTE DE DUAS PROPORÇÕES (2-Sample Proportions Test) #
########################################################

# Número de sucessos
sucessos <- c(15, 30)

# Tamanho das amostras
amostras <- c(200, 220)

# Teste
prop.test(
  x = sucessos,
  n = amostras,
  alternative = "two.sided",
  correct = FALSE
)

#################################
# TESTE BINOMIAL EXATO          #
#################################

# Número de sucessos
numero_defeitos <- 3

# Tamanho da amostra
tamanho_amostra <- 20

# Proporção da hipótese
proporcao_hipotese <- 0.10

# Teste
binom.test(
  x = numero_defeitos,
  n = tamanho_amostra,
  p = proporcao_hipotese,
  alternative = "two.sided"
)