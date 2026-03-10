######################################################
# TESTE DE UMA PROPORÇÃO (1 Sample Proportions Test) #
######################################################

# Versao sem correção de continuidade
# Use quando amostras grandes
# comparação de uma ou mais proporções
# aproximação normal/qui-quadrado é válida

prop.test(
  x = 18,
  n = 200,
  p = 0.10,
  conf.level = 0.95,
  alternative = "two.sided",   # two.sided, less, greater
  correct = TRUE              # remove correção de continuidade de Yates
)

########################################################
# TESTE DE DUAS PROPORÇÕES (2-Sample Proportions Test) #
########################################################

# Teste
prop.test(
  x = c(15, 30),
  n = c(200, 220),
  conf.level = 0.95,
  alternative = "two.sided",
  correct = TRUE
)

#################################
# TESTE BINOMIAL EXATO          #
#################################

# Use quando a amostra é pequena
# você quer um teste exato, sem aproximações
# existe apenas uma proporção
# os dados são sucessos vs falhas
# n < 30 (regra prática)
# n*p < 5 ou n * (1−p) < 5

# Teste
prop.test(
  x = 3,
  n = 20,
  p = 0.10,
  conf.level = 0.95,
  alternative = "two.sided",
  correct = TRUE
)
