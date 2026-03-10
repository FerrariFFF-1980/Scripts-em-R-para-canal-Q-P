######################################################
# TESTE DE UMA PROPORÇÃO (1 Sample Proportions Test) #
######################################################

# Versao sem correção de continuidade
# Use quando n < 30
prop.test(
  x = 18,
  n = 200,
  p = 0.10,
  alternative = "two.sided",   # two.sided, less, greater
  correct = FALSE              # remove correção de continuidade de Yates
)

########################################################
# TESTE DE DUAS PROPORÇÕES (2-Sample Proportions Test) #
########################################################

# Teste
prop.test(
  x = c(15, 30),
  n = c(200, 220),
  alternative = "two.sided",
  correct = FALSE
)

#################################
# TESTE BINOMIAL EXATO          #
#################################

# Teste
binom.test(
  x = 3,
  n = 20,
  p = 0.10,
  alternative = "two.sided"
)