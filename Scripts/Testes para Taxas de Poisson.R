###################################
# Teste de Poisson para 1 amostra #
###################################

# Teste de Poisson
poisson.test(
  x = 18,
  T = 200,
  r = 0.05,
  conf.level = 0.95,
  alternative = "two.sided"
)

####################################
# Teste de Poisson para 2 amostras #
####################################

# Teste de igualdade das taxas
poisson.test(
  x = c(14, 7),
  T = c(180, 160),
  conf.level = 0.95,
  alternative = "two.sided"
)
