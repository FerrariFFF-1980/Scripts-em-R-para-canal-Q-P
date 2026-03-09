###################################
# Teste de Poisson para 1 amostra #
###################################

# Número total de eventos observados
numero_defeitos <- 18

# Número de unidades inspecionadas
numero_pecas <- 200

# Taxa hipotética de defeitos
taxa_hipotetica <- 0.05

# Teste de Poisson
poisson.test(
  x = numero_defeitos,
  T = numero_pecas,
  r = taxa_hipotetica,
  alternative = "two.sided"
)

####################################
# Teste de Poisson para 2 amostras #
####################################

# Número de eventos
eventos <- c(14, 7)

# Exposição (peças inspecionadas)
exposicao <- c(180, 160)

# Teste de igualdade das taxas
poisson.test(
  x = eventos,
  T = exposicao,
  alternative = "two.sided"
)