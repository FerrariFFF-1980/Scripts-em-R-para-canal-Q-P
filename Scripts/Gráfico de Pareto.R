# Gráfico de Pareto no R

# Instalar qcc do repositório R
if (!requireNamespace("qcc", quietly = TRUE)) {
  install.packages("qcc")
}

# Carregar a biblioteca qcc
library(qcc)

# Frequency table of failure modes
tabela_frequencia <- table(falhas_pcb$FailureMode)

# Pareto chart
pareto.chart(
  tabela_frequencia,
  main = "Pareto chart by Failure Mode",
  ylab = "Frequency"
)
