# Pacotes
library(qcc)

# Diagrama de Ishikawa
causas <- list(
  Método = c("Instrução desatualizada", "Setup variável"),
  Máquina = c("Folga", "Falta de manutenção"),
  Material = c("Lote vencido", "Fornecedor"),
  Medicao = c("R&R alto", "Calibração vencida", "Instrumento Quebrado"),
  Mão_de_Obra = c("Treinamento", "Rotatividade"),
  Meio_Ambiente = c("Temperatura", "Umidade")
)

cause.and.effect(cause = causas, effect = "Defeitos acima do limite")
