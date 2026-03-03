# Diagrama de Ishikawa

# Pacotes
library(qcc)

#Configuração
causas <- list(
  Método = c("Instrução desatualizada", "Setup variável"),
  Máquina = c("Folga", "Falta de manutenção"),
  Material = c("Lote vencido", "Fornecedor"),
  Medicao = c("R&R alto", "Calibração vencida", "Instrumento Quebrado"),
  "Mão de Obra" = c("Treinamento", "Rotatividade"),
  "Meio Ambiente" = c("Temperatura", "Umidade")
  
)

cause.and.effect(cause = causas, effect = "Defeitos acima do limite")
