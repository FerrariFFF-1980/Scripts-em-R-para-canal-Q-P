# Teste de Associação Não-paramétrico de Kendall Tau ----
# Associação monotônica (sempre crescente)
# Dados ruidosos ou com outliers
# Escala ordinal ou ranking
# Amostra pequena

# Configuracoes ----
options(scipen = 999)

# Pacotes ----
library(ggplot2)
library(dplyr)

# Dados ----
# exemplo: relacao entre temperatura e desgaste
dados <- data.frame(
  temperatura = c(20, 22, 25, 27, 30, 32, 35, 37, 40, 42),
  desgaste = c(5, 6, 7, 9, 10, 12, 11, 15, 16, 18))

print(dados)

# Teste de kendall tau ----
# tau (τ) → força da associação
# leitura prática:
# τ > 0 → relação crescente
# τ < 0 → relação decrescente
# τ ≈ 0 → sem relação
cor.test(dados$temperatura, dados$desgaste, method = "kendall")

# Grafico ----
ggplot(dados, aes(x = temperatura, y = desgaste)) +
  geom_point(size = 3, color = "#0072B2") +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00") +
  labs(title = "Relacao entre temperatura e desgaste",
       x = "Temperatura", y = "Desgaste") +
  theme_light(base_size = 13)