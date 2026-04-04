# Teste de Associação Não-paramétrico de Kendall Tau ----
# Associação monotônica (crescente ou decrescente)
# Dados ruidosos ou com outliers
# Escala ordinal ou ranking
# Amostra pequena ----

# Configuracoes ----
options(scipen = 999)

# Pacotes ----
library(ggplot2)
library(dplyr)

# Dados ----
dados <- data.frame(
  temperatura = c(20, 22, 25, 27, 30, 32, 35, 37, 40, 42),
  desgaste    = c(5, 6, 7, 9, 10, 12, 11, 15, 16, 18))

# Gerar todos os pares (i, j) ----
pares <- expand.grid(i = 1:nrow(dados), j = 1:nrow(dados)) %>%
  filter(i < j) %>%
  mutate(
    x_i = dados$temperatura[i],
    x_j = dados$temperatura[j],
    y_i = dados$desgaste[i],
    y_j = dados$desgaste[j],
    
    diff_x = x_i - x_j,
    diff_y = y_i - y_j,
    
    produto = diff_x * diff_y,
    
    classificacao = case_when(
      produto > 0 ~ "Concordante",
      produto < 0 ~ "Discordante",
      TRUE ~ "Empate" ))

# Resumo dos pares ----
pares %>% count(classificacao)

# Calculo manual do Kendall Tau ----
C <- sum(pares$classificacao == "Concordante") # soma dos concordantes
D <- sum(pares$classificacao == "Discordante") # soma dos discordantes
n <- nrow(dados) # total de dados
tau_manual <- (C - D) / (n * (n - 1) / 2)

# Comparacao com cor.test ----
cor.test(dados$temperatura, dados$desgaste, method = "kendall")

# Grafico ----
ggplot(dados, aes(x = temperatura, y = desgaste)) +
  geom_point(size = 3, color = "#0072B2") +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00") +
  labs(title = "Relacao entre temperatura e desgaste",
       x = "Temperatura", y = "Desgaste") +
  theme_light(base_size = 13)
