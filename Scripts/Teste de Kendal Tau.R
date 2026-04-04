# Teste de Associação Não-paramétrico de Kendall Tau ----
# Associação monotônica (sempre crescente ou decrescente)
# Dados ruidosos ou com outliers
# Escala ordinal ou ranking
# Amostra pequena

# Configuracoes ----
options(scipen = 999)

# Pacotes ----
library(ggplot2)
library(dplyr)
library(tidyr)

# Dados ----
dados <- data.frame(
  temperatura = c(20, 22, 25, 27, 30, 32, 35, 37, 40, 42),
  desgaste    = c(5,   6,  7,  9, 10, 12, 11, 15, 16, 18))

# Gerar todos os pares ----
pares <- expand.grid(i = 1:nrow(dados), j = 1:nrow(dados)) %>%
  filter(i < j) %>%  # garante pares únicos
  mutate(
    x_i = dados$temperatura[i],
    x_j = dados$temperatura[j],
    y_i = dados$desgaste[i],
    y_j = dados$desgaste[j],
    
    # diferenças
    diff_x = x_i - x_j,
    diff_y = y_i - y_j,
    
    # produto das diferenças
    produto = diff_x * diff_y,
    
    # classificação
    classificacao = case_when(
      produto > 0 ~ "Concordante",
      produto < 0 ~ "Discordante",
      TRUE ~ "Empate"))


# Resumo dos pares ----
resumo <- pares %>% count(classificacao)

print(resumo)

# Cálculo manual ----
C <- sum(pares$classificacao == "Concordante")
D <- sum(pares$classificacao == "Discordante")
n <- nrow(dados)

total_pares <- n * (n - 1) / 2

tau_manual <- (C - D) / total_pares

# Comparação com Teste de Correlação ----
cor.test(dados$temperatura, dados$desgaste, method = "kendall")

# Gráfico ----
ggplot(dados, aes(x = temperatura, y = desgaste)) +
  geom_point(size = 3, color = "#0072B2") +
  geom_smooth(method = "lm", se = FALSE, color = "#D55E00") +
  labs(title = "Relacao entre temperatura e desgaste",
       x = "Temperatura", y = "Desgaste") +
  theme_light(base_size = 13)