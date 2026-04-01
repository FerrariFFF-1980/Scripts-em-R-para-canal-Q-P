# Teste de Friedman (Dados pareados) ----
# Cada linha da base original representa uma peça.
# metodo_a, metodo_b, metodo_c = tratamentos
# resposta = valor medido
# Usar quando há 3 ou mais condições:
# - os dados são dependentes ou pareados
# - a variável é ordinal ou numérica
# - a normalidade é duvidosa ou violada

# Configuracoes ----
options(scipen = 999)
paleta_okabe_ito <- c("metodo_a" = "#E69F00",
                      "metodo_b" = "#56B4E9",
                      "metodo_c" = "#009E73")

# Pacotes ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(rstatix)

# Dados ----
dados <- data.frame(
  peca = factor(1:8),
  metodo_a = c(10, 12, 11, 13, 12, 11, 14, 13),
  metodo_b = c(14, 15, 13, 16, 15, 14, 17, 16),
  metodo_c = c( 9, 10,  8, 11, 10,  9, 12, 11))

# Dados em formato longo ----
dados_longos <- dados %>%
  pivot_longer(cols = -peca, names_to = "metodo", values_to = "resposta")

# Teste de Friedman ----
dados_longos %>% friedman_test(resposta ~ metodo | peca)

# Pos-teste ----
dados_longos %>% pairwise_wilcox_test(resposta ~ metodo,
    paired = TRUE, p.adjust.method = "bonferroni")

# Tamanho de Efeito: Estatística W de Kendal (de 0 a 1) ----
dados_longos %>% friedman_effsize(resposta ~ metodo | peca)

# Boxplot ----
ggplot(dados_longos, aes(x = metodo, y = resposta, fill = metodo)) +
  geom_boxplot(alpha = 0.8, width = 0.6) +
  geom_line(aes(group = peca), color = "gray70", alpha = 0.6) +
  geom_point(size = 2) +
  scale_fill_manual(values = paleta_okabe_ito) +
  labs(title = "Resposta por metodo", subtitle = "Linhas conectam a mesma peca",
       x = "Metodo", y = "Resposta") +
  theme_light(base_size = 13)
