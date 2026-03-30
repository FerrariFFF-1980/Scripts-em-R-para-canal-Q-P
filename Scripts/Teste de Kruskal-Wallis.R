# Teste de Kruskal-Wallis ----
# Alternativa não-paramétrica ao ANOVA de 1 via
# Não depende de distribuição normal
# Funciona bem com dados assimétricos
# Usa postos, não valores absolutos
# Valores extremos não "puxam" os resultados
# Pode usar rankings, notas, escalas subjetivas

# Configuracoes ----
options(scipen = 999)

# Pacotes ----
library(ggplot2)
library(dplyr)
library(rstatix)

# Dados ----
dados <- data.frame(
  Grupo = factor(c(
    rep("Método A", 10),
    rep("Método B", 10),
    rep("Método C", 10))),
  Resposta = c(
    12, 15, 14, 10, 13, 11, 16, 14, 12, 13,
    18, 20, 17, 19, 21, 18, 22, 20, 19, 23,
    11,  9, 10, 12,  8, 11,  9, 10, 13,  8))

# Resumo estatistico ----
resumo_grupos <- dados %>%
  group_by(Grupo) %>%
  summarise(
    n = n(),
    mediana = median(Resposta),
    media = mean(Resposta),
    desvio_padrao = sd(Resposta),
    minimo = min(Resposta),
    maximo = max(Resposta))

print(resumo_grupos)

# Boxplot ----
paleta_okabe_ito <- c(
  "Método A" = "#e69f00",
  "Método B" = "#56b4e9",
  "Método C" = "#009e73")

# Kruskal-wallis ----
kruskal.test(Resposta ~ Grupo, data = dados)

# Pos-teste de Dunn (todos vs. todos) ----
dados %>% dunn_test(Resposta ~ Grupo, p.adjust.method = "bonferroni")

# Grafico com pontos ----
ggplot(dados, aes(x = Grupo, y = Resposta, color = Grupo)) +
  geom_jitter(width = 0.01, size = 2.8, alpha = 0.85) +
  geom_boxplot(alpha = 0.8, width = 0.6) +
  stat_summary(fun = median, geom = "point", shape = 18, size = 6,
    color = "#000000", alpha = 0.65) +
  scale_color_manual(values = paleta_okabe_ito) +
  theme_light(base_size = 13) +
  labs(title = "Distribuicao por grupo",
       subtitle = "Losango representa a mediana",
       x = "Grupo", y = "Resposta")
