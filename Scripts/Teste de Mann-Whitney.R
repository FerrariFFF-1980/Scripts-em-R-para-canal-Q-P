# Teste de Mann-Whitney ----
# usado para comparar dois grupos independentes quando:
# - a normalidade é duvidosa
# - há outliers
# - a variável é ordinal ou contínua sem boa aderência normal
# - testa se os dois grupos têm a mesma distribuição
# - muito usado como teste de diferença de posição central

# Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# Definir paleta Okabe-Ito para os grupos
okabe_ito <- c(
  "Grupo A" = "#56B4E9",
  "Grupo B" = "#D55E00")

# Criar dados de exemplo
tabela_dados <- data.frame(
  grupo = c(rep("Grupo A", 10),
            rep("Grupo B", 10)),
  valor = c(12, 15, 14, 10, 18, 16, 11, 13, 17, 14,
            20, 22, 19, 18, 24, 21, 23, 20, 25, 19))

# Converter grupo em fator para controlar a ordem
tabela_dados$grupo <- factor(tabela_dados$grupo, levels = c("Grupo A", "Grupo B"))

# Executar o teste de Mann-Whitney de forma automática
# Em R, isso é feito com wilcox.test para duas amostras independentes
resultado_teste <- wilcox.test(
  valor ~ grupo,
  data = tabela_dados,
  alternative = "two.sided",
  exact = FALSE,
  conf.int = TRUE)

print(resultado_teste)

# Gerar resumo descritivo simples
resumo_descritivo <- tabela_dados %>%
  group_by(grupo) %>%
  summarise(tamanho_amostra = n(),
            mediana = median(valor),
            iqr = IQR(valor),
            minimo = min(valor),
            maximo = max(valor),
            .groups = "drop")

print(resumo_descritivo)

# Preparar dados para o gráfico de segmentos
# Aqui os ranks são usados só para visualização
tabela_ranks <- tabela_dados %>%
  arrange(valor) %>%
  mutate(rank = rank(valor, ties.method = "average"),
         indice_observacao = row_number())

# Gráfico 1: boxplot com pontos individuais
ggplot(
  tabela_dados,
  aes(x = grupo, y = valor, fill = grupo, color = grupo)) +
  geom_boxplot(width = 0.5, alpha = 0.30, outlier.shape = NA) +
  geom_jitter(width = 0.08, size = 2.8, alpha = 0.85) +
  scale_fill_manual(values = okabe_ito) +
  scale_color_manual(values = okabe_ito) +
  labs(
    title = "Teste de Mann-Whitney",
    subtitle = "Boxplot com observações individuais",
    x = "Grupo", y = "Valor medido") +
  theme_light(base_size = 14) +
  theme(legend.position = "none")

# Gráfico 2: gráfico de segmentos dos ranks
ggplot(
  tabela_ranks, aes(x = indice_observacao, y = rank, color = grupo)) +
  geom_segment(aes(xend = indice_observacao, y = 0, yend = rank),
    alpha = 0.60, linewidth = 0.8) +
  geom_point(size = 3) +
  scale_color_manual(values = okabe_ito, name = "Grupo") +
  labs(
    title = "Ranks após combinar os dois grupos",
    subtitle = "Visualização da lógica do Mann-Whitney",
    x = "Ordem das observações", y = "Rank") +
  theme_light(base_size = 14)

