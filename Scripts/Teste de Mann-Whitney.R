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
library(tidyr)

# Definir semente para reprodutibilidade
set.seed(123)

# Definir paleta Okabe-Ito
okabe_ito <- c(
  "laranja"       = "#E69F00",
  "azul_claro"    = "#56B4E9",
  "verde_azulado" = "#009E73",
  "amarelo"       = "#F0E442",
  "azul"          = "#0072B2",
  "vermelho"      = "#D55E00",
  "roxo"          = "#CC79A7",
  "preto"         = "#000000")

# Simular dois grupos independentes
grupo_a_valores <- c(12, 15, 14, 10, 18, 16, 11, 13, 17, 14)
grupo_b_valores <- c(20, 22, 19, 18, 24, 21, 23, 20, 25, 19)

# Criar tabela de dados
tabela_dados <- data.frame(
  grupo = c(rep("Grupo A", length(grupo_a_valores)),
            rep("Grupo B", length(grupo_b_valores))),
  valor = c(grupo_a_valores, grupo_b_valores))

# Converter grupo em fator para controlar ordem no gráfico
tabela_dados$grupo <- factor(tabela_dados$grupo,
                             levels = c("Grupo A", "Grupo B"))

# Estatísticas descritivas
resumo_descritivo <- tabela_dados %>%
  group_by(grupo) %>%
  summarise(
    tamanho_amostra = n(),
    media = mean(valor),
    mediana = median(valor),
    desvio_padrao = sd(valor),
    iqr = IQR(valor),
    minimo = min(valor),
    maximo = max(valor))

print(resumo_descritivo)

# Executar teste de Mann-Whitney (Wilcoxon rank-sum)
resultado_teste <- wilcox.test(
  valor ~ grupo,
  data = tabela_dados,
  alternative = "two.sided",
  exact = FALSE,
  conf.int = TRUE
)

print(resultado_teste)

# Calcular ranks manualmente
tabela_ranks <- tabela_dados %>%
  arrange(valor) %>%  # ordenar valores
  mutate(rank = rank(valor, ties.method = "average"))  # atribuir ranks

print(tabela_ranks)

# Somar ranks por grupo
resumo_ranks <- tabela_ranks %>%
  group_by(grupo) %>%
  summarise(
    tamanho_amostra = n(),
    soma_ranks = sum(rank)
  )

print(resumo_ranks)

# Extrair valores para cálculo manual de U
n_grupo_a <- resumo_ranks$tamanho_amostra[resumo_ranks$grupo == "Grupo A"]
n_grupo_b <- resumo_ranks$tamanho_amostra[resumo_ranks$grupo == "Grupo B"]

soma_ranks_a <- resumo_ranks$soma_ranks[resumo_ranks$grupo == "Grupo A"]
soma_ranks_b <- resumo_ranks$soma_ranks[resumo_ranks$grupo == "Grupo B"]

# Calcular estatística U
u_grupo_a <- soma_ranks_a - (n_grupo_a * (n_grupo_a + 1)) / 2
u_grupo_b <- soma_ranks_b - (n_grupo_b * (n_grupo_b + 1)) / 2

cat("U do Grupo A =", u_grupo_a, "\n")
cat("U do Grupo B =", u_grupo_b, "\n")

# Verificação de consistência
cat("U_A + U_B =", u_grupo_a + u_grupo_b, "\n")
cat("n1 * n2 =", n_grupo_a * n_grupo_b, "\n")

# Calcular tamanho de efeito (correlação bisserial de ranks)
correlacao_bisserial <- 1 - (2 * min(u_grupo_a, u_grupo_b)) /
  (n_grupo_a * n_grupo_b)

cat("Correlação bisserial de ranks =", correlacao_bisserial, "\n")

# Gráfico 1: Boxplot com pontos individuais
grafico_boxplot <- ggplot(tabela_dados,
                          aes(x = grupo, y = valor,
                              fill = grupo, color = grupo)) +
  geom_boxplot(width = 0.5, alpha = 0.35, outlier.shape = NA) +
  geom_jitter(width = 0.08, size = 2.6, alpha = 0.85) +
  scale_fill_manual(values = c("Grupo A" = okabe_ito["azul_claro"],
                               "Grupo B" = okabe_ito["vermelho"])) +
  scale_color_manual(values = c("Grupo A" = okabe_ito["azul"],
                                "Grupo B" = okabe_ito["vermelho"])) +
  labs(
    title = "Exemplo do teste de Mann-Whitney",
    subtitle = "Boxplot com observações individuais",
    x = "Grupo",
    y = "Valor medido"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(grafico_boxplot)

# Gráfico 2: Violin plot com boxplot
grafico_violin <- ggplot(tabela_dados,
                         aes(x = grupo, y = valor, fill = grupo)) +
  geom_violin(alpha = 0.45, trim = FALSE, color = NA) +
  geom_boxplot(width = 0.12, fill = "white",
               color = "black", outlier.shape = NA) +
  scale_fill_manual(values = c("Grupo A" = okabe_ito["azul_claro"],
                               "Grupo B" = okabe_ito["vermelho"])) +
  labs(
    title = "Comparação de distribuições",
    subtitle = "Violin plot com boxplot",
    x = "Grupo",
    y = "Valor medido"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

print(grafico_violin)

# Gráfico 3: Visualização dos ranks
grafico_ranks <- tabela_ranks %>%
  mutate(indice_observacao = row_number()) %>%
  ggplot(aes(x = indice_observacao, y = rank, color = grupo)) +
  geom_point(size = 3) +
  geom_segment(aes(xend = indice_observacao,
                   y = 0, yend = rank), alpha = 0.5) +
  scale_color_manual(values = c("Grupo A" = okabe_ito["azul"],
                                "Grupo B" = okabe_ito["vermelho"])) +
  labs(
    title = "Ranks atribuídos após combinar os grupos",
    subtitle = "Base do teste de Mann-Whitney",
    x = "Ordem das observações",
    y = "Rank"
  ) +
  theme_minimal(base_size = 13)

print(grafico_ranks)