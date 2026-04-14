# Análise de Concordância entre Avaliadores ----

# Carregar pacotes -----
options(scipen=999)
library(dplyr)
library(tidyr)
library(ggplot2)
library(irr)
library(binom)
library(tibble)

# Dados -----
dados <- tibble(
  padrao = c("A", "A", "B", "B", "C", "A", "B", "C", "A", "B"),
  av1 = c("A", "A", "B", "B", "C", "A", "B", "C", "A", "B"),
  av2 = c("A", "B", "B", "B", "C", "A", "B", "C", "A", "C"),
  av3 = c("A", "A", "B", "B", "C", "A", "C", "C", "A", "B"),
  av4 = c("A", "A", "B", "C", "C", "A", "B", "C", "A", "B"))

print(dados)

n_itens <- nrow(dados)
avaliadores <- c("av1", "av2", "av3", "av4")

# Each Appraiser vs Standard ----
acertos_av1 <- sum(dados$av1 == dados$padrao)
acertos_av2 <- sum(dados$av2 == dados$padrao)
acertos_av3 <- sum(dados$av3 == dados$padrao)
acertos_av4 <- sum(dados$av4 == dados$padrao)

ic_av1 <- binom::binom.confint(acertos_av1, n_itens, methods = "exact")
ic_av2 <- binom::binom.confint(acertos_av2, n_itens, methods = "exact")
ic_av3 <- binom::binom.confint(acertos_av3, n_itens, methods = "exact")
ic_av4 <- binom::binom.confint(acertos_av4, n_itens, methods = "exact")

print (ic_av1)
print (ic_av2)
print (ic_av3)
print (ic_av4)

resultado_avaliador_padrao <- tibble(
  avaliador = c("av1", "av2", "av3", "av4"),
  acertos = c(acertos_av1, acertos_av2, acertos_av3, acertos_av4),
  total = n_itens,
  concordancia = c(ic_av1$mean, ic_av2$mean, ic_av3$mean, ic_av4$mean) * 100,
  li = c(ic_av1$lower, ic_av2$lower, ic_av3$lower, ic_av4$lower) * 100,
  ls = c(ic_av1$upper, ic_av2$upper, ic_av3$upper, ic_av4$upper) * 100)

print(resultado_avaliador_padrao)

# All Appraisers vs Standard ----
acordo_total_com_padrao <- apply(dados[, c("padrao", avaliadores)],
  1, \(x) length(unique(x)) == 1)

print(acordo_total_com_padrao)

acertos_todos_padrao <- sum(acordo_total_com_padrao)
ic_todos_padrao <- binom::binom.confint(acertos_todos_padrao, n_itens,
  methods = "exact")

resultado_todos_padrao <- tibble(medida = "All appraisers vs standard",
  acertos = acertos_todos_padrao, total = n_itens,
  concordancia = ic_todos_padrao$mean * 100, li = ic_todos_padrao$lower * 100,
  ls = ic_todos_padrao$upper * 100)

print(resultado_todos_padrao)

# Between Appraisers ----
acordo_total_entre_avaliadores <- apply(dados[, avaliadores], 1,
  \(x) length(unique(x)) == 1)

acertos_entre_avaliadores <- sum(acordo_total_entre_avaliadores)
ic_entre_avaliadores <- binom::binom.confint(acertos_entre_avaliadores,
  n_itens, methods = "exact")

resultado_entre_avaliadores <- tibble(medida = "Between appraisers",
  acertos = acertos_entre_avaliadores, total = n_itens,
  concordancia = ic_entre_avaliadores$mean * 100,
  li = ic_entre_avaliadores$lower * 100, ls = ic_entre_avaliadores$upper * 100)

print(resultado_entre_avaliadores)

# Kappa de cada avaliador vs padrão ----
kappa_av1 <- irr::kappa2(dados[, c("av1", "padrao")], weight = "unweighted")
kappa_av2 <- irr::kappa2(dados[, c("av2", "padrao")], weight = "unweighted")
kappa_av3 <- irr::kappa2(dados[, c("av3", "padrao")], weight = "unweighted")
kappa_av4 <- irr::kappa2(dados[, c("av4", "padrao")], weight = "unweighted")

print(kappa_av1)
print(kappa_av2)
print(kappa_av3)
print(kappa_av4)

resultado_kappa_padrao <- tibble(avaliador = c("av1", "av2", "av3", "av4"),
  kappa = c(kappa_av1$value, kappa_av2$value, kappa_av3$value, kappa_av4$value),
  z = c(kappa_av1$statistic, kappa_av2$statistic, kappa_av3$statistic, kappa_av4$statistic),
  p_valor = c(kappa_av1$p.value, kappa_av2$p.value, kappa_av3$p.value, kappa_av4$p.value))

print(resultado_kappa_padrao)

# Kappa de Fleiss ----
kappa_fleiss <- irr::kappam.fleiss(dados[, avaliadores])

resultado_fleiss <- tibble(medida = "Between appraisers", kappa = kappa_fleiss$value,
                           z = kappa_fleiss$statistic,p_valor = kappa_fleiss$p.value)

print(resultado_fleiss)

# Kappa par a par ----
pares <- combn(avaliadores, 2)

resultado_pares <- tibble(comparacao = character(), kappa = numeric(),
                          z = numeric(), p_valor = numeric())

for (i in 1:ncol(pares)) {
  a <- pares[1, i]
  b <- pares[2, i]
  
  k <- irr::kappa2(dados[, c(a, b)], weight = "unweighted")
  
  resultado_pares <- bind_rows(resultado_pares,
    tibble(comparacao = paste(a, "vs", b),
           kappa = k$value, z = k$statistic, p_valor = k$p.value
           ))}

print(resultado_pares)

# Concordância por categoria vs padrão ----
dados_longos <- dados |> pivot_longer(
    cols = all_of(avaliadores), names_to = "avaliador",
    values_to = "classificacao")

resultado_categoria_padrao <- dados_longos |>
  mutate(acerto = classificacao == padrao) |>
  group_by(padrao) |> summarise(acertos = sum(acerto), total = n(),
    .groups = "drop") |>
  rowwise() |>
  mutate(ic = list(binom::binom.confint(acertos, total, methods = "exact")),
         concordancia = ic$mean * 100,
         li = ic$lower * 100, ls = ic$upper * 100) |>
  ungroup() |>
  select(-ic)

print(resultado_categoria_padrao)

# Gráfico: Each Appraiser vs Standard ----
ggplot(resultado_avaliador_padrao, aes(x = avaliador, y = concordancia)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0.12, linewidth = 0.7) +
  ylim(0, 100) +
  labs(title = "Each Appraiser vs Standard",
       x = "Avaliador", y = "Concordância (%)") +
  theme_minimal()

# Gráfico: All Appraisers vs Standard e Between Appraisers ----
resultado_global_grafico <- bind_rows(resultado_todos_padrao,
  resultado_entre_avaliadores)

ggplot(resultado_global_grafico, aes(x = medida, y = concordancia)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0.12, linewidth = 0.7) +
  ylim(0, 100) +
  labs(title = "Overall Agreement", x = NULL, y = "Concordância (%)") +
  theme_minimal()

# Gráfico: Agreement by Category vs Standard ----
ggplot(resultado_categoria_padrao, aes(x = padrao, y = concordancia)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = li, ymax = ls), width = 0.12, linewidth = 0.7) +
  ylim(0, 100) +
  labs(title = "Agreement by Category vs Standard", x = "Categoria",
    y = "Concordância (%)") +
  theme_minimal()

# Gráfico extra: kappa por avaliador vs padrão ----
ggplot(resultado_kappa_padrao, aes(x = avaliador, y = kappa)) +
  geom_col(width = 0.65, fill = "steelblue") +
  geom_text(aes(label = round(kappa, 3)), vjust = -0.4) +
  ylim(0, 1) +
  labs(title = "Kappa de cada avaliador vs padrão", x = "Avaliador",
    y = "Kappa") +
  theme_minimal()