# teste não-paramétrico do sinal para a mediana ----

# biblioteca ggplot2 ----
library(ggplot2)

# paleta okabe-ito ----
pal <- c("+" = "#009E73", "-" = "#D55E00", "Empate" = "#999999")

# dados da amostra ----
x <- c(12, 15, 14, 10, 16, 13, 11, 15, 14, 12)

# mediana hipotética ----
m0 <- 13

# diferença entre observação e mediana hipotética ----
d <- x - m0

# classificação dos sinais ----
sign <- ifelse(d > 0, "+",
               ifelse(d < 0, "-", "Empate"))

# estatística do teste s (número de sinais positivos) ----
S <- sum(sign == "+")

# tamanho efetivo da amostra (empates removidos) ----
n <- sum(sign != "Empate")

# teste exato usando distribuição binomial ----
teste_exato <- binom.test(S, n, 0.5)
teste_exato

p_exato <- teste_exato$p.value

# aproximação normal ----
z <- (S - n / 2) / sqrt(n / 4)
z

# p-valor bicaudal da aproximação normal ----
p_normal <- 2 * pnorm(-abs(z))

# resultados ----
cat("S =", S, "\n")
cat("n =", n, "\n")
cat("p-valor exato =", p_exato, "\n")
cat("p-valor aproximacao normal =", p_normal, "\n")

# data frame para gráfico ----
df <- data.frame(
  observacao = factor(1:length(x)),
  diferenca = d,
  sinal = factor(sign, levels = c("+", "-", "Empate")))

# gráfico dos sinais ----
ggplot(df, aes(observacao, diferenca, fill = sinal)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(
    data = subset(df, sinal == "Empate"),
    aes(observacao, 0.2, label = "Empate"),
    inherit.aes = FALSE,
    size = 3) +
  scale_fill_manual(values = pal) +
  labs(title = "Gráfico das Diferenças")+
  theme_minimal()
