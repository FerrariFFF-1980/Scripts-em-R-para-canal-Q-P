# Teste Não-paramétrico do Sinal para a Mediana ----

# Biblioteca ggplot2 ----
library(ggplot2)

# Dados da amostra ----
x <- c(12, 15, 14, 10, 16, 13, 11, 15, 14, 12)

# Mediana hipotética ----
m0 <- 13

# Diferença entre observação e mediana hipotética ----
d <- (x - m0)

# Classificação dos sinais ----
sign <- ifelse(d > 0,"Positivo",
               ifelse(d < 0,"Negativo","Empate"))

# Estatística do teste S (número de sinais positivos) ----
S <- sum(sign == "Positivo")

# Tamanho efetivo da amostra (empates removidos) ----
n <- sum(sign != "Empate")

# Teste exato usando distribuição binomial ----
teste_exato <- binom.test(S,n,0.5)
p_exato <- teste_exato$p.value

# Aproximação normal ----
z <- (S - n/2) / sqrt(n/4)
z

# p-valor bicaudal da aproximação normal ----
p_normal <- 2 * pnorm(-abs(z))

# Resultados ----
cat("S =",S,"\n")
cat("n =",n,"\n")
cat("p-valor exato =", p_exato,"\n")
cat("p-valor aproximacao normal =", p_normal,"\n")

# Data frame para gráfico ----
df <- data.frame(observacao = factor(1:length(x)),
                 diferenca = d, sinal = sign)

# Paleta Okabe-Ito ----
pal <- c(Positivo = "#009E73", Negativo = "#D55E00", Empate = "#999999")

# Gráfico dos sinais ----
ggplot(df,aes(observacao,diferenca,fill=sinal)) +
  geom_col() +
  geom_hline(yintercept=0,linetype="dashed") +
  scale_fill_manual(values=pal) +
  labs(title="Teste do Sinal", y="x - mediana hipotetica", x="Observacao") +
  theme_minimal()