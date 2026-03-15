# Teste do Sinal de Wilcoxon para a Mediana ----
# Diferenças
#
# | Teste    | Informação usada    |
# | -------- | ------------------- |
# | Sinal    | apenas direção      |
# | Wilcoxon | direção + magnitude |
#

# GGplot2 para os gráficos abaixo ----
library(ggplot2)

# Paleta Okabe-Ito ----
pal <- c(Positivo="#009E73", Negativo="#D55E00")

# Dados da amostra ----
x <- c(12,15,14,10,16,13,11,15,14,12)

# Mediana hipotética ----
m0 <- 13

# Diferenças ----
d <- x - m0

# Remove empates ----
d <- d[d != 0]

# Postos dos valores absolutos ----
postos <- rank(abs(d))

# Classificação dos sinais ----
sinal <- ifelse(d > 0,"Positivo","Negativo")

# Estatística W (soma dos postos positivos) ----
W <- sum(postos[d > 0])

# Teste de Wilcoxon do R ----
teste <- wilcox.test(x, mu = m0, exact = TRUE)
teste

# p-valor ----
p_valor <- teste$p.value

# Resultados ----
cat("W =", W, "\n")
cat("p-valor =", p_valor, "\n")

# Data frame para gráfico ----
df <- data.frame(observacao = factor(1:length(d)),
                 diferenca = d, posto = postos, sinal = sinal)

# Gráfico das diferenças ----
ggplot(df,aes(observacao,diferenca,fill=sinal)) +
  geom_col() +
  geom_hline(yintercept=0,linetype="dashed") +
  scale_fill_manual(values=pal) +
  labs(title="Diferenças em relação à mediana", x="Observação", y="x - mediana hipotética") +
  theme_minimal()

# Gráfico dos postos ----
ggplot(df,aes(observacao,posto,fill=sinal)) +
  geom_col() +
  scale_fill_manual(values=pal) +
  labs(title="Postos absolutos usados no teste de Wilcoxon", x="Observação", y="Posto")+
  theme_minimal()
