# Média Aritmética
# Diâmetro médio de eixo / Espessura média de chapa / Peso médio de envase
mean(PesoPeixes$Peso)

# Mediana
# Medida de Tendência Central robusta a valores extremos
median(PesoPeixes$Peso)
median(PesoPeixes$PesoExtremo)

# Moda
# Valor mais frequente (que mais se repete)
tabela <- table(PesoPeixes$Peso)
moda <- as.numeric(names(tabela)[tabela == max(tabela)])

# Desvio Padrão
# Medida de dispersão que indica o quanto os dados estão afastados da média
sd(PesoPeixes$Peso)
sd(PesoPeixes$PesoExtremo)

# Coeficiente de Variação
# Medida de dispersão relativa, em porcentagem, indica a variabilidade dos dados em relação à média
cv <- (sd(PesoPeixes$Peso) / mean(PesoPeixes$Peso)) * 100
cv_natural <- (sd(PesoPeixes$Peso) / mean(PesoPeixes$Peso))

# Estatística Descritiva
# Consolidado com várias estatísticas descritivas
# Usando o pacote DescTools
# install.packages("DescTools")
library(DescTools)
Desc(PesoPeixes$Peso)

