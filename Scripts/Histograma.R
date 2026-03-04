# Histogramas no R
# install.packages("ggplot2")        # Instala o pacote ggplot2 para visualização de dados
library(ggplot2)                     # Carrega o pacote ggplot2 para visualização de dados

dados <- data.frame(PesoPeixes$Peso) # Cria um data frame com a variável Peso

média    <- mean(dados$Peso)               # Média dos pesos
desv_pad <- sd(dados$Peso)                 # Desvio padrão dos pesos

ggplot(dados, aes(x = dados$Peso)) +       # Cria o gráfico usando o pacote ggplot2
  geom_histogram(                    # Adiciona o histograma
    aes(y = after_stat(density)),    # Normaliza o histograma para mostrar a densidade
    bins = 8,                        # Número de bins (barras) no histograma 
    fill = "#FF44FF",             # Cor de preenchimento das barras
    color = "black",                 # Cor da borda das barras
  ) +
  
# opcional: adição de contagem de frequência em cada barra do histograma 
#     geom_text(
#     stat = "bin",
#     bins = 8,
#     aes(label = after_stat(count),
#         y = after_stat(density)),
#     vjust = -0.2,
#     size = 5
#   ) +

  stat_function(                                  # Adiciona a curva normal ao gráfico
  fun = dnorm,                                    # Função de densidade normal
  args = list(mean = média, sd = desv_pad),       # Parâmetros da curva normal
  xlim = c(média - 3.5*desv_pad,                  # Limites do eixo x para curva (média ± 3 desvios padrão)
           média + 3.5*desv_pad),
           linewidth = 1.2,                       # Largura da linha da curva normal
  ) +
  
  labs(                                             # Adiciona títulos e rótulos aos eixos
    title = "Histograma com Curva Normal",          # Título do gráfico
    subtitle = "Distribuição dos Pesos dos Peixes", # Subtítulo do gráfico
    x = "Valor",                                    # Rótulo do eixo x
    y = "Densidade"                                 # Rótulo do eixo y
  ) +
  
  theme_light(base_size = 16)                       # Aplica tema claro com tamanho de fonte base de 16

