# Gráfico de dispersão

library(ggplot2)   # pacote ggplot2 para gráficos

dados <- data.frame(Dispersao, check.names = FALSE) # Transformar em data frame mantendo os nomes das colunas

# Criar o gráfico de dispersão
ggplot(data = dados, aes(x = `Velocidade (m/min)`, y = `Rugosidade Ra (µm)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Adicionar linha de tendência 
  labs(
    title = "Gráfico de Dispersão",
  ) +
  theme_light()
