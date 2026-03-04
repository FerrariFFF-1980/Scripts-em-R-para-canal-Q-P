# Gráfico de setores (Pie Chart)

library(ggplot2)

ggplot(falhas_pcb, aes(x = "", fill = InspectionStep)) +
  
  # Cria as barras (que depois serão transformadas em setores)
  geom_bar(
    width = 1,
    color = "white"
  ) +
  
  # Converte o gráfico de barras em gráfico de setores
  coord_polar(theta = "y") +
  
  # Adiciona contagem e percentual nas fatias
  geom_text(
    stat = "count",
    aes(
      label = after_stat(
        paste0(count, " (", round(count / sum(count) * 100, 1), "%)")
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "black"
  ) +
  
  labs(
    title = "Distribuição de Falhas por Turno",
    fill = "Turno"
  ) +
  
  # Paleta de cores
  scale_fill_brewer(palette = "Set2") +
  
  # Remove eixos e grades (melhor para pie chart)
  theme_void(base_size = 16) +
  
  # Posição da legenda
  theme(legend.position = "right")
