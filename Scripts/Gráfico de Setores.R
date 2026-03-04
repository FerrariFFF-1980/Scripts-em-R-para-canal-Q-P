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



# Donut
ggplot(falhas_pcb, aes(x = 2, fill = Shift)) +
  
  # Cria as barras (que depois serão transformadas em setores)
  geom_bar(
    width = 1,
    color = "white"
  ) +
  
  # Converte coordenadas para polares, criando o gráfico de setores
  coord_polar(theta = "y") +
  
  # Ajusta os limites do eixo x para criar o espaço central do donut
  xlim(0.7, 2.5) +
  
  # Adiciona contagem e percentual nas fatias
  geom_text(
    stat = "count",
    aes(
      label = after_stat(
        paste0(count, " (", round(count / sum(count) * 100, 1), "%)")
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 5
  ) +
  
  # Adiciona o texto no centro do donut
  annotate(
    "text",
    x = 0.7,
    y = 0,
    label = paste("Total\n", nrow(falhas_pcb), "falhas"),
    size = 6
  ) +
  
  # Títulos e rótulos
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
