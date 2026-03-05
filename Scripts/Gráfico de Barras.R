###############################
# Gráfico de Colunas e Barras #
###############################

# Pacote ggplot2
library(ggplot2)

# Barras simples
ggplot(
  falhas_pcb,
  aes(x = Shift, fill = Shift)) +
  
  geom_bar(color = "white") +
  
  geom_text(
    stat = "count",
    aes(
      y = after_stat(count),
      label = after_stat(count)
    ),
    vjust = -0.4,
    size = 5,
    color = "black"
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  
  labs(
    title = "Falhas por Turno",
    x = "Turno",
    y = "Frequência",
    fill = "Turno"
  ) +
  
  scale_fill_brewer(palette = "Set2") +
  theme_light(base_size = 16) +
  theme(legend.position = "right")
# coord_flip()  # se quiser em barras horizontais


# Barras empilhadas
ggplot(falhas_pcb, aes(x = Shift, fill = Line)) +
  
  geom_bar(color = "white") +
  
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_stack(vjust = 0.5),
    size = 5,
    color = "black"
  ) +
  
  labs(
    title = "Falhas por Linha e Turno",
    x = "Turno",
    y = "Frequência",
    fill = "Linha"
  ) +
  
  scale_fill_brewer(palette = "Set2") +
  theme_light(base_size = 16) +
  theme(legend.position = "right")
# coord_flip()  # se quiser em barras horizontais


# Barras lado a lado
ggplot(falhas_pcb, aes(x = Shift, fill = Line)) +
  
  geom_bar(
    width = 0.8,
    position = "dodge",
    color = "white"
  ) +
  
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    position = position_dodge(width = 0.8),
    vjust = -0.3,
    size = 5,
    color = "black"
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  labs(
    title = "Falhas por Linha e Turno",
    x = "Turno",
    y = "Frequência",
    fill = "Linha"
  ) +
  
  scale_fill_brewer(palette = "Set2") +
  theme_light(base_size = 16) +
  theme(legend.position = "right")
# coord_flip()  # se quiser em barras horizontais (vjust muda para hjust)
 
