############
# Boxplots #
############

library(ggplot2)
library(tidyverse)

# Plot
CentrosUsinagem %>%
  ggplot(
    aes(
      x=`Centro de Usinagem`,
      y=`Diâmetro (mm)`,
      fill=`Centro de Usinagem`
    )
  ) +
  
  geom_boxplot() +
  
    # Escala Okabe-Ito
    scale_fill_manual(
    values = c(
      "#0072B2",  # azul
      "#E69F00",  # laranja
      "#009E73"   # verde
      # "#D55E00",  # vermelho
      # "#CC79A7",  # roxo
      # "#56B4E9",  # azul claro
      # "#F0E442",  # amarelo
      # "#000000"   # preto
    )
  ) +
  
  # Opcional - linhas dos limites de especificação
  # geom_hline(
  #   yintercept = c(25.05, 24.95), # Intervalo de especificação
  #   linetype = "dashed",
  #   linewidth = 0.5,
  #   color = "#D55E00"
  # ) +
  
  # Opcional - adição de jitters
  # geom_jitter(
  #   color="black",
  #   size=0.4,
  #   alpha=0.9
  # ) +
  
  theme_light() +
  
  theme(
    legend.position="right",
    plot.title = element_text(size=16)
  ) +
  
  labs (
    title = "Distribuição dos Diâmetros do Eixos [mm]",
    subtitle = "por Centros de Usinagem"
  ) 

# Opcional - girar 90°
# coord_flip()
