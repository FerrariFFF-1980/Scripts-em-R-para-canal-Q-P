#####################
# Gráfico de Linhas #
#####################

# Carregar bibliotecas necessárias
library(dplyr)
library(ggplot2)

# Converter data usando pipes %>% do tidyverse
falhas_pcb <- falhas_pcb %>%
  mutate(Data = as.Date(EventDate))

# Contar defeitos por data
defeitos_por_data <- falhas_pcb %>%
  count(Data, name = "Defeitos") %>%
  arrange(Data)

# Gráfico
ggplot(defeitos_por_data, aes(x = Data, y = Defeitos)) +
  
  # Usar uma linha mais grossa para melhor visualização
  geom_line(linewidth = 1,
            color = "#56B4E9") +
  
  # Adicionar pontos para destacar os valores individuais de defeitos por data
  geom_point(size = 2,
             color = "#56B4E9") +
  
  # Ajustar os rótulos do eixo x para mostrar as datas de forma mais legível
  scale_x_date(date_breaks = "1 week", date_labels = "%d/%m") +
  
  # Adicionar títulos e rótulos aos eixos para melhor compreensão do gráfico
  labs(
    title = "Número de Defeitos por Data",
    x = "Data",
    y = "Número de defeitos"
  ) +
  
  # Usar um tema mais claro para melhor visualização
  theme_light(base_size = 16) +
  
  scale_color_brewer(palette = "Set2") +
  
  # Ajustar a rotação dos rótulos do eixo x para melhor visualização
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####################################
# Multiplas séries no mesmo gráfico #
#####################################

# Converter data usando pipes %>% do tidyverse
falhas_pcb <- falhas_pcb %>%
  mutate(Data = as.Date(EventDate))

# Contar defeitos por data e turno
defeitos_por_data <- falhas_pcb %>%
  count(Data, Shift, name = "Defeitos") %>%
  arrange(Data)

# Gráficos
ggplot(defeitos_por_data, aes(x = Data, y = Defeitos, color = Shift)) +
  
  # Usar uma linha mais grossa para melhor visualização
  geom_line(linewidth = 1) +
  
  # Adicionar pontos para destacar valores individuais de defeitos por data/turno
  geom_point(size = 2) +
  
  # Personalizar as cores para Paleta de Okabe-Ito, amigável para daltônicos
  scale_color_manual(
    values = c(
      "#0072B2",  # azul
      "#E69F00",  # laranja
      "#009E73",  # verde
      "#D55E00",  # vermelho
      "#CC79A7",  # roxo
      "#56B4E9",  # azul claro
      "#F0E442",  # amarelo
      "#000000"   # preto
    )
  ) +

  # Ajustar os rótulos do eixo x para mostrar as datas por semana  
  scale_x_date(date_breaks = "1 week", date_labels = "%d/%m") +
  
  # Adicionar títulos e rótulos aos eixos para melhor compreensão do gráfico
  labs(
    title = "Número de Defeitos por Data e Turno",
    x = "Data",
    y = "Número de defeitos",
    color = "Turno"
  ) +
  
  # Usar um tema mais claro para melhor visualização
  theme_light(base_size = 16) +
  
  # Ajustar a rotação dos rótulos do eixo x para melhor visualização
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Paleta Okabe-Ito
"#0072B2"
"#E69F00"
"#009E73"
"#D55E00"
"#CC79A7"
"#56B4E9"
"#F0E442"
"#000000"

# Paleta Tableau
"#4E79A7"
"#F28E2B"
"#E15759"
"#76B7B2"
"#59A14F"
"#EDC948"

