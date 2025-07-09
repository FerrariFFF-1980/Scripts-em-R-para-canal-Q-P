
# Introdução às operações básicas no R

options(scipen = 999) #Desativar notação científica

# Atribuição de variáveis
a <- 10       # Usando o operador de atribuição "<-"
b <- 5        # Usando o operador de atribuição "=" (não recomendado para atribuições)

# Operações aritméticas básicas
soma <- a + b          # Soma
subtracao <- a - b     # Subtração
multiplicacao <- a * b # Multiplicação
divisao <- a / b       # Divisão
potencia <- a ^ b      # Potência

# Impressão dos resultados
print(paste("Soma: ", soma))
print(paste("Subtração: ", subtracao))
print(paste("Multiplicação: ", multiplicacao))
print(paste("Divisão: ", divisao))
print(paste("Potência: ", potencia))

# Usando funções embutidas
media <- mean(c(a, b))
maximo <- max(a, b)
minimo <- min(a, b)

print(paste("Média: ", media))
print(paste("Máximo: ", maximo))
print(paste("Mínimo: ", minimo))

# Pacotes ──────────────────────────────────────────────────────────────
library(tidyverse)   # carrega ggplot2 + dplyr etc.
library(viridis)     # garante a paleta viridis_d()

# 1. Preparação dos dados ------------------------------------------------
# Usaremos o dataset mtcars e contaremos quantos carros têm cada nº de cilindros
bar_data <- mtcars %>%
  count(cyl) %>%                # cria a variável 'n' (frequência)
  mutate(cyl = factor(cyl))     # transforma em fator para eixo x

# Significado das variáveis:
# • cyl – número de cilindros de cada carro (3 categorias: 4, 6, 8)
# • n   – quantidade de carros em cada categoria de cilindros

# 2. Gráfico de barras ----------------------------------------------------
ggplot(bar_data, aes(x = cyl, y = n, fill = cyl)) +
  geom_col(width = 0.7, color = "black") +
  scale_fill_viridis_d(option = "D", name = "Cilindros") +
  labs(
    title = "Distribuição de Carros por Número de Cilindros",
    x     = "Número de Cilindros",
    y     = "Quantidade de Carros"
  ) +
  theme_light(base_size = 14) +          # visual limpo para gravação em vídeo
  theme(legend.position = "none")        # esconde legenda (opcional)

