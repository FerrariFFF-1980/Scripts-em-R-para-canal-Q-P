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
potencia <- a^b        # Potência

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
