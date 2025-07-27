#########################################
########## Tipos de dados no R ##########
#########################################

# Script: Tipos de Dados no R

# Numéricos
num = 10.45  # Número com ponto flutuante

int = 10L  # O "L" indica que é um inteiro

# Lógicos
logic = TRUE  # ou false

# Texto ou Strings
char = "Qualidade e Produtividade"  # Texto

# Fatores
sexo = factor(c("Masculino", "Feminino"))  # Variável categórica

# Data e Hora - ISO 8601
data = as.Date("2025-07-27")  # Data no formato YYYY-MM-DD

# Vetores
vetor_num = c(1, 2, 3, 4, 5)  # Vetor numérico
vetor_char = c("A", "B", "C")  # Vetor de caracteres

str(vetor_num)  # Exibe a estrutura do vetor numérico)
str(vetor_char) # Exibe a estrutura do vetor de caracteres

summary(vetor_num)  # Soma dos elementos do vetor numérico

class(matriz)  # Exibe a classe do objeto matriz
class(int)  # Exibe a classe do objeto inteiro

# Listas
lista = list(
  nome = "João",
  idade = 30,
  altura = 1.75,
  ativo = TRUE
)  # Lista com diferentes tipos de dados
lista


# Matrizes
matriz = matrix(1:9, nrow = 3, ncol = 3)  # Matriz 3x3
matriz

matriz[1, 2]  # Acessa o elemento na primeira linha e segunda coluna

matriz[2, ]  # Acessa a segunda linha inteira
matriz[, 1]  # Acessa a primeira coluna inteira

# Data Frames
df = data.frame(
  nome = c("Ana", "Bruno", "Carlos"),
  idade = c(25, 30, 28),
  altura = c(1.65, 1.80, 1.75),
  sexo = factor(c("Feminino", "Masculino", "Masculino"))
)  # Data frame com colunas de diferentes tipos
df
summary(df)  # Resumo estatístico do data frame
str(df)  # Estrutura do data frame

# Acessando colunas do data frame
df$nome  # Acessa a coluna "nome"
df[["idade"]]  # Acessa a coluna "idade" usando colchetes
df[1:2, ]  # Acessa as duas primeiras linhas do data frame
