# Tipos de dados no R

# Numéricos
num = 10.45

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

sum(vetor_num)  # Soma dos elementos do vetor numérico

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

