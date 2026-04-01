# Gage R&R Cruzado no R
# Arquivo: GageRR.xlsx

# Pacote Six Sigma ----
# install.packages("SixSigma") # se o pacote não estiver instalado

# Evita notação científica ----
options(scipen=999)

# Pacote SixSigma no R ----
library(SixSigma)

# Função de geração do estudo R&R Cruzado ----
# O mesmo conjunto de peças é medido por todos os operadores
resultado_rr <- ss.rr(var = medida, part = parte,
                      appr = operador, data = dados_gage,
                      lsl = 40, usl = 60, sigma = 6,
                      method = "crossed", digits = 4,
                      main = "Estudo Gage R&R")

# Função de geração do estudo R&R Aninhado ----
# Conjunto EQUIVALENTE mas não igual de peças é
# medido por cada operador
resultado_rr <- ss.rr(var = medida, part = parte,
                      appr = operador, data = dados_nested,
                      lsl = 40, usl = 60, sigma = 6,
                      method = "nested", digits = 4,
                      main = "Estudo Gage R&R")

