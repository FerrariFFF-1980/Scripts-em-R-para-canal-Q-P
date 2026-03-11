# Análise Qui-quadrado ----
## Arquivo: Chi-quadrado.xlsx ----

# Configurações ----

## Evita notação científica ----
options(scipen = 999, digits = 6)

## PALETA OKABE-ITO ----
okabe_ito <- c(
  "#E69F00",  # laranja
  "#56B4E9",  # azul claro
  "#009E73",  # verde
  "#F0E442",  # amarelo
  "#0072B2",  # azul
  "#D55E00",  # vermelho
  "#CC79A7",  # roxo
  "#000000"   # preto
)

## Carrega pacotes necessários ----
library(ggplot2)
library(readxl)

## Importar dados ----
dados <- read_excel("Dados/Chi-quadrado.xlsx")

# Preparação dos dados ----

## Converte as variáveis categóricas para fator ----
dados$CentroUsinagem <- as.factor(dados$CentroUsinagem)
dados$TipoDefeito <- as.factor(dados$TipoDefeito)

## Cria a tabela de contingência a partir do arquivo ----
tabela_obs <- xtabs(
  Frequencia ~ CentroUsinagem + TipoDefeito,
  data = dados
)

tabela_obs

## Adição dos Totais ----
total_linha <- rowSums(tabela_obs)
total_coluna <- colSums(tabela_obs)
total_geral <- sum(tabela_obs)

## Calcular a tabela de valores esperados ----
tabela_esp <- outer(total_linha, total_coluna) / total_geral

## Mantém nomes de linhas e colunas ----
rownames(tabela_esp) <- rownames(tabela_obs)
colnames(tabela_esp) <- colnames(tabela_obs)

tabela_esp

# Análise Qui-quadrado ----

## Contribuição Chi-quadrado ----
chi_contrib <- (tabela_obs - tabela_esp)^2 / tabela_esp
chi_contrib

## Teste Chi-quadrado ----
teste_chi <- chisq.test(tabela_obs)
teste_chi

## Converte para data frame ----
obs_df <- as.data.frame(as.table(tabela_obs))
esp_df <- as.data.frame(as.table(tabela_esp))
chi_df <- as.data.frame(as.table(chi_contrib))

colnames(obs_df) <- c("Centro", "Defeito", "Valor")
colnames(esp_df) <- c("Centro", "Defeito", "Valor")
colnames(chi_df) <- c("Centro", "Defeito", "Chi2")

obs_df$Tipo <- "Observado"
esp_df$Tipo <- "Esperado"

dados_barras <- rbind(obs_df, esp_df)

## Gráfico Observado vs. Esperado ----
ggplot(dados_barras,
  aes(x = Centro, y = Valor, fill = Tipo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = round(Valor, 2)),
        position = position_dodge(width = 0.8),
        vjust = -0.3,
        size = 4) +
  facet_wrap(~ Defeito) +
  scale_fill_manual(values = c(
      "Observado" = okabe_ito[5],   # azul
      "Esperado" = okabe_ito[1])) + # laranja
  labs(
    title = "Frequências Observadas vs Esperadas",
    x = "Centro de Usinagem", y = "Frequência", fill = "Tipo") +
  theme_light() +
  theme(legend.position = "right")

## Gráfico de Contribuição para χ² ----
ggplot(chi_df, aes(x = Defeito, y = Chi2, fill = Centro)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = round(Chi2, 2)),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 4) +
  scale_fill_manual(
    values = c(
      "Centro 1" = okabe_ito[5],      # azul
      "Centro 2" = okabe_ito[1],      # laranja
      "Centro 3" = okabe_ito[3])) +   # verde
  labs(
    title = expression("Contribuição para a estatística " * chi^2),
    x = "Tipo de Defeito", y = expression((O - E)^2 / E),
    fill = "Centro de Usinagem") +
  theme_light() +
  theme(legend.position = "right")