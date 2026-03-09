# Evita notação científica
options(scipen = 999)

# Pacotes utilizados para criação e formatação de tabelas
library(knitr)       # criação de tabelas simples
library(kableExtra)  # melhoria visual das tabelas
library(gt)          # tabelas mais elaboradas
library(dplyr)       # manipulação de dados

# Exemplo de base de dados
# Cada linha representa um centro de usinagem
# com algumas estatísticas do processo

tabela_exemplo <- data.frame(
  Centro_Usinagem = c("Centro 1", "Centro 2", "Centro 3"),
  Media_Diametro_mm = c(10.245, 10.118, 10.332),
  Desvio_Padrao_mm = c(0.032, 0.041, 0.028),
  Peca_Fora_Especificacao = c(3, 7, 2),
  Percentual_Refugo = c(1.5, 3.5, 1.0)
)

# ----------------------------------
# Tabela simples usando knitr::kable
# ----------------------------------

kable(
  tabela_exemplo,
  caption = "Resumo dos centros de usinagem",
  align = c("l", "c", "c", "c", "c"),  # alinhamento das colunas
  digits = 2                           # número de casas decimais
)

# ----------------------------------------------
# Tabela com formatação visual usando kableExtra
# ----------------------------------------------

kable(
  tabela_exemplo,
  caption = "Resumo dos centros de usinagem",
  align = c("l", "c", "c", "c", "c"),
  digits = c(0, 0, 3, 3, 0, 1)
) %>%
  kable_styling(
    full_width = FALSE,                # evita que a tabela ocupe toda a largura
    bootstrap_options = c(
      "striped",                       # linhas alternadas
      "hover",                         # destaque ao passar o mouse
      "condensed"                      # espaçamento mais compacto
    ),
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE) %>%         # destaca o cabeçalho
  column_spec(1, bold = TRUE) %>%      # destaca a primeira coluna
  footnote(
    general = "Dados fictícios utilizados apenas para exemplo."
  )

# -----------------------------------------
# Tabela utilizando o pacote gt
# Mais controle sobre aparência e rotulagem
# -----------------------------------------

tabela_exemplo %>%
  gt() %>%
  tab_header(
    title = "Resumo dos centros de usinagem",
    subtitle = "Exemplo de tabela formatada com o pacote gt"
  ) %>%
  cols_label(
    Centro_Usinagem = "Centro de Usinagem",
    Media_Diametro_mm = "Média do diâmetro (mm)",
    Desvio_Padrao_mm = "Desvio-padrão (mm)",
    Peca_Fora_Especificacao = "Peças fora da especificação",
    Percentual_Refugo = "Refugo (%)"
  ) %>%
  fmt_number(
    columns = c(Media_Diametro_mm, Desvio_Padrao_mm),
    decimals = 3                       # casas decimais para medições
  ) %>%
  fmt_number(
    columns = Percentual_Refugo,
    decimals = 1                       # casas decimais para porcentagem
  ) %>%
  tab_source_note(
    source_note = "Fonte: dados fictícios para fins didáticos."
  )

# -------------------------------------------
# Exemplo de destaque condicional
# Valores maiores recebem cores mais intensas
# -------------------------------------------

tabela_exemplo %>%
  gt() %>%
  tab_header(
    title = "Tabela com destaque visual"
  ) %>%
  cols_label(
    Centro_Usinagem = "Centro",
    Media_Diametro_mm = "Média (mm)",
    Desvio_Padrao_mm = "Desvio-padrão",
    Peca_Fora_Especificacao = "Peças fora",
    Percentual_Refugo = "Refugo (%)"
  ) %>%
  fmt_number(
    columns = c(Media_Diametro_mm, Desvio_Padrao_mm),
    decimals = 3
  ) %>%
  data_color(
    columns = Percentual_Refugo,
    method = "numeric"
  )

# ---------------------------------------------------
# Ordenação da tabela pelo maior percentual de refugo
# ---------------------------------------------------

tabela_ordenada <- tabela_exemplo %>%
  arrange(desc(Percentual_Refugo))

kable(
  tabela_ordenada,
  caption = "Centros ordenados pelo percentual de refugo",
  align = c("l", "c", "c", "c", "c"),
  digits = c(0, 0, 3, 3, 0, 1)
) %>%
  kable_styling(
    full_width = FALSE,
    position = "center"
  ) %>%
  row_spec(0, bold = TRUE)