# Estudo da Linearidade e Tendência do Instrumento ----
# Gage Linearity and Bias Study

library(ggplot2)
library(dplyr)
library(grid)

# Informações do report preenchidas pelo usuário ----
gage_name <- "Paquímetro 150 mm"
date_of_study <- "2026-03-27"
reported_by <- "Paco"
tolerance <- "0,80"
misc <- "Operador A / 12 repetições"

# Fonte e cores ----
fonte_mono <- "mono"

cor_dados <- "#0072B2"
cor_regressao <- "#D55E00"
cor_ic <- "#009E73"
cor_bias_medio <- "black"

# Margens da página ----
margem_pagina_esquerda <- 0.03
margem_pagina_direita <- 0.03
margem_pagina_superior <- 0.04
margem_pagina_inferior <- 0.03

# Dados de entrada ----
dados_medicao <- data.frame(
  Part = rep(1:5, each = 12),
  Master = rep(c(2, 4, 6, 8, 10), each = 12),
  Response = c(2.7, 2.5, 2.4, 2.5, 2.7, 2.3, 2.5, 2.5, 2.4, 2.4, 2.6, 2.4,
               5.1, 3.9, 4.2, 5.0, 3.8, 3.9, 3.9, 3.9, 3.9, 4.0, 4.1, 3.8,
               5.8, 5.7, 5.9, 5.9, 6.0, 6.1, 6.0, 6.1, 6.4, 6.3, 6.0, 6.1,
               7.6, 7.7, 7.8, 7.7, 7.8, 7.8, 7.8, 7.7, 7.8, 7.5, 7.6, 7.7,
               9.1, 9.3, 9.5, 9.3, 9.4, 9.5, 9.5, 9.5, 9.6, 9.2, 9.3, 9.4))

# Cálculo do bias ----
dados_medicao <- dados_medicao %>%
  mutate(Bias = Response - Master)

# Bias médio por referência ----
bias_medio_por_referencia <- dados_medicao %>%
  group_by(Master) %>%
  summarise(AverageBias = mean(Bias),
            .groups = "drop")

# Modelo de regressão ----
modelo <- lm(Bias ~ Master, data = dados_medicao)
resumo <- summary(modelo)

b0 <- resumo$coefficients[1, 1]
b1 <- resumo$coefficients[2, 1]
se0 <- resumo$coefficients[1, 2]
se1 <- resumo$coefficients[2, 2]
p0 <- resumo$coefficients[1, 4]
p1 <- resumo$coefficients[2, 4]

S <- resumo$sigma
R2 <- resumo$r.squared * 100

# Bias geral ----
bias_medio <- mean(dados_medicao$Bias)
p_bias <- t.test(dados_medicao$Bias, mu = 0)$p.value

# Bias por referência ----
bias_por_ref <- dados_medicao %>%
  group_by(Master) %>%
  summarise(AverageBias = mean(Bias), PValue = t.test(Bias, mu = 0)$p.value,
    .groups = "drop")

# Formatação ----
fmt <- function(x, d = 5) sprintf(paste0("%.", d, "f"), x)
fmt_p <- function(x) sprintf("%.3f", x)

# Cabeçalhos ----
cab_esq <- paste0("Gage name: ", gage_name, "\n", "Date of study: ", date_of_study)

cab_dir <- paste0("Reported by: ", reported_by, "\n", 
  "Tolerance: ", tolerance, "\n", "Misc: ", misc)

# Texto linearidade ----
txt_lin <- paste("Predictor     Coef         SE Coef    P",
                 sprintf("Constant   %10s   %10s   %6s", fmt(b0), fmt(se0), fmt_p(p0)),
                 sprintf("Slope      %10s   %10s   %6s", fmt(b1), fmt(se1), fmt_p(p1)),
                 "", sprintf("S   %s    R-Sq   %.1f%%", fmt(S, 6), R2), sep = "\n")

# Texto bias ----
linhas_bias <- c(sprintf("Average   %10s   %6s", fmt(bias_medio, 6), fmt_p(p_bias)))

for (indice_linha in seq_len(nrow(bias_por_ref))) {
  linhas_bias <- c(
    linhas_bias,
    sprintf("%-8s  %10s   %6s",
            bias_por_ref$Master[indice_linha],
            fmt(bias_por_ref$AverageBias[indice_linha], 6),
            fmt_p(bias_por_ref$PValue[indice_linha])))
}

txt_bias <- paste("Reference     Bias        P",
                  paste(linhas_bias, collapse = "\n"),  sep = "\n")

# Grade de predição ----
n_pred <- max(300, 50 * n_distinct(dados_medicao$Master))

pred <- data.frame(Master = seq(min(dados_medicao$Master),
                                max(dados_medicao$Master),
                                length.out = n_pred))

ci <- predict(modelo, newdata = pred, interval = "confidence")
pred$fit <- ci[, 1]
pred$lwr <- ci[, 2]
pred$upr <- ci[, 3]

# Limites eixo X ----
valores_x <- sort(unique(dados_medicao$Master))
min_x <- min(valores_x)
max_x <- max(valores_x)

if (length(valores_x) >= 2) {
  passo_x <- min(diff(valores_x))
  margem_x <- 0.15 * passo_x
} else {
  margem_x <- 0.5
}

lim_x <- c(min_x - margem_x, max_x + margem_x)

# Limites eixo Y ----
y_min <- min(dados_medicao$Bias, pred$lwr, na.rm = TRUE)
y_max <- max(dados_medicao$Bias, pred$upr, na.rm = TRUE)

quebras_y <- pretty(c(y_min, y_max), n = 5)
lim_y <- c(min(quebras_y), max(quebras_y))

# Gráfico ----
grafico_principal <- ggplot() +
  geom_point(data = dados_medicao, aes(Master, Bias),
             color = cor_dados, size = 1.8) +
  geom_point(data = bias_medio_por_referencia, aes(Master, AverageBias),
             color = cor_bias_medio, shape = 15, size = 2.8) +
  geom_line(data = pred, aes(Master, fit), color = cor_regressao,
            linewidth = 0.8) +
  geom_line(data = pred, aes(Master, lwr), color = cor_ic, linetype = "dashed",
            linewidth = 0.6) +
  geom_line(data = pred, aes(Master, upr), color = cor_ic, linetype = "dashed",
            linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "black") +
  scale_x_continuous(breaks = valores_x, limits = lim_x, expand = c(0, 0)) +
  scale_y_continuous(breaks = quebras_y, limits = lim_y, expand = c(0, 0)) +
  labs(x = "Master", y = "Bias") +
  theme_minimal(base_size = 10) +
  theme(text = element_text(family = fonte_mono),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "#DDDDDD", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
        axis.line = element_blank(), plot.margin = margin(2, 2, 2, 2))

# Layout da página ----
layout_pagina <- grid.layout(nrow = 16, ncol = 24,
                             heights = unit(c(1.2, 0.8, 0.8,
                                       rep(1, 10), 0.6, 0.6, 0.6), "null"),
                              widths = unit(rep(1, 24), "null"))

# Página ----
grid.newpage()
grid.rect(gp = gpar(fill = "white", col = NA))

pushViewport(viewport(x = margem_pagina_esquerda, y = margem_pagina_inferior,
                      width = 1 - margem_pagina_esquerda - margem_pagina_direita,
                      height = 1 - margem_pagina_superior - margem_pagina_inferior,
                      just = c("left", "bottom"), layout = layout_pagina))

# Título ----
grid.text("Gage Linearity and Bias Report for Response", x = 0, y = 1,
  just = c("left", "top"), gp = gpar(fontsize = 18),
  vp = viewport(layout.pos.row = 1, layout.pos.col = 1:24))

# Cabeçalhos ----
grid.text(cab_esq,  x = 0, y = 1, just = c("left", "top"),
  gp = gpar(fontfamily = fonte_mono, fontsize = 11),
  vp = viewport(layout.pos.row = 2:3, layout.pos.col = 1:10))

grid.text(cab_dir, x = 0, y = 1, just = c("left", "top"),
  gp = gpar(fontfamily = fonte_mono, fontsize = 11),
  vp = viewport(layout.pos.row = 2:3, layout.pos.col = 13:24))

# Gráfico ----
print(grafico_principal, vp = viewport(layout.pos.row = 4:13, layout.pos.col = 1:11))

# Legenda ----
pushViewport(viewport(layout.pos.row = 4:7, layout.pos.col = 12:14))

y_legenda <- c(0.82, 0.60, 0.38, 0.16)
x_texto_legenda <- 0.42
x_linha_ini <- 0.05
x_linha_fim <- 0.30
x_ponto <- 0.175

grid.lines(x = unit(c(x_linha_ini, x_linha_fim), "npc"),
  y = unit(rep(y_legenda[1], 2), "npc"),
  gp = gpar(col = cor_regressao, lwd = 1.4))

grid.lines(x = unit(c(x_linha_ini, x_linha_fim), "npc"),
  y = unit(rep(y_legenda[2], 2), "npc"),
  gp = gpar(col = cor_ic, lwd = 1.2, lty = 2))

grid.points(x = unit(x_ponto, "npc"),
  y = unit(y_legenda[3], "npc"),
  pch = 16, size = unit(2.5, "mm"),
  gp = gpar(col = cor_dados))

grid.points(x = unit(x_ponto, "npc"), y = unit(y_legenda[4], "npc"),
  pch = 15, size = unit(3.0, "mm"), gp = gpar(col = cor_bias_medio))

grid.text(c("Regression", "95% CI", "Data", "Avg Bias"),
  x = unit(rep(x_texto_legenda, 4), "npc"), y = unit(y_legenda, "npc"),
  just = "left", gp = gpar(fontfamily = fonte_mono, fontsize = 9))

popViewport()

# Bloco Gage Linearity ----
grid.text("Gage Linearity", x = 0, y = 1, just = c("left", "top"),
  gp = gpar(fontfamily = fonte_mono, fontsize = 11),
  vp = viewport(layout.pos.row = 4, layout.pos.col = 15:24))

grid.text(txt_lin, x = 0, y = 1, just = c("left", "top"),
  gp = gpar(fontfamily = fonte_mono, fontsize = 10),
  vp = viewport(layout.pos.row = 5:8, layout.pos.col = 15:24))

# Bloco Gage Bias ----
grid.text("Gage Bias", x = 0, y = 1, just = c("left", "top"),
  gp = gpar(fontfamily = fonte_mono, fontsize = 11),
  vp = viewport(layout.pos.row = 9, layout.pos.col = 15:24))

grid.text(txt_bias, x = 0, y = 1, just = c("left", "top"),
  gp = gpar(fontfamily = fonte_mono, fontsize = 10),
  vp = viewport(layout.pos.row = 10:13, layout.pos.col = 15:24))

popViewport()