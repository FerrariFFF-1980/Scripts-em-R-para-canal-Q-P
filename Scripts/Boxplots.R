############
# Boxplots #
############

# Pacotes
library(tidyverse)
library(viridis)

# Datasets
data <- data.frame(
  name=c(
    rep("A",500),
    rep("B",500),
    rep("B",500),
    rep("C",20),
    rep('D', 100)
  ),
  value=c(
    rnorm(500, 10, 5),
    rnorm(500, 13, 1),
    rnorm(500, 18, 1),
    rnorm(20, 25, 4),
    rnorm(100, 12, 1)
  )
)

# Gráfico
data %>%
  ggplot(
    aes(x=name,
        y=value,
        fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE,
                     alpha=0.6) +
  geom_jitter(color="#808080",
              size=0.3,
              alpha=0.6,
              width = 0.4) +
  theme(
    legend.position="none",
    plot.title = element_text(size=16)
  ) +
  ggtitle("A boxplot with jitter") +
  theme_light() +
  xlab("")
