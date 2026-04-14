# TYPE 1 GAGE STUDY No R ----

# Pacotes ----
library(ggplot2)
library(patchwork)
library(scales)

# Paleta Okabe-Ito ----
okabe_ito_blue <- "#0072B2"
okabe_ito_green <- "#009E73"
okabe_ito_red <- "#D55E00"
okabe_ito_black <- "#000000"

# Dados de entrada ----
measurement_values <- c(0.02498, 0.02500, 0.02501, 0.02496, 0.02500, 0.02503, 0.02498, 0.02492, 0.02499, 0.02501,
                        0.02506, 0.02492, 0.02499, 0.02501, 0.02503, 0.02492, 0.02498, 0.02494, 0.02501, 0.02493,
                        0.02500, 0.02498, 0.02504, 0.02493, 0.02500, 0.02494, 0.02501, 0.02498, 0.02505, 0.02495,
                        0.02489, 0.02503, 0.02506, 0.02498, 0.02510, 0.02489, 0.02493, 0.02504, 0.02503, 0.02498,
                        0.02499, 0.02504, 0.02498, 0.02492, 0.02503, 0.02493, 0.02494, 0.02495, 0.02493)

characteristic_name <- "Thickness"
gage_name <- "Caliper 0-150mm"
study_date <- "27/03/2026"
reported_by <- "Fernando F. Fernandes"
misc_text <- "Preliminary study for later Gage R&R study"

reference_value <- 0.0250
tolerance_value <- 0.0007
bias_test_value <- 0
study_variation_multiplier <- 6
capability_tolerance_fraction <- 0.20

measurement_digits <- 5
standard_deviation_digits <- 6
p_value_digits <- 3
capability_digits <- 2
percent_digits <- 2

# Funções auxiliares ----
format_number <- function(numeric_value, digits, percent = FALSE) {
  if (is.na(numeric_value)) {
    return("")
  }
  
  formatted_value <- formatC(numeric_value, format = "f",
                             digits = digits, decimal.mark = ",")
  
  if (percent) {
    paste0(formatted_value, "%")
  } else {
    formatted_value
  }
}

create_text_panel <- function(panel_text, left_margin = 20, right_margin = 20) {
  ggplot() +
    annotate("text", x = 0, y = 1, label = panel_text, hjust = 0, vjust = 1,
      family = "mono", size = 3.0, color = "black", parse = FALSE) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_text_panel +
    theme(plot.margin = margin(2, right_margin, 4, left_margin))
}

# Cálculos ----
number_of_observations <- length(measurement_values)
observation_index <- seq_len(number_of_observations)

mean_value <- mean(measurement_values)
standard_deviation_value <- sd(measurement_values)
study_variation_value <- study_variation_multiplier * standard_deviation_value

bias_value <- mean_value - reference_value
t_statistic_value <- bias_value / (standard_deviation_value / sqrt(number_of_observations))
degrees_of_freedom <- number_of_observations - 1
p_value <- 2 * pt(-abs(t_statistic_value), df = degrees_of_freedom)

upper_reference_line <- reference_value + 0.10 * tolerance_value
lower_reference_line <- reference_value - 0.10 * tolerance_value

cg_value <- (capability_tolerance_fraction * tolerance_value) / study_variation_value
cgk_value <- ((capability_tolerance_fraction * tolerance_value) - (2 * abs(bias_value))) / study_variation_value

repeatability_percent <- 100 * study_variation_value / tolerance_value
repeatability_and_bias_percent <- 100 * (study_variation_value + abs(bias_value)) / tolerance_value

# Base do gráfico ----
chart_data <- data.frame(
  Observation = observation_index,
  Measurement = measurement_values)

y_axis_minimum <- min(c(measurement_values, lower_reference_line))
y_axis_maximum <- max(c(measurement_values, upper_reference_line))
y_axis_padding <- 0.00002

label_vertical_offset <- 0.02 * (y_axis_maximum - y_axis_minimum)
label_x_position <- number_of_observations - 0.15

# Temas ----
theme_for_chart <- theme(
  plot.background = element_rect(fill = "white", color = NA),
  panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  axis.line = element_blank(), axis.ticks = element_line(color = "black", linewidth = 0.3),
  axis.text = element_text(size = 7, color = "black"), 
  axis.title = element_text(size = 8, color = "black"),
  plot.title = element_text(size = 10, hjust = 0.5, color = "black"), 
  plot.margin = margin(2, 2, 2, 2))

theme_text_panel <- theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(0, 0, 0, 0))

# Cabeçalho superior do gráfico ----
header_plot <- ggplot() +
  annotate("text", x = 0.00, y = 0.92,
    label = paste0("Type 1 Gage Study for ", characteristic_name),
    hjust = 0, vjust = 1, size = 5.5, color = "black") +
  annotate("text", x = 0.00, y = 0.52, label = "Gage name:", hjust = 0, size = 3.1, family = "mono") +
  annotate("text", x = 0.00, y = 0.30, label = "Date of study:", hjust = 0, size = 3.1, family = "mono") +
  
  annotate("text", x = 0.18, y = 0.52, label = gage_name, hjust = 0, size = 3.1, family = "mono") +
  annotate("text", x = 0.18, y = 0.30, label = study_date, hjust = 0, size = 3.1, family = "mono") +
  
  annotate("text", x = 0.42, y = 0.52, label = "Reported by:", hjust = 0, size = 3.1, family = "mono") +
  annotate("text", x = 0.42, y = 0.30, label = "Tolerance:", hjust = 0, size = 3.1, family = "mono") +
  annotate("text", x = 0.42, y = 0.08, label = "Misc:", hjust = 0, size = 3.1, family = "mono") +
  
  annotate("text", x = 0.60, y = 0.52, label = reported_by, hjust = 0, size = 3.1, family = "mono") +
  annotate("text", x = 0.60, y = 0.30, label = format_number(tolerance_value, measurement_digits), hjust = 0, size = 3.1, family = "mono") +
  annotate("text", x = 0.60, y = 0.08, label = misc_text, hjust = 0, size = 3.1, family = "mono") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_text_panel +
  theme(plot.margin = margin(8, 8, 2, 8))

# Run Chart ----
run_chart_plot <- ggplot(chart_data, aes(x = Observation, y = Measurement)) +
  geom_line(color = okabe_ito_blue, linewidth = 0.75) +
  geom_point(color = okabe_ito_blue, size = 2.5) +
  geom_hline(yintercept = upper_reference_line, color = okabe_ito_red, linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = reference_value, color = okabe_ito_green, linetype = "dashed", linewidth = 0.5) +
  geom_hline(yintercept = lower_reference_line, color = okabe_ito_red, linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = label_x_position, y = upper_reference_line + label_vertical_offset,
    label = "Ref + 0,10 × Tol", hjust = 1, vjust = 0, size = 2.5, color = "black") +
  annotate("text", x = label_x_position, y = reference_value + label_vertical_offset,
    label = "Ref", hjust = 1, vjust = 0, size = 2.5, color = "black") +
  annotate("text", x = label_x_position, y = lower_reference_line - label_vertical_offset,
    label = "Ref - 0,10 × Tol", hjust = 1, vjust = 1, size = 2.5, color = "black") +
  scale_x_continuous(breaks = seq(1, number_of_observations, by = 5),
    limits = c(1, number_of_observations), expand = expansion(mult = c(0.01, 0.005))) +
  scale_y_continuous(labels = label_number(accuracy = 10^(-measurement_digits), decimal.mark = ","),
    limits = c(y_axis_minimum - y_axis_padding, y_axis_maximum + y_axis_padding),
    expand = c(0, 0)) +
  labs(title = paste0("Run Chart of ", characteristic_name),
    x = "Observation", y = characteristic_name) +
  theme_for_chart +
  theme(plot.margin = margin(0, 12, 0, 28))

# Texto das estatísticas ----
basic_statistics_text <- paste0(
  "Basic Statistics\n\n",
  "Reference           ", format_number(reference_value, measurement_digits), "\n",
  "Mean                ", format_number(mean_value, measurement_digits), "\n",
  "StDev               ", format_number(standard_deviation_value, standard_deviation_digits), "\n",
  "6 × StDev (SV)      ", format_number(study_variation_value, standard_deviation_digits), "\n",
  "Tolerance (Tol)     ", format_number(tolerance_value, measurement_digits))

bias_statistics_text <- paste0(
  "Bias\n\n",
  "Bias                ", format_number(bias_value, standard_deviation_digits), "\n",
  "T                   ", format_number(t_statistic_value, 7), "\n",
  "PValue              ", format_number(p_value, p_value_digits), "\n",
  "(Test Bias = ", bias_test_value, ")")

capability_statistics_text <- paste0(
  "Capability\n\n",
  "Cg                           ", format_number(cg_value, capability_digits), "\n",
  "Cgk                          ", format_number(cgk_value, capability_digits), "\n\n",
  "%Var(Repeatability)          ", format_number(repeatability_percent, percent_digits, percent = TRUE), "\n",
  "%Var(Repeatability and Bias) ", format_number(repeatability_and_bias_percent, percent_digits, percent = TRUE))

# Painéis de texto ----
basic_statistics_plot <- create_text_panel(
  panel_text = basic_statistics_text, left_margin = 45, right_margin = 20)

bias_statistics_plot <- create_text_panel(
  panel_text = bias_statistics_text, left_margin = 20, right_margin = 20)

capability_statistics_plot <- create_text_panel(
  panel_text = capability_statistics_text, left_margin = 20, right_margin = 45)

statistics_row <- basic_statistics_plot + bias_statistics_plot + capability_statistics_plot +
  plot_layout(widths = c(1.25, 0.95, 1.35))

# Composição final ----
final_plot <- header_plot / run_chart_plot / statistics_row +
  plot_layout(heights = c(0.24, 0.53, 0.23))

print(final_plot)
