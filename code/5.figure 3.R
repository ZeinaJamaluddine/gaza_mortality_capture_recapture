#..........................................................................................
###      Gaza-CAPTURE-RECAPTURE ANALYSIS Figure 3    ###
#..........................................................................................

#..........................................................................................

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(viridis)
library(gridExtra)
library(extrafont)
library(ggthemes)
loadfonts(device = "win")

# Read the Excel file
rate_df <- read_excel("C:/Users/Zeina Jamaluddine/OneDrive - London School of Hygiene and Tropical Medicine/gaza-capture recapture/github/input/3.figure3.xlsx")

# Define the correct age order and labels
age_order <- c("0", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
               "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
               "70-74", "75-79", "80 plus")

# Reshape the data
rate_df_long <- rate_df %>%
  pivot_longer(
    cols = c(starts_with("estimate_"), starts_with("lci_"), starts_with("uci_")),
    names_to = c(".value", "gender"),
    names_pattern = "(estimate|lci|uci)_(.+)"
  ) %>%
  filter(gender %in% c("male", "female")) %>%
  mutate(age = factor(age, levels = rate_df$age, labels = age_order),
         gender = factor(gender, levels = c("male", "female"), labels = c("Male", "Female")))

# Prepare pre-war data
prewar_data <- rate_df %>%
  dplyr::select(age, estimate_prewar) %>%
  rename(estimate = estimate_prewar) %>%
  mutate(gender = "Pre-war", 
         age = factor(age, levels = rate_df$age, labels = age_order))

# Combine the data
rate_df_combined <- bind_rows(rate_df_long, prewar_data) %>%
  mutate(gender = factor(gender, levels = c("Male", "Female", "Pre-war")))

# Create custom color palette using Viridis
custom_colors <- viridis(3, option = "D", end = 0.8)

# Create the plot
p <- ggplot(rate_df_combined, aes(x = age, y = estimate, color = gender, group = gender)) +
  geom_line(data = rate_df_combined %>% filter(gender != "Pre-war"), size = 1.2) +
  geom_line(data = rate_df_combined %>% filter(gender == "Pre-war"), size = 1.2, linetype = "dashed") +
  geom_ribbon(data = rate_df_long, aes(ymin = lci, ymax = uci, fill = gender), alpha = 0.2, color = NA) +
  scale_color_manual(values = setNames(custom_colors, c("Male", "Female", "Pre-war"))) +
  scale_fill_manual(values = setNames(custom_colors[1:2], c("Male", "Female"))) +
  labs(y = "Annualised age-specific mortality rate per 1,000",
       x = "Age group (years)",
       ) +
  theme_few() +
  theme(
    text = element_text(family = "Arial", size = 12),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = NA),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = age_order) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(rate_df_combined$estimate) * 1.1)) +
  guides(fill = "none")

# Create rate ratio table
rate_ratio_table <- rate_df %>%
  mutate(age = factor(age, levels = rate_df$age, labels = age_order)) %>%
  dplyr::select(age, rate_ratio, lci_rateratio, uci_rateratio) %>%
  mutate(
    rate_ratio_text = sprintf("%.1f (%.1f-%.1f)", rate_ratio, lci_rateratio, uci_rateratio)
  ) %>%
  dplyr::select(age, rate_ratio_text) %>%
  rename("Age Group\n(years)" = age, "Rate Ratio (95% CI)" = rate_ratio_text)

# Convert the table to a grob with larger text
table_grob <- tableGrob(rate_ratio_table, rows = NULL, theme = ttheme_minimal(
  core = list(fg_params = list(fontsize = 8, fontfamily = "Arial")),
  colhead = list(fg_params = list(fontsize = 9, fontface = "bold", fontfamily = "Arial"))
))

# Combine plot and table
combined_plot <- grid.arrange(p, table_grob, ncol = 2, widths = c(3, 1))

ggsave("C:/Users/Zeina Jamaluddine/OneDrive - London School of Hygiene and Tropical Medicine/gaza-capture recapture/github/output/3.figure3.pdf", plot = combined_plot, width = 200, height = 200, units = "mm", device = cairo_pdf, bg = "white")

