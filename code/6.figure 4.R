#..........................................................................................
###      Gaza-CAPTURE-RECAPTURE ANALYSIS Figure 4   ###
#..........................................................................................

#..........................................................................................
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(viridis)
library(gridExtra)
library(stringr)

# Read the Excel file (replace with your actual file path)
rate_df <- read_excel("C:/Users/Zeina Jamaluddine/OneDrive - London School of Hygiene and Tropical Medicine/gaza-capture recapture/Analysis/imp_fc_02112024/input/4.figure4_unadj.xlsx")

# Define the correct age order
age_order <- c("0 to 14 y", "15 to 29 y", "30 to 44 y", "45 to 59 y", "60+ y")

# Reshape the data
rate_df_long <- rate_df %>%
  dplyr::select(age, 
                rate_male_2023, rate_lci_male_2023, rate_uci_male_2023,
                rate_female_2023, rate_lci_female_2023, rate_uci_female_2023) %>%
  tidyr::pivot_longer(
    cols = c(rate_male_2023, rate_female_2023),
    names_to = "gender",
    values_to = "rate"
  ) %>%
  dplyr::mutate(
    gender = dplyr::if_else(gender == "rate_male_2023", "Male", "Female"),
    lci = dplyr::if_else(gender == "Male", rate_lci_male_2023, rate_lci_female_2023),
    uci = dplyr::if_else(gender == "Male", rate_uci_male_2023, rate_uci_female_2023),
    age = factor(str_replace(age, "y", " y"), levels = age_order),
    gender = factor(gender, levels = c("Male", "Female"))
  ) %>%
  dplyr::select(age, gender, rate, lci, uci)

# Create custom color palette using Viridis
custom_colors <- viridis(2, option = "D", end = 0.8)

# Create the dot plot
p <- ggplot(rate_df_long, aes(x = age, y = rate, color = gender)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lci, ymax = uci), 
                position = position_dodge(width = 0.5), 
                width = 0.2) +
  scale_color_manual(values = setNames(custom_colors, c("Male", "Female"))) +
  labs(y = "Annualised age-specific mortality rate per 1,000",
       x = "Age group (years)",
       color = "Sex") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", size = 12),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, margin = margin(t = 10)),
    axis.text.y = element_text(size = 10, margin = margin(r = 10)),
    axis.title = element_text(size = 12),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(margin = margin(r = 20)),
    plot.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_discrete(breaks = age_order) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(rate_df_long$uci) * 1.1))

# Create rate ratio table
rate_ratio_table <- rate_df %>%
  dplyr::select(age, rate_ratio, lci_rateratio, uci_rateratio) %>%
  dplyr::mutate(
    age = factor(str_replace(age, "y", " y"), levels = age_order),
    rate_ratio_text = sprintf("%.1f(%.1f-%.1f)", rate_ratio, lci_rateratio, uci_rateratio)
  ) %>%
  dplyr::select(age, rate_ratio_text)

# Rename columns without using rename()
names(rate_ratio_table) <- c("Age Group", "Rate Ratio\n2023/2022 (95% CI)")

# Convert the table to a grob
table_grob <- tableGrob(rate_ratio_table, rows = NULL, theme = ttheme_minimal(
  core = list(fg_params = list(fontsize = 8)),
  colhead = list(fg_params = list(fontsize = 9, fontface = "bold"))
))

# Combine plot and table
combined_plot <- grid.arrange(p, table_grob, ncol = 2, widths = c(3, 1))

# Display the combined plot
print(combined_plot)

# Save the plot (optional)

# Save the combined plot as a high-resolution PDF
ggsave(paste0(dir_path, "output/6.figure4.pdf"),
       plot = combined_plot,
       width = 200,
       height = 150,
       units = "mm",
       device = cairo_pdf,
       bg = "white")
