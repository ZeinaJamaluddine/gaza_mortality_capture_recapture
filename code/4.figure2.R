#..........................................................................................
###      Gaza-CAPTURE-RECAPTURE ANALYSIS Figure 2  ###
#..........................................................................................

#..........................................................................................

#................................... 

# Load required libraries
library(tidyverse)
library(gridExtra)
library(lubridate)
library(viridis)
library(cowplot)
library(extrafont)
library(scales)
library(eulerr)

# Set up the environment
loadfonts(device = "win")
windowsFonts(Arial = windowsFont("Arial"))
set.seed(123)

# Define common variables
common_font_size <- 10
dir_path <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path), "/")
setwd(dir_path)
dir_path <- gsub("/code", "", dir_path)
suppressWarnings(dir.create(paste0(dir_path, "output")))

# Read dataset
df <- as.data.frame(read_excel(paste0(dir_path, "input/2.gaza_list_capture_analysis_pub.xlsx"), sheet = "Gaza"))
df <- df[, c("id", "list1", "list2", "list3", "age", "gender", "month_death", "year_death", "gov_ns", "gov")]

# Convert month_death to factor with correct order
df$month_death <- factor(df$month_death, 
                         levels = c(10, 11, 12, 1, 2, 3, 4, 5, 6),
                         labels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"),
                         ordered = TRUE)

# Define colors and palettes
hospital_color <- alpha(viridis(4)[2], 0.5)
survey_color <- alpha(viridis(4)[3], 0.5)
social_media_color <- alpha(viridis(4)[4], 0.5)
color_palette <- c("Hospital" = hospital_color, "Survey" = survey_color, "Social Media" = social_media_color)
gender_color_palette <- c(
  "Hospital.female" = alpha(hospital_color, 0.3), "Hospital.male" = alpha(hospital_color, 0.5),
  "Survey.female" = alpha(survey_color, 0.3), "Survey.male" = alpha(survey_color, 0.5),
  "Social Media.female" = alpha(social_media_color, 0.3), "Social Media.male" = alpha(social_media_color, 0.5)
)

# Define common theme and functions
common_theme <- theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    axis.text = element_text(size = common_font_size),
    axis.title = element_text(size = common_font_size),
    legend.title = element_blank(),
    legend.text = element_text(size = common_font_size)
  )

add_space <- function(plot) {
  plot + theme(
    plot.margin = unit(c(0.5, 0.5, 0.5, 1.5), "cm"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )
}

# Monthly distribution plot
month_order <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun")
monthly_plot <- df %>%
  group_by(month_death) %>%
  summarise(
    Hospital = sum(list1),
    Survey = sum(list2),
    `Social Media` = sum(list3)
  ) %>%
  pivot_longer(cols = c(Hospital, Survey, `Social Media`), names_to = "Source", values_to = "Count") %>%
  group_by(Source) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Source = factor(Source, levels = c("Hospital", "Survey", "Social Media"))
  ) %>%
  ggplot(aes(x = month_death, y = Percentage, color = Source, group = Source)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = color_palette) +
  scale_x_discrete(limits = month_order) +
  scale_y_continuous(labels = comma) +
  labs(x = "Month", y = "Percentage") +
  common_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    legend.direction = "horizontal"
  )

monthly_plot <- add_space(monthly_plot)

# Age distribution plot with separate filtering and percentages for each list
age_plot <- df %>%
  # Create separate data frames for each list where the list equals 1
  filter(list1 == 1) %>%
  mutate(Source = "Hospital") %>%
  bind_rows(
    df %>% filter(list2 == 1) %>% mutate(Source = "Survey"),
    df %>% filter(list3 == 1) %>% mutate(Source = "Social Media")
  ) %>%
  # Create age groups
  mutate(age_group = cut(age, breaks = c(0, 14, 29, 44, 59, Inf), include.lowest = TRUE, 
                         labels = c("0 to 14y", "15 to 29y", "30 to 44y", "45 to 59y", "60+y"))) %>%
  filter(!is.na(age_group)) %>%
  # Group by Source and age group, then calculate count and percentage
  group_by(Source, age_group) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Source) %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  # Define Source order
  mutate(Source = factor(Source, levels = c("Hospital", "Survey", "Social Media"))) %>%
  # Plot
  ggplot(aes(x = age_group, y = Percentage, fill = Source)) +
  geom_col(position = position_dodge(width = 0.9), show.legend = TRUE) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 2.5) +
  scale_fill_manual(values = color_palette) +
  scale_y_continuous(labels = comma) +
  labs(x = "Age groups", y = "Percentage within each source") +
  common_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

age_plot <- add_space(age_plot)
# Gender proportion plot (unchanged)
gender_color_palette <- c(
  "Hospital.female" = alpha(hospital_color, 0.3),
  "Hospital.male" = alpha(hospital_color, 0.5),
  "Survey.female" = alpha(survey_color, 0.3),
  "Survey.male" = alpha(survey_color, 0.5),
  "Social Media.female" = alpha(social_media_color, 0.3),
  "Social Media.male" = alpha(social_media_color, 0.5)
)

gender_plot <- df %>%
  group_by(gender) %>%
  summarise(
    Hospital = sum(list1),
    Survey = sum(list2),
    `Social Media` = sum(list3)
  ) %>%
  pivot_longer(cols = c(Hospital, Survey, `Social Media`), names_to = "Source", values_to = "Count") %>%
  group_by(Source) %>%
  mutate(
    Proportion = Count / sum(Count),
    Source = factor(Source, levels = c("Hospital", "Survey", "Social Media")),
    gender = factor(gender, levels = c("female", "male"))
  ) %>%
  filter(!is.na(gender)) %>%
  ggplot(aes(x = Source, y = Proportion * 100, fill = interaction(Source, gender))) +
  geom_col(position = "stack", show.legend = TRUE) +
  geom_text(aes(label = paste0(ifelse(gender == "female", "female: ", "male: "), 
                               round(Proportion * 100, 1), "% n=", comma(Count))),
            position = position_stack(vjust = 0.5), 
            size = 3,
            angle = 0,
            hjust = 0.5,
            color = "grey30") +
  scale_fill_manual(values = gender_color_palette) +
  labs(x = "List", y = "Percentage") +
  common_theme

gender_plot <- add_space(gender_plot)

# Combine plots
combined_plot <- plot_grid(
  monthly_plot,
  plot_grid(age_plot + theme(legend.position = "none"),
            gender_plot + theme(legend.position = "none"),
            ncol = 1, rel_heights = c(1, 0.7)),
  ncol = 1,
  rel_heights = c(1.2, 1.7)
)

# Display the combined plot
print(combined_plot)

# Save the combined plot as a high-resolution PDF
ggsave(paste0(dir_path, "output/2.figure2.pdf"),
       plot = combined_plot,
       width = 200,
       height = 247,
       units = "mm",
       device = cairo_pdf,
       bg = "white")
