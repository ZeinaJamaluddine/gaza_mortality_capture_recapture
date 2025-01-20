#..........................................................................................
###      Gaza-CAPTURE-RECAPTURE ANALYSIS Figure 5 ###
#..........................................................................................

#..........................................................................................

#................................... 


# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(geodata)
library(viridis)
library(tidyr)

# Specify the path to your Excel file
file_path <- "C:/Users/Zeina Jamaluddine/OneDrive - London School of Hygiene and Tropical Medicine/gaza-capture recapture/Analysis/imp_fc_02112024/input/5.figure5.xlsx"

data <- read_excel(file_path)

# Read the Gaza shapefile from the zip file
temp_dir <- tempdir()
unzip("C:/Users/Zeina Jamaluddine/OneDrive - London School of Hygiene and Tropical Medicine/gaza-capture recapture/github/input/pse_adm_pamop_20231019_shp.zip", exdir = temp_dir)
gaza_map <- st_read(file.path(temp_dir, "pse_admbnda_adm2_pamop_20231019.shp"))

# Filter and process the data
gaza_data <- data %>%
  filter(admin1 == "Gaza Strip" & !is.na(admin2)) %>%
  mutate(event_date = ymd(event_date),
         month = floor_date(event_date, "month")) %>%
  filter(month <= ymd("2024-06-30"))

# Summarize fatalities
monthly_data <- gaza_data %>%
  group_by(month, admin2) %>%
  summarise(fatalities = sum(fatalities), .groups = "drop") %>%
  mutate(admin2 = factor(admin2, levels = c("North Gaza", "Gaza City", "Deir El Balah", "Khan Yunis", "Rafah")))

monthly_totals <- monthly_data %>%
  group_by(month) %>%
  summarise(total_fatalities = sum(fatalities), .groups = "drop")

# Define colors
governorate_levels <- c("North Gaza", "Gaza City", "Deir El Balah", "Khan Yunis", "Rafah")
custom_colors <- alpha(viridis(length(governorate_levels), option = "D", begin = 0.1, end = 0.9), 0.8)
names(custom_colors) <- governorate_levels

# Prepare the map
gaza_map <- gaza_map %>%
  filter(ADM1_EN == "Gaza Strip") %>%
  st_transform(4326) %>%
  mutate(admin2 = recode(ADM2_EN,
                         "Gaza" = "Gaza City",
                         "Deir Al-Balah" = "Deir El Balah",
                         "Khan Younis" = "Khan Yunis",
                         .default = ADM2_EN))

# Base theme
base_theme <- theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0, face = "plain", size = 14),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, margin = margin(r = 10)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Plot 1A: Cumulative Fatalities
plot1A <- ggplot(monthly_data, aes(x = month, y = fatalities, fill = admin2)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(data = monthly_totals, aes(x = month, y = total_fatalities, label = total_fatalities),
            vjust = -0.5, size = 3, inherit.aes = FALSE) +
  scale_fill_manual(values = custom_colors) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Monthly Deaths by Governorate", y = "Deaths") +
  base_theme

# Plot 1B: Proportion of Fatalities
plot1B <- ggplot(monthly_data, aes(x = month, y = fatalities, fill = admin2)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = custom_colors) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(title = "Monthly Proportion of Deaths by Governorate", y = "Proportion") +
  base_theme

# Map plot
plot_map <- ggplot(gaza_map) +
  geom_sf(aes(fill = admin2)) +
  scale_fill_manual(values = custom_colors) +
  geom_sf_text(aes(label = admin2), size = 3, color = "black") +
  theme_void() +
  theme(legend.position = "none")+
  annotate("text", x = Inf, y = Inf, label = "Legend", hjust = 2, vjust = 1, size = 5)


# Destruction data
destruction_data <- data.frame(
  date = as.Date(c("2023-10-10", "2023-10-15", "2023-11-07", "2023-11-26", "2024-01-06", "2024-02-29", "2024-05-03", "2024-07-06")),
  North = c(0, 13, 26, 35, 49, 55, 77, 82),
  Gaza_city = c(1, 4, 12, 22, 38, 42, 66, 68),
  Deir_el_balah = c(0, 2, 5, 7, 17, 21, 41, 43),
  Khan_Younis = c(0, 0, 6, 7, 29, 52, 76, 75),
  Rafah = c(0, 2, 4, 4, 5, 6, 14, 58)
)

destruction_data_long <- pivot_longer(destruction_data, cols = -date, names_to = "Governorate", values_to = "Destruction") %>%
  mutate(Governorate_num = as.numeric(factor(Governorate, levels = c("Rafah", "Khan_Younis", "Deir_el_balah", "Gaza_city", "North"))))

# Destruction plot
plot_destruction <- ggplot(destruction_data_long, aes(x = Governorate_num, y = date)) +
  geom_tile(aes(fill = Destruction), color = "black", size = 0.5, width = 0.8, height = 15) +
  geom_text(aes(label = Destruction), color = "black", size = 3, fontface = "plain", vjust = 0.5, hjust = 0.5) +
  scale_fill_viridis(option = "D", direction = -1, begin = 0.1, end = 0.9, alpha = 0.7, name = "% Destruction") +
  scale_y_date(date_breaks = "1 month", date_labels = "%b") +
  scale_x_continuous(breaks = 1:5, labels = c("Rafah", "Khan Yunis", "Deir el Balah", "Gaza City", "North"),
                     expand = expansion(mult = c(0.1, 0.1))) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    plot.title = element_text(size = 14),
    axis.text.y = element_text(angle = 0, hjust = 1),
    panel.grid = element_blank(),
    legend.position = "right",
    axis.title.x = element_text(angle = 0, vjust = 0.5)
  ) +
  labs(title = "Progression of Destruction in Gaza Governorates", y = "Date", x = "Governorate") +
  coord_flip()
# Combine all plots without excess space, keeping top and bottom same size
top_plot <- (plot1A + plot1B + plot_map) + 
  plot_layout(ncol = 3, widths = c(2, 2, 1)) & 
  theme(plot.margin = margin(5, 5, 5, 5))  

# Adjust destruction plot size and font for consistency
plot_destruction <- plot_destruction + 
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    plot.title = element_text(size = 12)
  )
# Combine all plots without excess space, keeping top section size fixed
top_plot <- (plot1A + plot1B + plot_map) + 
  plot_layout(ncol = 3, widths = c(2, 2, 1)) & 
  theme(plot.margin = margin(10, 5, 5, 5))  

# Adjust destruction plot size and font for consistency
plot_destruction <- plot_destruction + 
  theme(
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10),
    plot.title = element_text(size = 12)
  )

# Combine the plots, reducing the size of the bottom plot
combined_plot <- plot_spacer()/top_plot /plot_spacer()/ plot_destruction / plot_spacer() +
  plot_layout(ncol = 1, heights = c(0.5, 1,0.1,1,0.5)) & 
  theme(plot.margin = margin(30, 10, 10, 30))  

# Save the combined plot as a PDF


ggsave("C:/Users/Zeina Jamaluddine/OneDrive - London School of Hygiene and Tropical Medicine/gaza-capture recapture/Analysis/imp_fc_02112024/output/6.figure5.pdf", plot = combined_plot, width = 12, height = 14, units = "in", device = cairo_pdf, bg = "white")

