#..........................................................................................
###      Gaza-CAPTURE-RECAPTURE ANALYSIS Figure 1     ###
#..........................................................................................

#..........................................................................................

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(scales)
library(showtext) 
library(viridis)

# Path
file_path <- "C:/Users/Zeina Jamaluddine/OneDrive - London School of Hygiene and Tropical Medicine/gaza-capture recapture/github/input/1.figure1.xlsx"

data <- read_excel(file_path)

# Convert the Date column to Date type
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")

# Reshape data for ggplot
data_long <- data %>%
  dplyr::select(`Date`, `Hospital list`, `Survey list`, `Missing names`) %>%
  gather(key = "Source", value = "Count", -Date)

# Specify the order of stackin
data_long$Source <- factor(data_long$Source, levels = c("Missing names", "Survey list", "Hospital list"))
levels(data_long$Source)[levels(data_long$Source) == "Missing names"] <- "Unidentified"

# Create the stacked area plot
font_add("Arial", "arial.ttf")
showtext_auto()

p <- ggplot(data_long, aes(x = Date, y = Count, fill = Source)) +
  geom_area() +
  scale_fill_manual(values = c("Unidentified" = "grey90", "Hospital list" = alpha(viridis(4)[2], 0.5), "Survey list" = alpha(viridis(4)[3], 0.5))) +
  labs(
    y = "Cumulative deaths reported",
    x = ""
  ) +
  theme_minimal(base_family = "Arial", base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 18),
    panel.grid = element_blank(),
    aspect.ratio = 1,
    legend.position = "top",
    legend.title = element_blank()  
  ) +
  scale_y_continuous(labels = scales::comma)  

# Define the labels and their corresponding dates
labels <- c("6/9 hospitals reporting\nAl Shifa Siege",
            "5/9 hospitals reporting\n",
            "3/9 hospitals reporting\n",
            "2/9 hospitals reporting\nNasser Siege",
            "3/9 hospitals reporting\n",
            "5/9 hospitlas reporting\n")

# Use the actual dates from the `Hopitals stopped reporting` column
label_dates <- data$Date[!is.na(data$`Hopitals stopped reporting`) & data$`Hopitals stopped reporting` > 0]

# Ensure the number of labels matches the number of dates
if (length(labels) != length(label_dates)) {
  stop("The number of labels does not match the number of dates.")
}

# Add vertical lines and labels for these dates
for (i in seq_along(label_dates)) {
  date <- label_dates[i]
  label_text <- labels[i]
  
  p <- p +
    geom_vline(xintercept = as.numeric(date), color = "#800020", linetype = "dashed") +
    annotate("text", x = date + 10, y = 22000, label = label_text,  
             color = "black", size = 4, angle = 90, vjust = 0, hjust = 0)  
}

p <- p + scale_x_date(breaks = seq(as.Date("2023-05-01"), max(data$Date), by = "month"), labels = date_format("%b %Y"))

print(p)


ggsave("C:/Users/Zeina Jamaluddine/OneDrive - London School of Hygiene and Tropical Medicine/gaza-capture recapture/github/output/1.figure1.pdf", plot = p, width = 10, height = 8, units = "in", device = cairo_pdf, bg = "white")