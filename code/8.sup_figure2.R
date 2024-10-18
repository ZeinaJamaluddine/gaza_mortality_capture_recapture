#..........................................................................................
###      Gaza-CAPTURE-RECAPTURE ANALYSIS Supp-Figure2  ###
#..........................................................................................

#..........................................................................................

# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(scales)
library(extrafont)
library(cowplot)

loadfonts(device = "win")  

# Read the Excel file
month_df <- read_excel("C:/Users/Zeina Jamaluddine/OneDrive - London School of Hygiene and Tropical Medicine/gaza-capture recapture/github/input/supp_month.xlsx")

# Ensure all required columns are present
required_columns <- c("stratum", "unlisted", "unlisted_lci", "unlisted_uci", "total_deaths_est", "total_deaths_lci", "total_deaths_uci")
if(!all(required_columns %in% colnames(month_df))) {
  stop("Missing required columns in the dataframe")
}

# Convert stratum to factor with correct order and labels
month_df <- month_df %>%
  mutate(stratum = factor(stratum, levels = c("oct", "nov", "dec", "jan", "feb", "mar", "apr", "may", "june"),
                          labels = c("Oct 2023", "Nov 2023", "Dec 2023", "Jan 2024", "Feb 2024", "Mar 2024", "Apr 2024", "May 2024", "Jun 2024")))

# Calculate percentage of unlisted deaths and its confidence interval
month_df <- month_df %>% 
  mutate(
    percentage = unlisted / total_deaths_est,
    percentage_lci = unlisted_lci / total_deaths_lci,
    percentage_uci = unlisted_uci / total_deaths_uci
  )

# Create the top plot (estimated deaths)
p_top <- ggplot(month_df, aes(x = stratum, group = 1)) +
  # Total deaths
  geom_ribbon(aes(ymin = total_deaths_lci, ymax = total_deaths_uci), 
              fill = alpha(viridis(10)[1], 0.4)) +
  geom_line(aes(y = total_deaths_est, color = "Total deaths"), size = 0.5) +
  geom_point(aes(y = total_deaths_est, color = "Total deaths"), size = 1) +
  
  # Unlisted deaths
  geom_line(aes(y = unlisted, color = "Unlisted deaths"), size = 0.5) +
  geom_point(aes(y = unlisted, color = "Unlisted deaths"), size = 1) +
  geom_ribbon(aes(ymin = unlisted_lci, ymax = unlisted_uci), 
              fill = alpha(viridis(10)[1], 0.1)) +
  
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_manual(values = c("Total deaths" = viridis(10)[1], 
                                "Unlisted deaths" = alpha(viridis(10)[1], 0.5))) +
  
  labs(x = NULL, 
       y = "Estimated number of deaths") +
  
  theme_minimal(base_family = "Times New Roman") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, family = "Times New Roman", size = 14), # Increase x-axis text size
    axis.text.y = element_text(family = "Times New Roman", size = 14), # Increase y-axis text size
    axis.title.y = element_text(family = "Times New Roman", size = 16), # Increase y-axis title size
    legend.text = element_text(family = "Times New Roman", size = 12), # Increase legend text size
    text = element_text(family = "Times New Roman", size=14), # Increase overall text size
    legend.position = "top",
    legend.title = element_blank()
  )

# Create the bottom plot (percentage of unlisted deaths)
p_bottom <- ggplot(month_df, aes(x = stratum, group = 1)) +
  geom_line(aes(y = percentage), color = "grey50", size = 0.5) +
  geom_point(aes(y = percentage), color =  "grey50", size = 1) +
  
  geom_text(aes(y = percentage, 
                label = scales::percent(percentage, accuracy = 1)),
            vjust = -0.5, size=4.5, family="Times New Roman") +
  
  scale_y_continuous(labels= scales::percent_format(), limits=c(0,1.0), expand=expansion(mult=c(0.05,0.1))) + 
  labs(x="Month",
       y="Percentage of deaths unlisted") + 
  theme_minimal(base_family="Times New Roman") + 
  theme(axis.text.x=element_text(angle=0,hjust=0.5,family="Times New Roman",size=14), 
        axis.text.y=element_text(family="Times New Roman",size=14), # Increase y-axis text size
        axis.title.y=element_text(family="Times New Roman",size=16), # Increase y-axis title size
        text=element_text(family="Times New Roman",size=14)) # Increase overall text size

# Combine the plots
combined_plot <- plot_grid(
  p_top, p_bottom,
  ncol=1,
  align='v',
  axis='lr',
  rel_heights=c(1,1)
)

# Print the final plot
print(combined_plot)

# Save the plot
ggsave("C:/Users/Zeina Jamaluddine/OneDrive - London School of Hygiene and Tropical Medicine/gaza-capture recapture/github/output/6.supp_figure2.pdf", plot = combined_plot, width=12, height=10, device = cairo_pdf, bg = "white")

