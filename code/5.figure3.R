#..........................................................................................
###      Gaza-CAPTURE-RECAPTURE ANALYSIS Figure 3  ###
#..........................................................................................

#..........................................................................................

#...................................      
## Starting setup
library(tidyverse)
library(eulerr)
library(gridExtra)
library(viridis)
library(extrafont)

loadfonts(device = "win")  
# Clean up from previous code / runs
rm(list=ls(all=T) )

# Set font
windowsFonts(Arial=windowsFont("Arial"))

# Set working directory to where this file is stored
dir_path <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")
setwd(dir_path)
print( getwd() )
dir_path <- gsub("/code", "", dir_path)
suppressWarnings(dir.create(paste0(dir_path, "output")))

# Initialise random numbers
set.seed(123)

# Colour-blind palette for graphing
palette_gen <- viridis(16)
show_col(palette_gen)


#...................................      
## Read dataset and source functions

# Dataset (and streamline columns)
df <- as.data.frame(read_excel(paste0(dir_path, 
                                      "input/2.gaza_list_capture_analysis_pub.xlsx"), sheet = "Gaza"))
x <- c("id", "list1", "list2", "list3", "age", "gender", "month_death",
       "year_death", "gov_ns", "gov")
df <- df[, x]



# Define colors using viridis palette
hospital_color <- alpha(viridis(4)[2], 0.5)
survey_color <- alpha(viridis(4)[3], 0.5)
social_media_color <- alpha(viridis(4)[4], 0.5)

# Function to create Euler diagram with counts, percentages, and labels outside
create_euler <- function(data, title) {
  set1 <- which(data$list1 == 1)
  set2 <- which(data$list2 == 1)
  set3 <- which(data$list3 == 1)
  
  venn_data <- list(
    Hospital = set1,
    Survey = set2,
    "Social Media" = set3
  )
  
  fit <- euler(venn_data)
  
  # Calculate total number of observations
  total_obs <- nrow(data)
  
  # Create custom labels with counts and percentages
  custom_labels <- sapply(fit$original.values, function(x) {
    sprintf("%d\n(%.1f%%)", x, x / total_obs * 100)
  })
  
  plot(fit, 
       quantities = custom_labels,
       fills = c(hospital_color, survey_color, social_media_color),
       edges = list(col = "white", lwd = 0),
       labels = list(font = 0.3, family = "Times New Roman", cex = 1, col = "black"),  
       quantity = list(font = 0.3, family = "Times New Roman", cex = 1, col = "black"), 
       legend = FALSE,  # Remove legend
       shape = "ellipse",  # Use ellipses instead of circles
       shape_args = list(h = 5, w = 5))  # Adjust the size of the ellipses
}

# Create Euler diagrams
euler_overall <- create_euler(df, "Overall")
euler_male <- create_euler(df %>% filter(gender == "male"), "Male")
euler_female <- create_euler(df %>% filter(gender == "female"), "Female")

# Combine Euler diagrams with overall on top
combined_euler <- grid.arrange(
  euler_overall,
  arrangeGrob(euler_male, euler_female, ncol = 2),
  nrow = 2,
  heights = c(1, 1)
  
)



# Display the plot
print(euler_overall)
# Save the combined plot as a high-resolution PDF
ggsave(paste0(dir_path, "output/5.supp_figure1.pdf"),
       plot = euler_overall,
       width = 200,
       height = 200,
       units = "mm",
       device = cairo_pdf,
       bg = "white")



