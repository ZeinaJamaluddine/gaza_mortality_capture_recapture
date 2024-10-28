library(readr)
library(dplyr)
library(stringr)

# Set the directory path
csv_dir <- "C:/Users/Zeina Jamaluddine/OneDrive - London School of Hygiene and Tropical Medicine/gaza-capture recapture/github/output"

# Function to extract numerical data from a single CSV file
extract_data <- function(file_path) {
  # Read the CSV file
  lines <- readLines(file_path)
  
  # Function to extract numbers from a line
  extract_numbers <- function(line) {
    as.numeric(str_extract_all(line, "-?\\d+\\.?\\d*e?-?\\d*")[[1]])
  }
  
  # Extract numbers from each line
  numbers <- lapply(lines, extract_numbers)
  
  # Return the list of numbers
  return(numbers)
}

# Get all CSV files in the directory matching the pattern
csv_files <- list.files(path = csv_dir, pattern = "gaza_analysis_output_imp\\d+\\.csv$", full.names = TRUE)

# Process all files and combine the data
all_data <- lapply(csv_files, extract_data)

# Calculate averages
avg_data <- lapply(seq_along(all_data[[1]]), function(i) {
  row_data <- lapply(all_data, `[[`, i)
  row_data <- row_data[lengths(row_data) > 0]
  if (length(row_data) > 0) {
    colMeans(do.call(rbind, row_data), na.rm = TRUE)
  } else {
    numeric(0)
  }
})

# Function to replace numbers in a line with averaged numbers
replace_numbers <- function(line, avg_numbers) {
  if (length(avg_numbers) == 0) return(line)
  numbers <- str_extract_all(line, "-?\\d+\\.?\\d*e?-?\\d*")[[1]]
  for (i in seq_along(numbers)) {
    if (i <= length(avg_numbers)) {
      line <- sub(numbers[i], sprintf("%.2f", avg_numbers[i]), line, fixed = TRUE)
    }
  }
  return(line)
}

# Read the structure from the first file
template_lines <- readLines(csv_files[1])

# Replace numbers in the template with averaged numbers
output_lines <- mapply(replace_numbers, template_lines, avg_data, SIMPLIFY = FALSE)

# Write the output to a new CSV file in the same directory
output_file <- file.path(csv_dir, "averaged_output.csv")
writeLines(unlist(output_lines), output_file)

print(paste("Averaged data has been written to", output_file))