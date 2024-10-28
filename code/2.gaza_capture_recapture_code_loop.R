#..........................................................................................
###      Gaza-CAPTURE-RECAPTURE ANALYSIS      ###
#..........................................................................................

# Install and load required packages
x1 <- c("BMA", "data.table", "ggpubr", "ggvenn", "gtools", "leaps", "lubridate", "MASS", "mclogit", "mlogitBMA",
        "RColorBrewer", "readxl", "Rfast", "scales", "tidyverse", "mice", "dplyr")
x2 <- x1 %in% row.names(installed.packages())
if (any(x2 == FALSE)) { install.packages(x1[! x2]) }
lapply(x1, library, character.only = TRUE)

# Setup
rm(list=ls(all=TRUE))
windowsFonts(Arial=windowsFont("Arial"))
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))

input <- file.path(getwd(), "input")
output <- file.path(getwd(), "output")
if (!dir.exists(output)) { dir.create(output) }

set.seed(123)
palette_cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Functions
create_age_groups <- function(age) {
  cut(age, breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, Inf),
      labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", 
                 "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", 
                 "65-69", "70-74", "75-79", "80+"),
      right = FALSE)
}

############ CHANGE to 1000 
impute_age <- function(data, iterations = 2) {
  imp <- mice(data, m = iterations, method = "pmm", maxit = 10, seed = 123)
  imputed_datasets <- lapply(1:iterations, function(i) {
    dataset <- complete(imp, i)
    dataset$age_group <- create_age_groups(dataset$age)
    return(dataset)
  })
  return(imputed_datasets)
}

write_results_to_file <- function(out, n_lists, filename) {
  if (n_lists == 2) {
    write.table(out, filename, append = TRUE, row.names = FALSE, col.names = TRUE, sep = ",")
    write.table(rbind(rep("......................", 2)), filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
  } else if (n_lists %in% c(3, 4)) {
    write.table("  Unformatted output:", filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")      
    write.table("    Candidate models:", filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
    write.table(out$out_raw, filename, append = TRUE, row.names = FALSE, na = "", sep = ",")
    write.table("    Estimated deaths:", filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
    write.table(out$out_est_raw, filename, append = TRUE, row.names = FALSE, col.names = TRUE, na = "", sep = ",")
    write.table("    List sensitivity:", filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
    write.table(out$out_sens_raw, filename, append = TRUE, row.names = FALSE, na = "", sep = ",")
    write.table(rbind(rep("-----", 2)), filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
    write.table("  Formatted output:", filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")      
    write.table("    Candidate models:", filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
    write.table(out$out_pretty, filename, append = TRUE, row.names = FALSE, col.names = TRUE, na = "", sep = ",")
    write.table(rbind(rep("......................", 2)), filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
  }
}

# Source bespoke functions
source(file.path(getwd(), "code", "3.bespoke_functions.R"), echo = TRUE)

# Read data and parameters
file_path <- file.path(input, "2.gaza_list_capture_analysis.xlsx")
pars <- as.data.frame(read_excel(file_path, sheet = "parameters"))

x1 <- gsub("[^[:alnum:] ]", "", pars$location)
x1 <- trimws(gsub("\\s+", " ", x1))
x1 <- tolower(gsub(" ", "_", x1))
pars[, "loc_df"] <- x1

for (i in 1:nrow(pars)) {
  if (pars[i, "analyse"] == "Y") {
    x1 <- as.data.frame(read_excel(file_path, sheet = pars[i, "location"]))
    assign(pars[i, "loc_df"], x1)
  }
}

# Prepare data for analysis
pars[, "n_lists"] <- apply(pars[, c("list1", "list2", "list3", "list4")], 1, function(x) {length(which(!is.na(x)))})
pars_in <- subset(pars, analyse == "Y")

for (i in pars_in[, "loc_df"]) { 
  assign(i, f_clean(get(i), paste(i), pars, f_clean_month, f_clean_date))
}

# Visualize data and prepare contingency tables
for (i in pars_in[, "loc_df"] ) { 
  f_describe(get(i), paste(i), pars, palette_cb)
  assign(paste(i, "_overlap", sep = ""), f_overlap(get(i), i, pars, palette_cb))
}

# Analyze data
for (i in pars_in[, "loc_df"]) { 
  print(paste("Now doing analysis for this site:", pars_in[pars_in[, "loc_df"] == i, "location"]))
  
  original_df <- get(i)
  imputed_datasets <- impute_age(original_df)
  
  for (imp_index in 1:length(imputed_datasets)) {
    df <- imputed_datasets[[imp_index]]
    
    ######### FIGURE OUT WHY exposure stopped working! it was working! 
    ######### generate a gender age categorical variable
    ######### The gender as confounder  works 
    pars_index <- which(pars_in[, "loc_df"] == i)
    pars_in[pars_index, "confounders"] <- "age, month_death"
    pars_in[pars_index, "exposure"] <- "gender"
    
    overlap <- f_overlap(df, i, pars_in, palette_cb)
    
    n_lists <- pars_in[pars_in[, "loc_df"] == i, "n_lists"]
    filename <- file.path(output, paste0(i, "_analysis_output_imp", imp_index, ".csv"))
    
    for (j in 1:nrow(overlap)) {
      print(paste("  Working on this stratum:", gsub("_", " ", overlap[j, "stratum"])))
      
      if (n_lists == 2) {
        out <- f_chapman(overlap[j, ], i, pars_in)
      } else if (n_lists %in% c(3, 4)) {
        if (j == 1) {
          x1 <- df
          x3 <- pars_in
        } else {
          x1 <- df[df[, overlap[j, "variable"]] == overlap[j, "stratum"], ]
          x3 <- pars_in
          x4 <- which(x3[, "loc_df"] == i)
          x5 <- trimws(unlist(strsplit(x3[x4, "confounders"], ",")))
          x6 <- gsub("_stratum", "", overlap[j, "variable"])
          if (x6 %in% x5) {x3[x4, "confounders"] <- paste(x5[x5 != x6], collapse = ", ")}
          
          # Handle exposure
          if (!is.na(x3[x4, "exposure"]) && x6 == x3[x4, "exposure"]) {
            x3[x4, "exposure"] <- NA
          }
        }
        
        # Implement log-linear models
        x2 <- f_logl(x1, i, x3)
        
        # Implement model averaging
        out <- f_model_average(x2, i, x3)
      }
      
      # Write results to file
      write_results_to_file(out, n_lists, filename)
    }
  }
}

print("Analysis completed.")
