#..........................................................................................
###      Gaza-CAPTURE-RECAPTURE ANALYSIS      ###
#..........................................................................................
# Adapted from codes written by Francesco Checchi, LSHTM 
# Original codes available on https://github.com/francescochecchi/mortality_capture_recapture_analysis

#..........................................................................................
### Preparatory steps
#..........................................................................................

#...................................      
## Install or load required R packages

# List of required packages
x1 <- c("BMA", "data.table", "ggpubr", "ggvenn", "gtools", "leaps", "lubridate", "MASS", "mclogit", "mlogitBMA",
        "RColorBrewer", "readxl", "Rfast", "scales", "tidyverse")

# Install any packages not yet installed
x2 <- x1 %in% row.names(installed.packages())
if (any(x2 == FALSE)) { install.packages(x1[! x2]) }

# Load all packages    
lapply(x1, library, character.only = TRUE)

#...................................      
## Starting setup

# Clean up from previous code / runs
rm(list=ls(all=TRUE))

# Set font for plots
windowsFonts(Arial=windowsFont("Arial"))

# Set working directory to the project root
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))

# Define paths
input <- file.path(getwd(), "input")
output <- file.path(getwd(), "output")

# Create output folder if it doesn't exist
if (!dir.exists(output)) {
  dir.create(output)
}

# Initialise random numbers
set.seed(123)

# Colour-blind palette for graphing
palette_cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#.........................................................................................
### Reading in required files
#.........................................................................................

#...................................      
## Identify data file in the input directory
file_path <- file.path(input, "2.gaza_list_capture_analysis.xlsx")

#...................................      
## Read in parameters
pars <- read_excel(file_path, sheet = "parameters")
pars <- as.data.frame(pars)

#...................................      
## Read in key informant datasets

# Create names for datasets of each location (will also be used for analysis output)
x1 <- gsub("[^[:alnum:] ]", "", pars$location)
x1 <- trimws(gsub("\\s+", " ", x1))
x1 <- tolower(gsub(" ", "_", x1))
pars[, "loc_df"] <- x1

# Read individual datasets for each location and assign objects - only read in datasets that should be analysed
for (i in 1:nrow(pars)) {
  if (pars[i, "analyse"] == "Y") {
    x1 <- read_excel(file_path, sheet = pars[i, "location"])
    x1 <- as.data.frame(x1)
    assign(pars[i, "loc_df"], x1)
  }
}

#.........................................................................................
### Calling bespoke functions
#.........................................................................................

source(file.path(getwd(), "code", "3.bespoke_functions.R"), echo = TRUE)

#.........................................................................................
### Preparing data for analysis
#.........................................................................................

#...................................      
## Recognise characteristics of each dataset

# Number of lists
pars[, "n_lists"] <- apply(pars[, c("list1", "list2", "list3", "list4")], 1, function(x) {length(which(!is.na(x)))})

# Parameters for datasets that should be analysed
pars_in <- subset(pars, analyse == "Y")

#...................................      
## Manage each dataset

for (i in pars_in[, "loc_df"]) { 
  # Clean dataset
  assign(i, f_clean(get(i), paste(i), pars, f_clean_month, f_clean_date))
}


#.........................................................................................
### Visualising data and preparing contingency tables for analysis
#.........................................................................................

#...................................      
## Describe patterns and overlap among lists
for (i in pars_in[, "loc_df"] ) { 
  
  # Describe patterns in period, age and gender, by list and overall
  f_describe(get(i), paste(i), pars, palette_cb)
  
  # Visualise and quantify overlap among lists
  assign(paste(i, "_overlap", sep = ""), f_overlap(get(i), i, pars, palette_cb) )
}


#.........................................................................................
### Analysing data
#.........................................................................................

for (i in pars_in[, "loc_df"] ) { 
  
  # Control message
  print(paste("now doing analysis for this site: ", pars_in[pars_in[, "loc_df"] == i, "location"], sep = ""))
  
  # Assemble required inputs
  # dataset
  df <- get(i) 
  
  # output of preparatory steps for this dataset
  overlap <- get(paste(i, "_overlap", sep = ""))
  
  # number of lists
  n_lists <- pars_in[pars_in[, "loc_df"] == i, "n_lists"]
  
  # Start output table
  filename <- file.path(output, paste0(i, "_analysis_output.csv"))
  write.table(paste("LOCATION: ", pars_in[pars_in[, "loc_df"] == i, "location"], sep = "" ), filename, 
              row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(paste("Period: ", min(subset(df, eligible == TRUE)$date_clean, na.rm = TRUE), " to ",
                    max(subset(df, eligible == TRUE)$date_clean, na.rm = TRUE), sep = ""), filename, append = TRUE, 
              row.names = FALSE, col.names = FALSE, sep = ",")
  write.table(rbind(rep("......................", 5)), filename, append = TRUE, 
              row.names = FALSE, col.names = FALSE, sep = ",")
  
  # For both all observations and each desired stratum...
  for (j in 1:nrow(overlap) ) {
    
    # control message
    print(paste("  working on this stratum: ", gsub("_", " ", overlap[j, "stratum"]), sep = ""))
    
    # continue table
    write.table(paste("Stratum: ", gsub("_", " ", overlap[j, "stratum"]), sep = ""), filename, append = TRUE, 
                row.names = FALSE, col.names = FALSE, sep = ",")
    
    # implement analysis depending on number of lists
    # if two lists only, simple Chapman estimator
    if (n_lists == 2) {out <- f_chapman(overlap[j, ], i, pars_in)}
    
    # if three or four lists, log-linear models and model averaging
    if (n_lists %in% c(3, 4) ) {
      # select dataset depending on which stratum is being worked on
      if (j == 1) {x1 <- df; x3 <- pars_in}
      if (j > 1) {
        x1 <- df[which(df[, overlap[j, "variable"]] == overlap[j, "stratum"]), ] 
        # modify parameters to make sure that the stratum isn't also a confounder...
        x3 <- pars_in
        x4 <- which(x3[, "loc_df"] == i)
        x5 <- trimws(unlist(strsplit(x3[x4, "confounders"], ",")))
        x6 <- gsub("_stratum", "", overlap[j, "variable"])
        if (x6 %in% x5) {x3[x4, "confounders"] <- paste(x5[x5 != x6], sep = ", ")}
        
        #... or an exposure
        x5 <- x3[x4, "exposure"]
        x6 <- overlap[j, "variable"]
        if (x6 %in% x5) {x3[x4, "exposure"] <- NA}
      }
      
      # implement log-linear models
      x2 <- f_logl(x1, i, x3)
      
      # implement model averaging
      out <- f_model_average(x2, i, x3)
    }
    
    # write results to table
    # 2-list scenario
    if (n_lists == 2) {
      write.table(out, filename, append = TRUE, row.names = FALSE, col.names = TRUE, sep = ",")
      write.table(rbind(rep("......................", 2)), filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
    }        
    
    # 3- or 4-list scenario
    if (n_lists %in% c(3, 4)) {
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
      write.table(out$out_pretty, filename, append = TRUE, row.names = FALSE, na = "", sep = ",")
      write.table("    Estimated deaths:", filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
      write.table(out$out_est_pretty, filename, append = TRUE, row.names = FALSE, col.names = TRUE, na = "", sep = ",")
      write.table("    List sensitivity:", filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
      write.table(out$out_sens_pretty, filename, append = TRUE, row.names = FALSE, na = "", sep = ",")
      write.table(rbind(rep("......................", 5)), filename, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
    }
  }
}


#.........................................................................................
### ENDS
#.........................................................................................
