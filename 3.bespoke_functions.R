#...............................................................................
### +++++ CAPTURE-RECAPTURE ANALYSIS OF MORTALITY DATA - GENERIC CODE ++++++ ###
#...............................................................................

#...............................................................................
## --------- BESPOKE FUNCTIONS TO IMPLEMENT DIFFERENT ANALYSIS TASKS  ------- ##
#...............................................................................



#...............................................................................
### Function for crude capture-recapture estimation based on a two-list system 
    # (m00 = x10 * x01 / x11)
    # Uses Chapman estimator given the expectation of low sample sizes:
    # https://en.wikipedia.org/wiki/Mark_and_recapture
#...............................................................................

f_chapman <- function(data_f, data_name_f, pars_f) {
  
  #...................................      
  ## Preparatory steps   
    # Confirm number of lists
    n_lists <- pars_f[pars_f[, "loc_df"] == data_name_f, "n_lists"]
    
    # stop if fewer than three lists or more than four...
    if (n_lists != 2  | is.na(n_lists)) 
    {stop("wrong number of lists: only 2 lists allowed for two-list analysis")}
    
    # List names
    list_names <- pars_f[pars_f[, "loc_df"] == data_name_f, 
      paste0("list", 1:n_lists)]
    
    # Prepare contingency table
    df <- unlist(data_f[! names(data_f) %in% c("variable", "stratum")])
    names(df) <- names(data_f)[! names(data_f) %in% c("variable", "stratum")]
    x1 <- df["x10"] + df["x11"]
    x2 <- df["x01"] + df["x11"]
    x12 <- df["x11"]
  
  #...................................      
  ## Point estimate and confidence interval of total deaths, 
      # unlisted deaths and list sensitivity
  
    # Estimate of total deaths...
    total <- trunc(((x1 + 1) * (x2 + 1) / (x12 + 1)) - 1, 0)
    
    # 95% confidence interval of total deaths
    sigma_0.5 <- sqrt(1 / (x12 + 0.5) + 1 / (x2 - x12 + 0.5) + 1 / 
      (x1 - x12 + 0.5) + (x12 + 0.5) / ( (x1 - x12 + 0.5) * (x2 - x12 + 0.5) ) )
    total_lci <- x2 + x1 - x12 - 0.5 + ( (x2 - x12 + 0.5) * (x1 - x12 + 0.5) / 
      (x12 + 0.5) ) * exp(-1.96 * sigma_0.5)
    total_lci <- round(total_lci, 0)
    total_uci <- x2 + x1 - x12 - 0.5 + ( (x2 - x12 + 0.5) * (x1 - x12 + 0.5) / 
      (x12 + 0.5) ) * exp(1.96 * sigma_0.5)
    total_uci <- round(total_uci, 0)
    
    # Output
    out <- c(
      paste0(total - sum(df), " (95%CI ", total_lci - sum(df), " to ", 
        total_uci - sum(df), ")"),
      paste0(total, " (95%CI ", total_lci, " to ", total_uci, ")"),
      paste0(round(100 * x1 / total, 1), "% (95%CI ", 
        round(100 * x1 / total_uci, 1), "% to ", 
        round(100 * x1 / total_lci, 1), "%)"),
      paste0(round(100 * x2 / total, 1), "% (95%CI ", 
        round(100 * x2 / total_uci, 1), "% to ", 
        round(100 * x2 / total_lci, 1), "%)", sep = ""),
      paste0(round(100 * sum(df) / total, 1), "% (95%CI ", 
        round(100 * sum(df) / total_uci, 1), "% to ", 
        round(100 * sum(df) / total_lci, 1), "%)")
    )
    
    out <- cbind(c(
      "deaths outside any list", "total deaths", 
      paste0("sensitivity of ", list_names[1], "'s list"),
      paste0("sensitivity of ", list_names[2], "'s list"),
      "sensitivity of both lists combined"
    ), out )
    colnames(out) <- c("statistic", "estimate")
    
    return(out)
}


#...............................................................................
### Function to clean dataset for analysis
#...............................................................................

f_clean <- function(data_f, data_name_f, pars_f, f_clean_month, f_clean_date) {
  
  # Name of the dataset
  x1 <- data_name_f
  
  #...................................      
  ## Clean list variables
  
  # Figure out number of lists
  n_lists <- pars_f[pars_f[, "loc_df"] == x1, "n_lists"]
  
  # Stop if fewer than two lists or more than four...
  if (n_lists < 2 | n_lists > 4 | is.na(n_lists)) 
  {stop(paste("too few or too many lists: only 2, 3 or 4 lists allowed", 
    data_name_f, sep = " - "))}
  
  # Make sure each list variable contains only 1, 0 or NA
  for (i in 1:n_lists) {
    x2 <- suppressWarnings(as.integer(data_f[, paste0("list", i)]))
    x2[! x2 %in% c(0, 1)] <- NA
    data_f[, paste0("list", i)] <- x2
  }
  
  #...................................      
  ## Clean age variable
  
  data_f[, "age"] <- suppressWarnings(as.integer(data_f[, "age"]))
  # warnings for implausible ages
  if (min(data_f[, "age"], na.rm = TRUE) < 0) 
  {warning(paste("at least one of the age values is negative", data_name_f, sep = " - "))}
  if (max(data_f[, "age"], na.rm = TRUE) > 100) 
  {warning(paste("at least one of the age values is > 100", data_name_f, sep = " - "))}
  
  #...................................      
  ## Clean gender variable
  
  data_f[, "gender"] <- tolower(as.character(data_f[, "gender"]))
  # create clean output vector
  x2 <- rep(NA, length(data_f[, "gender"]))
  
  # detect male values
  male <- which(data_f[, "gender"] %in% c("male", "m") )
  x2[male] <- "male"
  
  # detect female values
  female <- which(data_f[, "gender"] %in% c("female", "fem", "f") )
  x2[female] <- "female"
  
  #...................................      
  ## Clean and generate dates for each death
  
  # Create new clean variable
  data_f[, "date_clean"] <- NA
  
  # Identify date / time variables
  x2 <- colnames(data_f)[colnames(data_f) %in% c("date_death", "month_death", "year_death")]
  
  # If there are no date / time variables...
  if (length(x2) == 0) {warning(paste("no month, year or date of death variables found: check dataset", data_name_f, sep = " - "))}
  
  # If date is one of the variables, clean it - all wrong formats should go to NA
  if ("date_death" %in% x2) {
    data_f[, "date_death"] <- lubridate::as_date(sapply(data_f[, "date_death"], f_clean_date))
    # warnings for implausible dates
    if (min(data_f[, "date_death"], na.rm = TRUE) < (lubridate::as_date(Sys.Date()) - 3650) ) 
    {warning(paste("at least one of the dates is more than 10 years ago", data_name_f, sep = " - "))}
    if (max(data_f[, "date_death"], na.rm = TRUE) > (lubridate::as_date(Sys.Date())) ) 
    {warning(paste("at least one of the dates is in the future!", data_name_f, sep = " - "))}
  }
  
  # If month is one of the variables, clean it to only feature integers (1-12)
  if ("month_death" %in% x2) {
    data_f[, "month_death"] <- f_clean_month(data_f[, "month_death"])
  }
  
  # If year is one of the variables, clean it to only feature integers; check for unusual values
  if ("year_death" %in% x2) {
    data_f[, "year_death"] <- suppressWarnings(as.integer(data_f[, "year_death"]) )
    # warnings for implausible years
    if (min(data_f[, "year_death"], na.rm = TRUE) < year(lubridate::as_date(Sys.Date()) - 3650) ) 
    {warning(paste("at least one of the year values is more than 10 years ago", data_name_f, sep = " - "))}
    if (max(data_f[, "year_death"], na.rm = TRUE) > year(lubridate::as_date(Sys.Date())) ) 
    {warning(paste("at least one of the year values is in the future!", data_name_f, sep = " - "))}
  }
  
  # If year but not month is among the variables, create date from year (assume 15 June)
  if ((! "month_death" %in% x2) & ("year_death" %in% x2)) {
    data_f[, "date_clean"] <- lubridate::dmy(paste(15, 6, data_f[, "year_death"], sep = "-") )
  }
  
  # If month and year are among the variables, create date from these two (assume 15th day of the month)
  # if month value is missing, create date from year only, as above
  if ("month_death" %in% x2 & "year_death" %in% x2) {
    data_f[, "date_clean"] <- ifelse(! is.na(data_f[, "month_death"]), 
                                     lubridate::dmy(paste(15, data_f[, "month_death"], data_f[, "year_death"], sep = "-") ), 
                                     lubridate::dmy(paste(15, 6, data_f[, "year_death"], sep = "-") )
    )
  }
  
  # If date is among the variables, override date created from month and year
  if ("date_death" %in% x2) {
    data_f[, "date_clean"] <- ifelse(! is.na(data_f[, "date_death"]), data_f[, "date_death"], data_f[, "date_clean"])
  }
  
  # Make sure date is in the right format
  data_f[, "date_clean"] <- lubridate::as_date(data_f[, "date_clean"])
  # warnings for implausible dates
  if (min(data_f[, "date_clean"], na.rm = TRUE) < (lubridate::as_date(Sys.Date()) - 3650) ) 
  {warning(paste("at least one of the dates is more than 10 years ago", data_name_f, sep = " - "))}
  if (max(data_f[, "date_clean"], na.rm = TRUE) > (lubridate::as_date(Sys.Date())) ) 
  {warning(paste("at least one of the dates is in the future!", data_name_f, sep = " - "))}
  
  #...................................      
  ## Generate eligibility variable for each death (eligible if all lists have a 1/0 value)
  
  data_f[, "eligible"] <- rowSums(is.na(data_f[, paste("list", c(1:n_lists), sep = "")]))
  data_f[, "eligible"] <- ifelse(data_f[, "eligible"] > 0, FALSE, TRUE)
  
  #...................................      
  ## Restrict dataset as desired
  
  # Restrict gender if desired
  if (!is.na(pars_f[pars_f[, "loc_df"] == x1, "gender_restrict"]) ) {
    data_f <- subset(data_f, gender == pars_f[pars_f[, "loc_df"] == x1, "gender_restrict"] )
  }  
  
  # Restrict age if desired
  # minimum age    
  if (!is.na(pars_f[pars_f[, "loc_df"] == x1, "age_min"]) ) {
    data_f <- subset(data_f, age >= as.integer(pars_f[pars_f[, "loc_df"] == x1, "age_min"]) )
  }  
  
  # maximum age    
  if (!is.na(pars_f[pars_f[, "loc_df"] == x1, "age_max"]) ) {
    data_f <- subset(data_f, age <= as.integer(pars_f[pars_f[, "loc_df"] == x1, "age_max"]) )
  }  
  
  # Restrict analysis period if desired
  # starting date
  if (!is.na(pars_f[pars_f[, "loc_df"] == x1, "date_start"]) ) {
    x2 <- f_clean_date(pars_f[pars_f[, "loc_df"] == x1, "date_start"])
    if (is.na(x2)) stop(paste("date_start has been entered in the parameters but its date format is not recognisable", data_name_f, sep = " - "))
    data_f <- subset(data_f, date_clean >= x2 )
  }
  
  # ending date
  if (!is.na(pars_f[pars_f[, "loc_df"] == x1, "date_end"]) ) {
    x2 <- f_clean_date(pars_f[pars_f[, "loc_df"] == x1, "date_end"])
    if (is.na(x2)) stop(paste("date_end has been entered in the parameters but its date format is not recognisable", data_name_f, sep = " - "))
    data_f <- subset(data_f, date_clean <= x2 )
  }
  
  #...................................      
  ## Return prepared dataset
  return(data_f)
  
}


#...............................................................................
### Function to clean a single date observation
# will try to read the date as a date; if it can't, parse date as a character; 
# note: can only cope with day-month-year order, whatever the format
#...............................................................................

f_clean_date <- function(x) {
  # Output
  out <- NA
  
  # First try to read the date as a date
  out <- suppressWarnings(as_date(x) )
  
  # If the date can't be read as a date but is non-NA...
  if (is.na(out) & ! is.na(x)) {
    
    # convert to lower-case character, remove double spaces
    x <- trimws(tolower(as.character(x)))
    
    # try to detect all-digit format without separators
    if (! is.na(suppressWarnings(as.integer(x))) ) { 
      if (! nchar(x) %in% c(6, 8) ) {stop("at least one date has an ambiguous format: check dataset")}
      if (nchar(x) == 6) { 
        x1 <- paste(substr(x, 1, 2), substr(x, 3, 4), substr(x, 5, 6), sep = "-")
        out <- readr::parse_date(x1, "%d%.%m%.%Y")
      }
      if (nchar(x) == 8) { 
        x1 <- paste(substr(x, 1, 2), substr(x, 3, 4), substr(x, 5, 8), sep = "-")
        out <- readr::parse_date(x1, "%d%.%m%.%Y")
      }
    }
    
    # try to detect all-digit format with separators (" ", "/", "-")
    x1 <- trimws(gsub("[^[:alpha:] ]", "", x) )
    for (i in c(" ", "/")) {x <- gsub(i, "-", x)} 
    if (nchar(x1) == 0 & grepl("-", x) ) { out <- readr::parse_date(x, "%d%.%m%.%Y") }
    
    # try to detect format with month as character, with or without separators
    if (nchar(x1) > 0) {
      x2 <- match(substr(x1, 1, 3), tolower(month.abb))
      if(! is.na(x2) ) {
        x3 <- gsub("[^[:alnum:] ]", "", x)
        x3 <- gsub('[a-zA-Z]', "", x3)
        if (! nchar(x3) %in% c(3:6)) {stop("at least one date has an ambigious format: check dataset")}
        if (nchar(x3) == 3) {x4 <- as.integer(substr(x3, 1, 1)); x5 <- as.integer(substr(x3, 2, 3))}
        if (nchar(x3) == 4) {x4 <- as.integer(substr(x3, 1, 2)); x5 <- as.integer(substr(x3, 3, 4))}
        if (nchar(x3) == 5) {x4 <- as.integer(substr(x3, 1, 1)); x5 <- as.integer(substr(x3, 2, 5))}
        if (nchar(x3) == 6) {x4 <- as.integer(substr(x3, 1, 2)); x5 <- as.integer(substr(x3, 3, 6))}
        out <- readr::parse_date(paste(x4, x2, x5, sep = "-"), "%d%.%m%.%Y")
      }
    }
  }
  
  # Return date
  out <- as_date(out)
  return(out)
}  


#...............................................................................
### Function to clean months, given a vector entry
#...............................................................................

f_clean_month <- function(x) {
  
  # Check input is eligible
  if(! is.vector(x)) {stop("input is not a vector")}
  
  # Create output corrected vector
  out <- rep(NA, length(x))
  
  # Deal with character vectors
  if (is.character(x)) {
    # go lower-case
    x1 <- tolower(x)
    x2 <- tolower(month.abb)
    
    # elements that are full or abbreviated month names (e.g. "jan", "MAR", "November", "april"..)  
    mmm <- grep(paste(x2, collapse = "|"), x1 )
    out[mmm] <- match(substr(x1[mmm], 1, 3), x2)
    
    # elements that can be coerced to integers
    int <- suppressWarnings(which(! is.na(as.integer(x1))) )
    out[int] <- as.integer(x1[int])
  }
  
  # Deal with numeric vectors
  if (is.numeric(x)) {
    # elements that can be coerced to integers
    int <- suppressWarnings(which(! is.na(as.integer(x))) )
    out[int] <- as.integer(x[int])
  }
  
  # Return cleaned vector as 1:12 integers or NA values
  out[!out %in% c(1:12)] <- NA
  return(out)
}


#...............................................................................
### Function to describe patterns in the (clean) data and test for statistical 
    # differences
#...............................................................................

f_describe <- function(data_f, data_name_f, pars_f, f_palette) {
  
  #...................................      
  ## Preparatory steps 
  # Name of the dataset
  x1 <- data_name_f
  
  # Figure out number of lists
  n_lists <- pars_f[pars_f[, "loc_df"] == x1, "n_lists"]
  
  # Remove ineligible observations
  if ("eligible" %in% colnames(data_f)) {data_f <- subset(data_f, eligible == TRUE)}
  
  # Transform dataset so that each list is appended, and whole dataset (1 line = 1 decedent) right below it
  # first append deaths that appear on each list (meaning some deaths will be repeated)
  long <- c()
  for (i in 1:n_lists) {
    x2 <- data_f[data_f[, paste("list", i, sep = "")] == 1, ]
    x2 <- x2[, -grep("list", colnames(x2))]
    x2[, "which"] <- pars_f[pars_f[, "loc_df"] == x1, paste("list", i, sep = "")]
    long <- rbind(long, x2)
  }
  colnames(long)[colnames(long) == "which"] <- "list"
  
  # then append deaths that appear on at least one list
  x2 <- data_f[, -grep("list", colnames(data_f))]
  x2[, "list"] <- "any list"
  long <- rbind(long, x2)
  
  # Output
  # names of lists, sorted
  x2 <- sort(unique(long$list)[unique(long$list) != "any list"] )
  
  # create table
  tab <- as.data.frame(matrix(NA, nrow = 0, ncol = (4 + n_lists)))
  colnames(tab) <- c("characteristic", "category", "any list", x2, "p_value" )      
  
  #...................................      
  ## Distribution of deaths by list and year (or month if overall time-span is less than two years)   
  
  # Which time unit to use for table
  x3 <- "year"
  if ((max(long[, "date_clean"], na.rm = TRUE) - min(long[, "date_clean"], na.rm = TRUE) ) < 730) {x3 <- "month"}
  
  # If year...
  if (x3 == "year" & "date_clean" %in% colnames(long)) {
    # create unique year variable and sort
    long[, "year"] <- year(long[, "date_clean"])
    long <- long[order(long[, "date_clean"]), ]
    
    # tabulate
    tab_part <- table(long[, "year"], long[, "list"])
    tab_part <- tab_part[, c("any list",  x2)]
    tab_part <- as.data.frame.matrix(tab_part) 
    
    # create additional columns (characteristic, categories, Fisher test p-value if numbers allow)
    tab_part <- cbind(rep("year", length(unique(long[, "year"]))), row.names(tab_part), tab_part, 
                      c(fisher.test(tab_part[, x2], simulate.p.value = TRUE)$p.value, rep(NA, nrow(tab_part) - 1)))
    
    # create totals row
    tab_part <- rbind(tab_part, c("year", "totals", colSums(tab_part[, c("any list", x2)]), NA))
    
    # add to main table
    colnames(tab_part) <- c("characteristic", "category", "any list", x2, "p_value" )
    tab <- rbind(tab, tab_part)
    tab <- rbind(tab, rep(NA, ncol(tab)))
    
  }
  
  # If month...
  if (x3 == "month" & "date_clean" %in% colnames(long)) {
    # create unique date variable on which to sort
    long[, "date"] <- dmy(paste(15, month(long[, "date_clean"]), year(long[, "date_clean"]), sep = "/") )
    long <- long[order(long[, "date"]), ]
    
    # tabulate
    tab_part <- table(long[, "date"], long[, "list"])
    tab_part <- tab_part[, c("any list",  x2)]
    tab_part <- as.data.frame.matrix(tab_part) 
    
    # create additional columns (characteristic, categories, p-value)
    x4 <- paste(month.abb[month(row.names(tab_part))], year(row.names(tab_part)), sep = " ")
    tab_part <- cbind(rep("month", length(x4)), x4, tab_part, 
                      c(fisher.test(tab_part[, x2], simulate.p.value = TRUE)$p.value, rep(NA, nrow(tab_part) - 1)))
    # warning on p-value
    if (length(x4) > 5) warning("best not to present p-value: table has too many categories")
    
    # create totals row
    tab_part <- rbind(tab_part, c("month", "totals", colSums(tab_part[, c("any list", x2)]), NA))
    
    # add to main table
    colnames(tab_part) <- c("characteristic", "category", "any list", x2, "p_value" )
    tab <- rbind(tab, tab_part)
    tab <- rbind(tab, rep(NA, ncol(tab)))
    
  }
  
  #...................................      
  ## Distribution of deaths by list and age categories
  
  if ("age" %in% colnames(long)) {
    # Create age categories, which must include any chosen age cut-offs
    # identify cutoffs
    x3 <- pars_f[pars_f[, "loc_df"] == x1, "age_cutoffs"]
    x3 <- as.character(x3)
    x3 <- as.integer(unlist(strsplit(x3, ",")))
    
    # add other cutoffs that should always be there (1y, 5y, 15y, 45y)
    x3 <- sort(unique(c(x3, c(1, 5, 15, 45)) ))
    
    # create labels
    x3_lab <- c()
    for (i in 1:length(x3)) {
      if (i == 1) x3_lab <- paste("age_0_to_", x3[i] - 1, sep = "")
      if (i > 1) x3_lab <- c(x3_lab, paste("age_", x3[i-1], "_to_", x3[i] - 1, sep = "") )
    }
    x3_lab <- c(x3_lab, paste("age_", x3[length(x3)], "_plus", sep = "") )
    
    # create categories
    long[, "age_cat"] <- cut(long[, "age"], breaks = c(0, x3, 1000), include.lowest = TRUE, right = FALSE,
                             labels = x3_lab)
    
    # Mean age by list, and p-value; add to main table
    tab <- rbind(tab, c("age (years)", "means", 
                        (aggregate(long[, "age"],  by = list(long[, "list"]), FUN = mean, na.rm = TRUE))$x,
                        oneway.test(long[, "age"] ~ long[, "list"])$p.value)
    )
    
    # Tabulate
    tab_part <- table(long[, "age_cat"], long[, "list"])
    tab_part <- tab_part[, c("any list",  x2)]
    tab_part <- as.data.frame.matrix(tab_part) 
    
    # Create additional columns (characteristic, categories)
    tab_part <- cbind(rep("age (years)", nrow(tab_part)), row.names(tab_part), tab_part, rep(NA, nrow(tab_part) ))
    
    # Create totals row
    tab_part <- rbind(tab_part, c("age (years)", "totals", colSums(tab_part[, c("any list", x2)]), NA))
    
    # Add to main table
    colnames(tab_part) <- c("characteristic", "category", "any list", x2, "p_value" )
    tab <- rbind(tab, tab_part)
    tab <- rbind(tab, rep(NA, ncol(tab)))
    
  }
  
  #...................................      
  ## Distribution of deaths by list and gender     
  
  if ("gender" %in% colnames(long)) {
    
    # Tabulate
    tab_part <- table(long[, "gender"], long[, "list"])
    tab_part <- tab_part[, c("any list",  x2)]
    tab_part <- as.data.frame.matrix(tab_part) 
    
    # Create additional columns (characteristic, categories, p-value)
    tab_part <- cbind(rep("gender", nrow(tab_part)), row.names(tab_part), tab_part, 
                      c(fisher.test(tab_part[, x2], simulate.p.value = TRUE)$p.value, rep(NA, nrow(tab_part) - 1)))
    
    # Create totals row
    tab_part <- rbind(tab_part, c("gender", "totals", colSums(tab_part[, c("any list", x2)]), NA))
    
    # Add to main table
    colnames(tab_part) <- c("characteristic", "category", "any list", x2, "p_value" )
    tab <- rbind(tab, tab_part)
    
  }    
  
  #...................................      
  ## Write output table   
  # Reformat labels
  tab[, "category"] <- gsub("_", " ", tab[, "category"])
  tab[, "category"] <- gsub(" plus", "+", tab[, "category"])
  tab[, "category"] <- gsub("0 to 0", "0", tab[, "category"])
  tab[, "category"] <- gsub("age ", "", tab[, "category"])  
  
  # Write
  write.csv(tab, paste(x1, "_table_descriptive.csv", sep = ""), row.names = FALSE, na = "")
  
  #...................................      
  ## Create and write plots 
  # Choose colours
  x3 <- f_palette[c(2,6,8,14)]
  x5 <- length(unique(long[, "list"]))
  colours <- c("grey20", x3[1:x5])
  
  # Year or month
  if ("year" %in% tab[, "characteristic"] | "month" %in% tab[, "characteristic"]) {
    x4 <- subset(tab, characteristic %in% c("year", "month") & category != "totals")
    xlab <- unique(x4[, "characteristic"])
    x4 <- x4[, c("category", "any list", x2)]
    x4 <- reshape2::melt(x4, id = "category", value.name = "n_cat")
    x4[, "n_cat"] <- as.integer(x4[, "n_cat"])
    
    # Reorder the levels of "category" chronologically
      x4$category <- factor(x4$category, levels = unique(x4$category))

    plot_ym <- ggplot(x4, aes(y = n_cat, x = category, group = variable, colour = variable, fill = variable)) +
      geom_bar(position = "dodge", stat = "identity", alpha = 0.4) +
      scale_y_continuous("number") +
      scale_x_discrete(xlab) +
      labs(fill = "Source:") +
      guides(colour = "none" ) +
      theme_bw() +
      scale_color_manual(values = colours ) +
      scale_fill_manual(values = colours ) +
      theme(axis.title = element_text(size = 10, colour = "grey20"), 
            legend.title = element_text(size = 10, colour = "grey20"),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot_ym)
    ggsave(paste(x1, "_bar_descr_", "year_or_month.png", sep = ""), plot_ym, 
           width = 15, height = 8, units = "cm", dpi = "print")
  }
  
  # Age
  if ("age (years)" %in% tab[, "characteristic"]) {
    x4 <- subset(tab, characteristic == "age (years)" & category != "totals")
    xlab <- unique(x4[, "characteristic"])
    x4 <- x4[, c("category", "any list", x2)]
    x4 <- reshape2::melt(x4, id = "category", value.name = "n_cat")
    x4[, "n_cat"] <- as.integer(x4[, "n_cat"])
    x4 <- subset(x4, category != "means")
    
    # if plotting age, need to order the categories
    x4[, "category"] <- factor(x4[, "category"])
    x5 <- substr(levels(x4[, "category"]), 1, 2)
    x5 <- data.frame("levels" = levels(x4[, "category"]), "integers" = as.integer(trimws(x5)) )
    x5 <- x5[order(x5[, "integers"]), ]
    x4[, "category"] <- factor(x4[, "category"], x5[, "levels"])
    
    plot_age <- ggplot(x4, aes(y = n_cat, x = category, group = variable, colour = variable, fill = variable)) +
      geom_bar(position = "dodge", stat = "identity", alpha = 0.4) +
      scale_y_continuous("number") +
      scale_x_discrete(xlab) +
      labs(fill = "Source:") +
      guides(colour = "none" ) +
      theme_bw() +
      scale_color_manual(values = colours ) +
      scale_fill_manual(values = colours ) +
      theme(axis.title = element_text(size = 10, colour = "grey20"), 
            legend.title = element_text(size = 10, colour = "grey20"))
    
    print(plot_age)
    ggsave(paste(x1, "_bar_descr_", "age.png", sep = ""), plot_age, 
           width = 15, height = 8, units = "cm", dpi = "print")
  }    
  
  # Gender
  if ("gender" %in% tab[, "characteristic"]) {
    x4 <- subset(tab, characteristic == "gender" & category != "totals")
    xlab <- unique(x4[, "characteristic"])
    x4 <- x4[, c("category", "any list", x2)]
    x4 <- reshape2::melt(x4, id = "category", value.name = "n_cat")
    x4[, "n_cat"] <- as.integer(x4[, "n_cat"])
    x4 <- transform(x4, prop = ave(n_cat, variable, FUN = prop.table))
    # x4 <- transform(x4, pos = ave(prop, variable, FUN = function(x) {cumsum(x) - x/2}) )
    x4[, "label"] <- paste(x4[, "category"], ": ", x4[, "n_cat"], " (", percent(x4[, "prop"], accuracy = 0.1), ")", sep = "")
    x4[, "category"] <- factor(x4[, "category"], levels = c("male", "female") )
    
    # bar chart version
    plot_gender1 <- ggplot(x4, aes(x = variable, y = prop, colour = variable, fill = variable, alpha = category, 
                                   label = label)) +
      geom_bar(stat="identity", position = "stack") +
      theme_bw() +
      scale_y_continuous("percentage", labels = scales::percent) +
      scale_x_discrete("source", breaks = NULL) +
      geom_text(position = position_stack(vjust = 0.5), size = 3, colour = "grey20", alpha = 1) +
      # geom_text(aes(x = variable, y = pos, label = label), position = "stack", size = 3, colour = "grey20", alpha = 1) +
      scale_fill_manual(values = colours ) +
      scale_colour_manual(values = colours ) +
      scale_alpha_manual(values = c(0.2, 0.6)) +
      guides(colour = "none") +
      guides(fill = "none") +
      guides(alpha = "none") +
      theme(axis.title = element_text(size = 10, colour = "grey20") )
    
    print(plot_gender1)
    ggsave(paste(x1, "_bar_descr_", "gender.png", sep = ""), plot_gender1, 
           width = 15, height = 8, units = "cm", dpi = "print")      
    
    # pie chart version
    x4[, "label2"] <- gsub("female", "F", x4[, "label"])
    x4[, "label2"] <- gsub("male", "M", x4[, "label2"])
    plot_gender2 <- ggplot(x4, aes(x = "", y = n_cat, colour = variable, fill = variable, alpha = category)) +
      geom_bar(stat="identity", width = 1, position = "fill") +
      theme_bw() +
      facet_wrap(variable~.) +
      geom_text(aes(label = label2), position = position_fill(vjust = 0.2), size = 3, colour = "grey20", alpha = 1) +
      scale_fill_manual(values = colours ) +
      scale_colour_manual(values = colours ) +
      scale_alpha_manual(values = c(0.2, 0.5)) +
      coord_polar(theta = "y") +
      theme(axis.title = element_blank() ) +
      theme(axis.text = element_blank(), panel.grid = element_blank(), axis.ticks = element_blank() ) +
      guides(colour = "none") +
      guides(fill = "none") +
      guides(alpha = "none") +
      theme(strip.background = element_rect(fill = "white"))
    
    print(plot_gender2)
    ggsave(paste(x1, "_pie_descr_", "gender.png", sep = ""), plot_gender2, 
           width = 15, height = 8, units = "cm", dpi = "print")
  }
  
  # Combined graphs
  # list of plots that do exist
  plot_list1 <- list(if(exists("plot_ym")) {plot_ym}, if(exists("plot_age")) {plot_age}, if(exists("plot_gender1")) {plot_gender1} )
  plot_list2 <- list(if(exists("plot_ym")) {plot_ym}, if(exists("plot_age")) {plot_age}, if(exists("plot_gender2")) {plot_gender2 + facet_wrap(variable~., nrow = 1)} )
  
  # version with bar chart for gender
  plot_comb1 <- ggpubr::ggarrange(plotlist = plot_list1, ncol = 1)  
  print(plot_comb1)
  ggsave(paste(x1, "_plots_descr_", "comb1_long.png", sep = ""), plot_comb1, 
         width = 15, height = 20, units = "cm", dpi = "print")
  ggsave(paste(x1, "_plots_descr_", "comb1_wide.png", sep = ""), plot_comb1, 
         width = 20, height = 13, units = "cm", dpi = "print")
  
  # version with pie chart for gender  
  plot_comb2 <- ggpubr::ggarrange(plotlist = plot_list2, ncol = 1)  
  print(plot_comb2)
  ggsave(paste(x1, "_plots_descr_", "comb2_long.png", sep = ""), plot_comb2, 
         width = 15, height = 20, units = "cm", dpi = "print")    
  ggsave(paste(x1, "_plots_descr_", "comb2_wide.png", sep = ""), plot_comb2, 
         width = 20, height = 13, units = "cm", dpi = "print")  
}  



#...............................................................................
### Function to fit each candidate log-linear model for a 3- or 4-list system
    # as per Rossi et al. https://rivista-statistica.unibo.it/article/view/9854 
#...............................................................................

f_logl <- function(data_f, data_name_f, pars_f) {
  
  #...................................      
  ## Preparatory steps
  
  # Confirm number of lists
  n_lists <- pars_f[pars_f[, "loc_df"] == data_name_f, "n_lists"]
  # stop if fewer than three lists or more than four...
  if (! n_lists %in% c(3, 4)  | is.na(n_lists)) 
  {stop("wrong number of lists: only 3 or 4 lists allowed for a three- or four-list analysis")}
  
  # list names
  list_names <- pars_f[pars_f[, "loc_df"] == data_name_f, paste("list", 1:n_lists, sep = "")]
  
  # Identify exposure and confounders    
  exposure <- tolower(trimws(pars_f[pars_f[, "loc_df"] == data_name_f, "exposure"]))
  confounders <- pars_f[pars_f[, "loc_df"] == data_name_f, "confounders"]
  if (! is.na(confounders[1])) {confounders <- tolower(sapply(unlist(strsplit(confounders, ",")), trimws)) }
  
  # Prepare dataset
  # restrict data to eligible observations
  if ("eligible" %in% colnames(data_f)) {data_f <- subset(data_f, eligible == TRUE)}
  
  # create unique id for each observation (ignore any existing id variable)
  data_f[, "key"] <- paste("id", 1:nrow(data_f), sep = "")
  
  # create indicator variable for outcome (all = 1)
  data_f[, "y"] <- 1
  
  # rename list columns
  for (i in 1:n_lists) {colnames(data_f)[colnames(data_f) == paste("list", i, sep = "")] <- paste("x", i, sep = "")}
  
  # defactor exposure and confounders
  if (! is.na(exposure) ) {
    if (is.factor(data_f[, exposure]) ) {data_f[, exposure] <- as.character(data_f[, exposure]) }
  }  
  if (! is.na(confounders[1]) ) {
    for (i in confounders) {
      if (is.factor(data_f[, i]) ) {data_f[, i] <- as.character(data_f[, i]) }
    }
  }
  
  #...................................      
  ## Prepare data for modelling (note: for individual-level analysis, i.e. one row = one observation)
  
  # Prepare possible 'profiles' for each individual observation
  # identify all possible list profiles (cells in contingency table)
  profiles <- as.data.frame(permutations(n = 2, r = n_lists, v = c(0, 1), repeats.allowed = TRUE))
  colnames(profiles) <- as.character(1:n_lists)
  
  # columns for which observations appear on which combinations of lists
  for (i in 3:n_lists) {
    x1 <- combinations(n = n_lists, r = i - 1, v = 1:n_lists, repeats.allowed = FALSE)
    for (j in 1:nrow(x1)) { profiles[, paste(x1[j, ], collapse = "")] <- rowSums(profiles[, x1[j, ]]) }
    profiles[, nchar(colnames(profiles)) == (i - 1)] <- ifelse(profiles[, nchar(colnames(profiles)) == (i - 1)] == (i - 1), 1, 0)
  }
  colnames(profiles) <- paste("x", colnames(profiles), sep = "")
  x2 <- colnames(profiles)
  
  # Set profiles for each observation, based on the data
  # create expanded dataframe with all possible profiles for each observation
  df <- expand_grid(data_f[, "key"], profiles)
  colnames(df) <- c("key", colnames(profiles))
  df <- as.data.frame(df)
  
  # match each observation to possible profiles (except for which lists the observation is in)
  x1 <- paste("x", 1:n_lists, sep = "")
  df <- merge(df, data_f[, ! colnames(data_f) %in% c(x1, "y")], by = "key", all.x = TRUE)
  
  # determine which profile the observation has, based on which lists the observation is in
  # outcome indicator = 1 if observation falls within a given profile, 0 otherwise and NA for x000(0) profile)
  df <- merge(df, data_f[, c("key", x1, "y")], by = c("key", x1), all.x = TRUE)
  df[, "y"] <- ifelse(is.na(df[, "y"]), 0, df[, "y"])
  df[rowSums(df[, x1]) == 0, "y"] <- NA
  
  # Add columns for interactions between exposure and confounder (if present) and lists
  # (note: omit interactions among confounders and between exposure and confounders)
  # profiles:exposure interactions
  if (! is.na(exposure)) { 
    df[, paste(x2, exposure, sep = ":")] <- ifelse(df[, x2] == 1, df[, exposure], 0)
  }
  
  # profiles:confounder(s) interactions
  if (! is.na(confounders[1])) {
    for (i in confounders) {df[, paste(x2, i, sep = ":")] <- ifelse(df[, x2] == 1, df[, i], 0)} 
  }
  
  
  #...................................      
  ## Define candidate models
  # Possible terms...
  x1 <- c()
  for (i in 3:n_lists) {
    x1 <- c(x1, apply( combinations(n_lists, i - 1, unlist(list_names) ) , 1 , paste , collapse = " x " ) )
  }
  # ...and their length
  x2 <- lapply(sapply(x1, strsplit, " x "), length)
  
  # Combinations of two-list terms
  x3 <- names(x2[x2 == 2])
  x4 <- c()
  for (i in n_lists:1 ) {
    x4 <- c(x4, apply(combinations(length(x3), i, x3) , 1 , paste , collapse = ", " ) )
  }
  # if 3 lists, stop here and attribute to model
  if (n_lists == 3) {out <- data.frame("model" = c("no interactions", x4) )}
  
  # If 4 lists, continue:
  if (n_lists == 4) {
    # combinations of three-list terms
    x3 <- names(x2[x2 == 3])
    x5 <- c()
    if (length(x3) > 0) {
      for (i in n_lists:1 ) {
        x5 <- c(x5, apply(combinations(length(x3), i, x3) , 1 , paste , collapse = ", " ) )
      }
    }
    
    # matrix to indicate overlap of two- and three-list term combinations (overlap = violation of hierarchy principle)
    x7 <- rep(NA, length(x5))
    for (i in 1:length(x5) ) {
      x8 <- unlist(strsplit(x5[i], ", "))
      x9 <- c()
      for (j in 1:length(x8)) {
        x10 <- unlist(strsplit(x8[j], " x ") )
        x9 <- c(x9, apply(permutations(length(x10), 3, x10), 1, paste, collapse = " x ") )
      }
      x7[i] <- paste(x9, collapse = ", ")
    }
    
    x6 <- as.data.frame(matrix(sapply(gsub(", ", "|", x4), grepl, x7), nrow = length(x5), ncol = length(x4)))
    colnames(x6) <- x4
    rownames(x6) <- x5
    
    # select unique non-overlapping combinations
    x8 <- c()
    for (i in 1:nrow(x6)) {
      for (j in 1:ncol(x6)) {
        if (x6[i, j] == TRUE) {x8 <- c(x8, rownames(x6)[i], colnames(x6)[j])}
        if (x6[i, j] == FALSE) {x8 <- c(x8, rownames(x6)[i], colnames(x6)[j], 
                                        paste(rownames(x6)[i], colnames(x6)[j], sep = ", "))}
      }
    }
    x8 <- unique(x8)
    
    # attribute to output
    out <- data.frame("model" = c("no interactions", x8) )
    
  }
  
  # Derive model formulae from the above
  x1 <- out[, "model"]
  for (i in 1:length(list_names) ) {
    x1 <- gsub(list_names[i], gsub("list", "", names(list_names[i]) ), x1)
  }
  x1 <- suppressWarnings(do.call(rbind, strsplit(x1, ", ")))
  # this will result in elements being recycled because of the uneven n of resulting columns, which the next few lines deal with
  x2 <- apply(x1, c(1, 2), function(x) {
    x3 <- strsplit(x, " x "); x3 <- sort(unlist(x3)); return(paste(x3, collapse = ""))
  })
  x3 <- apply(x2, 1, function(x) {unique(paste("x", x, sep = ""))})
  x3 <- sapply(x3, paste, collapse = " + ")
  x1 <- paste(paste("x", 1:n_lists, sep = ""), collapse = " + ")
  x2 <- paste("y ~  ", x1, " + ", x3, sep = "")
  out[, "formula"] <- x2
  out[1, "formula"] <- paste("y ~  ", x1, sep = "")
  
  # Lastly, add terms for exposure, confounders and interactions of these with profiles
  # profiles:exposure interactions
  if (! is.na(exposure)) { 
    for (i in 1:nrow(out)) {  
      x1 <- all.vars(as.formula(out[i, "formula"]))[-1]
      x1 <- paste(x1, exposure, sep = ":")
      x1 <- paste(x1, collapse = " + ")
      out[i, "formula"] <- paste(out[i, "formula"], exposure, x1, sep = " + ")
    }
  }  
  
  # profiles:confounder(s) interactions
  if (! is.na(confounders[1])) {
    for (i in 1:nrow(out)) {  
      for (j in confounders) {
        x1 <- all.vars(as.formula(out[i, "formula"]))[-1]
        if (length(grep(paste(confounders, collapse = "|"), x1)) > 0 ) {
          x1 <- x1[-grep(paste(confounders, collapse = "|"), x1)]
        }
        if (! is.na(exposure)) {x1 <- x1[-grep(exposure, x1)]}
        x1 <- paste(x1, j, sep = ":")
        x1 <- paste(x1, collapse = " + ")
        out[i, "formula"] <- paste(out[i, "formula"], j, x1, sep = " + ")
      }
    }
  }
  
  
  #...................................      
  ## Fit all possible log-linear models and calculate statistics
  
  # Statistics to compute for each model
  # symbol for unlisted observations
  if (n_lists == 3) {unlisted <- "m000"}
  if (n_lists == 4) {unlisted <- "m0000"}
  
  # unlisted observations
  out[, paste(unlisted, "screen", sep = "_")] <- NA
  
  # unlisted observations by exposure category, if exposure is categorical
  if (! is.na(exposure)) {
    if (length(levels(as.factor(df[, exposure]))) < 10 ) {
      out[, paste(unlisted, "screen", levels(as.factor(df[, exposure])), sep = "_")] <- NA
    }
  }
  
  # unlisted observations for model without each of the confounders, or no adjustment for confounders
  if (! is.na(confounders[1])) { out[, paste(unlisted, "without", c(confounders, "adjustment"), sep = "_")] <- NA }
  
  # other statistics  
  out[, "lrt_p"] <- NA
  
  # Fit saturated model (needed to perform likelihood-ratio test for models nested within it)
  
  # identify saturated model
  x1 <- lapply(out[, "model"], function(x) {unlist(strsplit(x, ","))} )
  x2 <- which.max(unlist(lapply(x1, length)))
  
  # fit saturated model
  fit_sat <- try(glm(formula = as.formula(out[x2, "formula"]), family = "poisson", data = df, maxit = 1000 ), 
                 silent = TRUE)
  
  # Fit all other models        
  for (i in 1:nrow(out) ) {
    print(paste("now fitting candidate model  ", i, "of", nrow(out), sep = " ") )
    
    # fit model (or at least try)
    suppressWarnings(rm(fit) )
    fit <- try(glm(formula = as.formula(out[i, "formula"]), family = "poisson", data = df, maxit = 1000 ), 
               silent = TRUE)
    
    # check: if model has not fit or any of the coefficients is NA, skip to next loop
    if (class("fit")[1] == "try-error" ) {next}
    if (any(is.na(coef(fit))) ) {next}
    
    # estimate expected number unlisted
    # select data with 000[0] profile
    df0 <- subset(df, is.na(y))
    
    # probability of being unlisted for each observation
    df0[, paste(unlisted, "screen", sep = "_")] <- predict(fit, df0)
    
    # number unlisted overall and by exposure level
    out[i, paste(unlisted, "screen", sep = "_")] <- round(sum(exp(na.omit(df0[, paste(unlisted, "screen", sep = "_")]))), digits = 0)
    if (! is.na(exposure)) {
      if( length(levels(as.factor(df[, exposure]))) < 10 ) {
        out[i, paste(unlisted, "screen", levels(as.factor(df[, exposure])), sep = "_")] <- 
          aggregate(df0[, paste(unlisted, "screen", sep = "_")],
                    by = list(as.factor(df0[, exposure])), FUN = function(x) {round(sum(exp(na.omit(x))), digits = 0)})[, 2]
      }
    }  
    
    # number unlisted if each confounder is taken out, or all are taken out of the model
    if (! is.na(confounders[1])) {
      
      # formula terms
      x1 <- gsub(" + ", " , ", out[i, "formula"], fixed = TRUE)
      x1 <- gsub("y ~ ", "", x1)
      x1 <- sapply(unlist(strsplit(x1, " , ")), trimws)
      
      # take out each of the confounders
      for (j in confounders) {
        # rewrite formula without confounder
        x2 <- x1[-grep(j, x1)]
        x2 <- as.formula(paste("y ~ ", paste(x2, collapse = " + "), sep = ""))
        
        # update fit with new formula
        x3 <- try(update(fit, formula = x2), silent = TRUE)
        
        # probability of being unlisted for each observation
        x4 <- predict(x3, df0)
        
        # number unlisted overall and by exposure level
        out[i, paste(unlisted, "without", j, sep = "_")] <- round(sum(exp(na.omit(x4))), digits = 0)
      }
      
      # take out all confounders
      # rewrite formula without confounders
      x2 <- x1[-grep(paste(confounders, collapse = "|"), x1)]
      x2 <- as.formula(paste("y ~ ", paste(x2, collapse = " + "), sep = ""))
      
      # update fit with new formula
      x3 <- try(update(fit, formula = x2), silent = TRUE)
      
      # probability of being unlisted for each observation
      x4 <- predict(x3, df0)
      
      # number unlisted overall and by exposure level
      out[i, paste(unlisted, "without_adjustment", sep = "_")] <- round(sum(exp(na.omit(x4))), digits = 0)
    }  
    
    # Likelihood ratio test p-value comparing model to saturated model (low p = model is better than saturated model)
    x1 <- -2 * (logLik(fit) - logLik(fit_sat))
    out[i, "lrt_p"] <- as.numeric(pchisq(x1, df = fit$df.residual - fit_sat$df.residual, lower.tail = FALSE))
    
  }
  
  #...................................      
  ## Return output and dataframe used for model fitting
  x1 <- list(out, df)
  names(x1) <- c("out", "df")
  return(x1)    
  
}      



#...............................................................................
### Function to perform model averaging for all eligible candidate models
#...............................................................................

f_model_average <- function(f_out, data_name_f, pars_f) {
  
  #...................................      
  ## Preparatory steps
  
  # Confirm number of lists
  n_lists <- pars_f[pars_f[, "loc_df"] == data_name_f, "n_lists"]
  # stop if fewer than three lists or more than four...
  if (! n_lists %in% c(3, 4)  | is.na(n_lists)) 
  {stop("wrong number of lists: only 3 or 4 lists allowed for a three- or four-list analysis")}
  
  # list names
  list_names <- pars_f[pars_f[, "loc_df"] == data_name_f, paste("list", 1:n_lists, sep = "")]
  
  # Identify exposure  
  exposure <- tolower(trimws(pars_f[pars_f[, "loc_df"] == data_name_f, "exposure"]))
  
  # Symbol for unlisted observations
  if (n_lists == 3) {unlisted <- "m000"}
  if (n_lists == 4) {unlisted <- "m0000"}
  
  # Dataframe for model fitting
  df <- f_out[["df"]]
  
  # select data with 000[0] profile
  df0 <- subset(df, is.na(y))
  
  # Set up output of model averaging
  out <- f_out[["out"]]
  
  # model eligibility and statistics
  out[, c("eligible", "aic", "post_prob")] <- NA
  
  # unlisted observations
  out[, unlisted] <- NA
  out[, paste(unlisted, "lci", sep = "_")] <- NA
  out[, paste(unlisted, "uci", sep = "_")] <- NA
  
  # unlisted observations by exposure category, if exposure is categorical
  if (! is.na(exposure)) {
    if (length(levels(as.factor(df[, exposure]))) < 10 ) {
      for (j in levels(as.factor(df[, exposure]))) {
        out[, paste(unlisted, j, sep = "_")] <- NA
        out[, paste(unlisted, j, "lci", sep = "_")] <- NA
        out[, paste(unlisted, j, "uci", sep = "_")] <- NA
      }
    }  
  }
  
  
  #...................................      
  ## Select models for averaging and assign explanations for exclusion    
  out[, "eligible"] <- "yes"
  for (i in 1:nrow(out)) {
    if (is.na(out[i, paste(unlisted, "screen", sep = "_")]) )
    {out[i, "eligible"] <- "no - model error or sparse dataset"}
    if (out[i, paste(unlisted, "screen", sep = "_")] / sum(df[, "y"], na.rm = TRUE) > pars_f[pars_f[, "loc_df"] == data_name_f, "plausibility_1"]) 
    {out[i, "eligible"] <- "no - implausible estimate"}
    if (out[i, "lrt_p"] > pars_f[pars_f[, "loc_df"] == data_name_f, "plausibility_2"]) 
    {out[i, "eligible"] <- "no - possible over-fitting"}
  }
  
  #...................................      
  ## Re-fit each eligible model, compute AIC and predict n unlisted
  for (i in 1:nrow(out)) {
    if (out[i, "eligible"] == "yes") {
      
      # refit model      
      fit <- try(glm(formula = as.formula(out[i, "formula"]), family = "poisson", data = df, maxit = 1000 ), 
                 silent = TRUE)
      
      # compute model's AIC
      out[i, "aic"] <- round(AIC(fit), digits = 2)
      
      # predict unlisted observations
      # contribution to total unlisted for each observation - point estimate and 95%CI
      x1 <- predict(fit, df0, se.fit = TRUE)
      df0[, unlisted] <- exp(x1[[1]])
      df0[, paste(unlisted, "lci", sep = "_")] <- exp(x1[[1]] - 1.96 * x1[[2]])
      df0[, paste(unlisted, "uci", sep = "_")] <- exp(x1[[1]] + 1.96 * x1[[2]])
      
      # estimated number unlisted overall... 
      out[i, unlisted] <- round(sum(na.omit(df0[, unlisted])), digits = 0)
      out[i, paste(unlisted, "lci", sep = "_")] <- round(sum(na.omit(df0[, paste(unlisted, "lci", sep = "_")])), digits = 0)
      out[i, paste(unlisted, "uci", sep = "_")] <- round(sum(na.omit(df0[, paste(unlisted, "uci", sep = "_")])), digits = 0)
      
      # ...and by exposure level
      if (! is.na(exposure)) { 
        if (length(levels(as.factor(df[, exposure]))) < 10 ) {
          out[i, paste(unlisted, levels(as.factor(df[, exposure])), sep = "_")] <- aggregate(df0[, unlisted],
                                                                                             by = list(as.factor(df0[, exposure])), FUN = function(x) {round(sum(na.omit(x)), digits = 0)})[, 2]
          out[i, paste(unlisted, levels(as.factor(df[, exposure])), "lci", sep = "_")] <- aggregate(df0[, paste(unlisted, "lci", sep = "_")],
                                                                                                    by = list(as.factor(df0[, exposure])), FUN = function(x) {round(sum(na.omit(x)), digits = 0)})[, 2]
          out[i, paste(unlisted, levels(as.factor(df[, exposure])), "uci", sep = "_")] <- aggregate(df0[, paste(unlisted, "uci", sep = "_")],
                                                                                                    by = list(as.factor(df0[, exposure])), FUN = function(x) {round(sum(na.omit(x)), digits = 0)})[, 2]
        }
      }
    }
  }
  
  #...................................      
  ## Calculate 'posterior probabilities' (weights) of eligible models from their AICs
  # (based on Rossi, 2010: https://rivista-statistica.unibo.it/article/view/3593/2945 )
  
  # Calculate an AIC delta based on lowest one
  out[, "aic_delta"] <- NA
  out[out[, "eligible"] == "yes", "aic_delta"] <- out[out[, "eligible"] == "yes", "aic"] - min(out[out[, "eligible"] == "yes", "aic"])
  
  # Then calculate weights / posterior probabilities 
  tot_prob <- sum(exp(- out[out[, "eligible"] == "yes", "aic_delta"] / 2))
  out[out[, "eligible"] == "yes", "post_prob"] <- 
    exp(- out[out[, "eligible"] == "yes", "aic_delta"] / 2) / tot_prob
  
  #...................................      
  ## Compute and return overall results
  
  # Format model output thus far
  out_raw <- out
  out_raw <- cbind(rep("", nrow(out_raw)), out_raw)
  colnames(out_raw)[1] <- "-"
  
  out_pretty <- out
  out_pretty[, "deaths outside any list (95%CI)"] <- paste(
    out_pretty[, unlisted], " (", 
    out_pretty[, paste(unlisted, "lci", sep = "_")], " to ", 
    out_pretty[, paste(unlisted, "uci", sep = "_")], ")", sep = "")
  if (! is.na(exposure)) { 
    if (length(levels(as.factor(df[, exposure]))) < 10 ) {
      for (i in levels(as.factor(df[, exposure])) ) {
        out_pretty[, paste("deaths outside any list (95%CI)", i, sep = " - ")] <- paste(
          out_pretty[, paste(unlisted, i, sep = "_")], " (", 
          out_pretty[, paste(unlisted, i, "lci", sep = "_")], " to ", 
          out_pretty[, paste(unlisted, i, "uci", sep = "_")], ")", sep = "")
      }  
    }
  }
  out_pretty[, "likelihood ratio p-value"] <- out_pretty[, "lrt_p"]
  out_pretty[, "AIC"] <- round(out_pretty[, "aic"], 2)
  out_pretty[, "posterior probability"] <- round(out_pretty[, "post_prob"], 3)
  out_pretty <- out_pretty[, c("model", grep("outside", colnames(out_pretty), value = TRUE),
                               "likelihood ratio p-value", "AIC", "posterior probability")]
  out_pretty <- cbind(rep("", nrow(out_pretty)), out_pretty)
  colnames(out_pretty)[1] <- "-"
  
  # Estimated unlisted and total deaths with 95%CIs
  out_est_raw <-c(
    weighted.mean(out[, unlisted], out$post_prob, na.rm = TRUE),
    weighted.mean(out[, paste(unlisted, "lci", sep = "_")], out$post_prob, na.rm = TRUE),
    weighted.mean(out[, paste(unlisted, "uci", sep = "_")], out$post_prob, na.rm = TRUE)
  )
  out_est_raw <-c(out_est_raw, sum(df[, "y"], na.rm = TRUE) + out_est_raw)
  if (! is.na(exposure)) { 
    if (length(levels(as.factor(df[, exposure]))) < 10 ) {
      for (i in levels(as.factor(df[, exposure])) ) {
        x1 <- c(weighted.mean(out[, paste(unlisted, i, sep = "_")], out$post_prob, na.rm = TRUE),
                weighted.mean(out[, paste(unlisted, i, "lci", sep = "_")], out$post_prob, na.rm = TRUE),
                weighted.mean(out[, paste(unlisted, i, "uci", sep = "_")], out$post_prob, na.rm = TRUE)
        )
        out_est_raw <- rbind(out_est_raw, c(x1, sum(df[which(df[, exposure] == i), "y"], na.rm = TRUE) + x1)
        )
      }  
    }
  }
  
  out_est_raw <- as.data.frame(rbind(out_est_raw))
  out_est_raw <- round(out_est_raw, 0)
  out_est_raw <- cbind(rep(NA, nrow(out_est_raw)), out_est_raw)
  colnames(out_est_raw) <- c("stratum", "unlisted", "unlisted_lci", "unlisted_uci", 
                             "total_deaths_est", "total_deaths_lci", "total_deaths_uci")
  out_est_raw[1, "stratum"] <- "overall"
  if (! is.na(exposure)) { 
    if (length(levels(as.factor(df[, exposure]))) < 10 ) {
      for (i in 1:length(levels(as.factor(df[, exposure]))) ) {
        out_est_raw[1+i, "stratum"] <- levels(as.factor(df[, exposure]))[i]
      }  
    }
  }
  out_est_raw <- cbind(rep("", nrow(out_est_raw)), out_est_raw)
  colnames(out_est_raw)[1] <- "-"
  
  out_est_pretty <- as.data.frame(matrix(NA, nrow = nrow(out_est_raw), ncol = 3))
  for (i in 1:nrow(out_est_raw)) {
    out_est_pretty[i, 1] <- out_est_raw[i, 2]
    out_est_pretty[i, 2] <- paste(out_est_raw[i, 3], " (", out_est_raw[i, 4], " to ", out_est_raw[i, 5], ")", sep = "")
    out_est_pretty[i, 3] <- paste(out_est_raw[i, 6], " (", out_est_raw[i, 7], " to ", out_est_raw[i, 8], ")", sep = "") 
  }
  colnames(out_est_pretty) <- c("stratum", "deaths outside any list (95%CI)", "total deaths (95%CI)")
  out_est_pretty <- cbind(rep("", nrow(out_est_pretty)), out_est_pretty)
  colnames(out_est_pretty)[1] <- "-"
  
  # Sensitivity of each list and all lists combined
  out_sens_raw <- as.data.frame(matrix(NA, ncol = 5, nrow = n_lists + 1))
  colnames(out_sens_raw) <- c("list", "n_deaths", "sens_est", "sens_lci", "sens_uci")
  out_sens_raw[, "list"] <- c(unlist(list_names), "all lists")
  
  for (i in 1:n_lists) {
    out_sens_raw[i, "n_deaths"] <- sum(df[df[, grep(as.character(i), colnames(df))] == 1, "y"], na.rm = TRUE)
  }
  
  # Calculate sensitivity for all lists combined
  out_sens_raw[n_lists + 1, "n_deaths"] <- sum(out_sens_raw[1:n_lists, "n_deaths"], na.rm = TRUE)
  
  # Calculate sensitivity estimates, LCI, and UCI
  out_sens_raw[, "sens_est"] <- out_sens_raw[, "n_deaths"] / out_est_raw[1, "total_deaths_est"]
  out_sens_raw[, "sens_lci"] <- out_sens_raw[, "n_deaths"] / out_est_raw[1, "total_deaths_uci"]
  out_sens_raw[, "sens_uci"] <- out_sens_raw[, "n_deaths"] / out_est_raw[1, "total_deaths_lci"]
  
  out_sens_raw <- cbind(rep("", nrow(out_sens_raw)), out_sens_raw)
  colnames(out_sens_raw)[1] <- "-"
  
  out_sens_pretty <- out_sens_raw
  out_sens_pretty[, "number of deaths"] <- out_sens_pretty[, "n_deaths"]
  out_sens_pretty[, "sensitivity (95%CI)"] <- paste(round(out_sens_pretty[, "sens_est"] * 100, 1), "% (",
                                                    round(out_sens_pretty[, "sens_lci"] * 100, 1), "% to ", round(out_sens_pretty[, "sens_uci"] * 100, 1), "%)", sep = "")
  out_sens_pretty <- out_sens_pretty[, c("list", "number of deaths", "sensitivity (95%CI)")]
  out_sens_pretty <- cbind(rep("", nrow(out_sens_pretty)), out_sens_pretty)
  colnames(out_sens_pretty)[1] <- "-"
  
  
  # Return outputs as a list
  x1 <- list("out_raw" = out_raw, "out_pretty" = out_pretty, "out_est_raw" = out_est_raw, 
             "out_est_pretty" = out_est_pretty, "out_sens_raw" = out_sens_raw, "out_sens_pretty" = out_sens_pretty)
  return(x1)
  
}



#...............................................................................
### Function to tabulate overlap among lists, create contingency table in vector 
    # form and draw Venn diagram for entire dataset and desired strata
#...............................................................................

f_overlap <- function(data_f, data_name_f, pars_f, f_palette) {
  
  #...................................      
  ## Preparatory steps
  
  # Figure out number of lists
  n_lists <- pars_f[pars_f[, "loc_df"] == data_name_f, "n_lists"]
  # stop if fewer than two lists or more than four...
  if (n_lists < 2 | n_lists > 4 | is.na(n_lists)) 
  {stop("too few or too many lists: only 2, 3 or 4 lists allowed")}
  
  # List names
  list_names <- pars_f[pars_f[, "loc_df"] == data_name_f, paste("list", 1:n_lists, sep = "")]
  
  # Remove ineligible observations
  if ("eligible" %in% colnames(data_f)) {data_f <- subset(data_f, eligible == TRUE)}
  
  # Set up output overlap vectors for overall dataset and each stratum
  # strata
  x1 <- grep(paste(c("gender_stratum", "age_stratum", "period_stratum"), collapse = "|"), colnames(data_f), value = TRUE)
  x2 <- c()
  for (i in x1) {x2 <- c(x2, rep(i, length(levels(data_f[, i]))))}
  x3 <- c()
  for (i in x1) {x3 <- c(x3, levels(data_f[, i]))}
  out <- data.frame("variable" = c("overall", x2), "stratum" = c("overall", x3) )
  
  # Create n-list contingency table
  x1 <- as.data.frame(permutations(n = 2, r = n_lists, v = c(0, 1), repeats.allowed = TRUE))
  x1 <- x1[! rowSums(x1) == 0, ]
  x2 <- apply(cbind("x", x1), 1, paste, collapse = "")
  tab <- as.data.frame(matrix(NA, ncol = n_lists + 1, nrow = length(x2)))
  colnames(tab) <- c("permutation", paste("list", 1:n_lists, sep = "") )
  tab[, "permutation"] <- x2
  tab[, grep("list", colnames(tab))] <- x1
  
  # contingency cells in output
  out[, tab[, "permutation"] ] <- NA
  
  # Choose colours
  x3 <- f_palette[c(2, 6, 8, 14)]
  colours <- c(x3[1:n_lists])
  
  #...................................      
  ## Compute contigency table and draw venn diagrams for all observations and each desired stratum
  
  for (i in 1:nrow(out)) {
    
    # Select data
    if (out[i, "variable"] == "overall") {x1 <- data_f}
    if (out[i, "variable"] != "overall") {x1 <- data_f[data_f[, out[i, "variable"]] == out[i, "stratum"], ] }
    
    # Contingency table
    # reset contingency table
    tab <- tab[, colnames(tab) != "Freq"]
    
    # tabulate overlap among lists
    x3 <- "~ list1"
    for (j in 2:n_lists) {x3 <- paste(x3, paste("list", j, sep = ""), sep = " + ")}
    x3 <- as.formula(x3)
    x2 <- as.data.frame(xtabs(x3, data = x1) )
    
    # add to contingency table
    tab <- merge(tab, x2, by = paste("list", 1:n_lists, sep = ""), all.x = TRUE)
    tab[is.na(tab[, "Freq"]), "Freq"] <- 0
    
    # transfer to output
    out[i, tab[, "permutation"]] <- tab[, "Freq"]
    
    # Draw and save Venn diagram
    # create list of sets
    x1[, "unique_id"] <- paste("id", 1:nrow(x1), sep = "")
    x2 <- list()
    for (j in 1:n_lists) {x2[[j]] <- x1[x1[, paste("list", j, sep = "")] == 1, "unique_id"]}
    names(x2) <- list_names
    
    # plot and save
    plot <- ggvenn(x2, fill_color = colours, fill_alpha = 0.3, show_percentage = FALSE,
                   stroke_color = "grey50", stroke_alpha = 0.7, stroke_size = 1, set_name_color = "grey20", set_name_size = 4,
                   text_color = "grey20", text_size = 4)
    print(plot)
    ggsave(paste(data_name_f, "_venn_", gsub("-", "_", paste(out[i, "stratum"]) ), ".png", sep = ""), plot, 
           width = 25, height = 15, units = "cm", dpi = "print")
    
    # assign plot name
    assign(paste("venn_", gsub("-", "_", paste(out[i, "stratum"]) ), sep = ""), plot)
  }
  
  #...................................      
  ## Format and save output
  
  # Create combined plots of all Venn diagrams
  x1 <- gsub("-", "_", paste("venn_", out[, "stratum"], sep = "") )
  plot_list <- mget(x1)
  plot_labels <- gsub("venn_", "", x1 )
  plot_labels <- gsub("_", " ", plot_labels)
  # fix date labels
  x2 <- substr(plot_labels, 1, 1) %in% c(1:2)
  for (i in plot_labels[x2]) {
    plot_labels[plot_labels == i] <- paste(substr(i, 9, 10), "/", substr(i, 6, 7), "/",  substr(i, 3, 4), " to ",
                                           substr(i, 23, 24), "/", substr(i, 20, 21), "/",  substr(i, 17, 18), sep = "") 
  }
  
  plot_comb <- ggpubr::ggarrange(plotlist = plot_list, labels = plot_labels, hjust = 0, vjust = 1,
                                 font.label = list(size = 12, face = "bold", color = "grey40"))  
  print(plot_comb)
  
  # Save combined plots
  ggsave(paste(data_name_f, "_venn_", "comb_long.png", sep = ""), plot_comb, 
         width = 18, height = 25, units = "cm", dpi = "print")      
  ggsave(paste(data_name_f, "_venn_", "comb_wide.png", sep = ""), plot_comb, 
         width = 30, height = 20, units = "cm", dpi = "print")      
  
  # Return and save output vector
  write.csv(out, paste(data_name_f, "_table_overlap.csv", sep = ""), row.names = FALSE)
  return(out)
}  


#...............................................................................
### Function to prepare datasets for gender, period and age strata, as desired
#...............................................................................

f_stratify <- function(data_f, data_name_f, pars_f, f_clean_date) {
  
  #...................................      
  ## Preparatory steps  
  
  # Name of the dataset
  x1 <- data_name_f
  
  #...................................      
  ## Prepare gender stratum variable        
  if (pars_f[pars_f[, "loc_df"] == x1, "gender"] == "Y") {
    data_f[, "gender_stratum"] <- factor(data_f[, "gender"], levels = c("female", "male"))
    
    # Stop if the dataset contains only one gender
    if (length(unique(data_f[, "gender"])) < 2 ) {
      stop("you want to stratify by gender, but the data contain only one gender: check parameters")
    }  
  }
  
  #...................................      
  ## Prepare age stratum variable        
  if (! is.na(pars_f[pars_f[, "loc_df"] == x1, "age_cutoffs"])) {
    # Identify strata
    x2 <- pars_f[pars_f[, "loc_df"] == x1, "age_cutoffs"]
    x2 <- as.character(x2)
    x2 <- as.integer(unlist(strsplit(x2, ",")))
    
    # Stop if the dataset doesn't feature all the age strata
    if (min(data_f[, "age"], na.rm = TRUE) > min(x2) |  max(data_f[, "age"], na.rm = TRUE) < max(x2)) {
      stop("you want to stratify by age, but the data do not contain all the desired age strata: check parameters")
    }  
    
    
    # Create labels
    x2_lab <- c()
    for (i in 1:length(x2)) {
      if (i == 1) x2_lab <- paste("age_0_to_", x2[i] - 1, sep = "")
      if (i > 1) x2_lab <- c(x2_lab, paste("age_", x2[i-1], "_to_", x2[i] - 1, sep = "") )
    }
    x2_lab <- c(x2_lab, paste("age_", x2[length(x2)], "_plus", sep = "") )
    
    # Create strata
    data_f[, "age_stratum"] <- cut(data_f[, "age"], breaks = c(0, x2, 1000), labels = x2_lab)
  }
  
  #...................................      
  ## Prepare period stratum variable     
  if (! is.na(pars_f[pars_f[, "loc_df"] == x1, "period_cutoffs"])) {
    # Identify strata
    x2 <- pars_f[pars_f[, "loc_df"] == x1, "period_cutoffs"]
    x2 <- unlist(strsplit(as.character(x2), ","))
    x2 <- lubridate::as_date(sapply(x2, f_clean_date))
    
    # Stop if periods are outside date range of dataset
    if (min(x2) < min(data_f[, "date_clean"], na.rm = TRUE) | max(x2) > max(data_f[, "date_clean"], na.rm = TRUE) ) {
      stop("period cut-offs outside range of dates within dataset: check parameters")  
    }
    
    # Create labels
    x2_lab <- c()
    for (i in 1:length(x2)) {
      if (i == 1) x2_lab <- paste(min(data_f[, "date_clean"], na.rm = TRUE), "_to_", x2[i] - 1, sep = "")
      if (i > 1) x2_lab <- c(x2_lab, paste(x2[i-1], "_to_", x2[i] - 1, sep = "") )
    }
    x2_lab <- c(x2_lab, paste(x2[length(x2)], "_to_", max(data_f[, "date_clean"], na.rm = TRUE), sep = "") )
    
    # Create strata
    data_f[, "period_stratum"] <- cut(data_f[, "date_clean"], breaks = c(min(data_f[, "date_clean"], na.rm = TRUE),
                                                                         x2, lubridate::as_date(Sys.Date())), labels = x2_lab)
  }    
  
  #...................................      
  ## Return prepared dataset
  return(data_f)
  
}


