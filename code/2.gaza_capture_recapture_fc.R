#...............................................................................
###  R CODE FOR CAPTURE-RECAPTURE ANALYSIS OF MORTALITY IN GAZA (OCT 2024)   ###
#...............................................................................


#...............................................................................
### Preparatory steps
#...............................................................................

  #...................................      
  ## Install or load required R packages
  if (!"pacman" %in% rownames(installed.packages())){install.packages("pacman")}
  
  pacman::p_load(
    ggplot2,       # Data visualization
    ggpubr,        # Arranging multiple plots into a single plot
    ggrepel,       # Improve labelling of plots
    lubridate,     # Makes it easier to work with dates and times
    mice,          # Impute missing data
    readxl,        # Read Excel files
    scales,        # Scaling and formatting data for visualizations
    showtext,      # Manage fonts in graphs
    tidyverse,     # Tidyverse suite of packages
    viridis,       # Colour palettes
    zoo)           # For computing running means

  #...................................      
  ## Starting setup

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
    
    # Functions
    source(paste0(dir_path, "/code/3.bespoke_functions.R"), echo = TRUE)

  #...................................      
  ## Set parameters
    
    # List names
    list_names <- data.frame(
      list = paste0("list", 1:3), 
      list_name = c("hospitals", "survey", "social media"),
      list_colour = palette_gen[c(3, 8, 13)]
    )
    
    # Estimate and confidence intervals
    stats <- c("est", "lci", "uci")
    
    # Start and end date of analysis period
    date_start <- as.Date("2023-10-07")
    date_end <- as.Date("2023-07-31")

    # Plausibility thresholds
    plausibility <- 5 # if unlisted estimate is >= 5x total n listed, exclude
      # from model averaging

    # Number of MICE imputations
    n_imp <- 100
    
#...............................................................................
### Describing the data
#...............................................................................

  #...................................      
  ## Check variable format and ranges
  for (i in 1:3) {print(table(df[, paste0("list", i)], useNA = "ifany"))}
  range(df$age, na.rm = T)
  x <- c("age", "gender", "month_death", "year_death", "gov_ns", "gov")
  for (i in x) {
    print("#_______")
    print(i)
    print(table(df[, i], useNA = "ifany"))
  }
  
  #...................................      
  ## Visualise missingness
  tab <- data.frame(variable = x, missing = NA, percent = NA)
  for (i in tab$variable) {tab[which(tab$variable == i),
    "missing"] <- length(which(is.na(df[, i])))}
  tab$percent <- percent(tab$missing / nrow(df)  )
  write.csv(tab, paste0(dir_path, "output/tab_missingness.csv"), row.names = F)

  #...................................      
  ## Reconstitute 'long' dataset with one row = one record (before matching)
    
    # Reshape dataset long
    long <- data.frame()
    for (i in 1:3) {
      x <- df[which(df[, paste0("list", i)] == 1), ]
      x <- x[, -grep("list", colnames(x))]
      x$list <- paste0("list", i)
      long <- rbind(long, x)
    }
    
    # Add list name
    long <- merge(long, list_names, by = "list", all.x = T)

      
  #...................................      
  ## Tabulate characteristics of each list, and do significance testing
   
    # Categorise age
    long$age_cat <- cut(long$age, c(0, 15, 30, 45, 60, 120), include.lowest = T,
      right = F)
     
    # Initialise table
    tab <- data.frame(
      characteristic = c(
        c("sex", rep(NA, length(table(long$gender))-1)),
        c("age (years)", rep(NA, length(table(long$age_cat))-1)),
        c("region of death", rep(NA, length(table(long$gov_ns))-1)),
        c("governorate of death", rep(NA, length(table(long$gov))-1)),
        c("year of death", rep(NA, length(table(long$year_death))-1)),
        c("month of death", rep(NA, length(table(long$month_death))-1))
      ),
      category = c(
        names(table(long$gender)),
        names(table(long$age_cat)),
        names(table(long$gov_ns)),
        names(table(long$gov)),
        names(table(long$year_death)),
        names(table(long$month_death))
      )      
    )
    
    # Add contingency cells
    x <- rbind(
      as.data.frame.matrix(table(long$gender, long$list_name)),
      as.data.frame.matrix(table(long$age_cat, long$list_name)),
      as.data.frame.matrix(table(long$gov_ns, long$list_name)),
      as.data.frame.matrix(table(long$gov, long$list_name)),
      as.data.frame.matrix(table(long$year_death, long$list_name)),
      as.data.frame.matrix(table(long$month_death, long$list_name))
    )
    tab <- cbind(tab, x)
    
    # Add column-wise percentages
    x <- rbind(
      as.data.frame.matrix(prop.table(table(long$gender, long$list_name), 2)),
      as.data.frame.matrix(prop.table(table(long$age_cat, long$list_name), 2)),
      as.data.frame.matrix(prop.table(table(long$gov_ns,long$list_name),2)),
      as.data.frame.matrix(prop.table(table(long$gov,long$list_name),2)),
      as.data.frame.matrix(prop.table(table(long$year_death,long$list_name),2)),
      as.data.frame.matrix(prop.table(table(long$month_death,long$list_name),2))
    )
    x <- apply(x, c(1,2), percent, accuracy = 0.1)
    colnames(x) <- list_names$list_name
    for (i in list_names$list_name) {
      tab[, i] <- paste0(tab[, i], " (", x[, i], ")")
    }
    
    # Add Fisher p-value test of significance (non-missing categories only)
    f_fisher <- function(x) {
      x <- x[rownames(x) != "missing", ]
      return(fisher.test(x, simulate.p.value = T)$p.value)
    }
    p_value <- c(
      f_fisher(as.data.frame.matrix(table(long$gender, long$list_name))),
      f_fisher(as.data.frame.matrix(table(long$age_cat, long$list_name))),
      f_fisher(as.data.frame.matrix(table(long$gov_ns, long$list_name))),
      f_fisher(as.data.frame.matrix(table(long$gov, long$list_name))),
      f_fisher(as.data.frame.matrix(table(long$year_death, long$list_name))),
      f_fisher(as.data.frame.matrix(table(long$month_death, long$list_name)))
    )
    p_value <- scales::pvalue(p_value)
    p_value <- data.frame(characteristic = na.omit(tab$characteristic), p_value)
    tab$row <- 1:nrow(tab)
    tab <- merge(tab, p_value, by = "characteristic", all.x = T)
    tab <- tab[order(tab$row), ]
    tab <- subset(tab, select=-row)
    
    # Add list totals
    tab <- rbind(tab, c("total observations", NA, table(long$list_name), NA))
    
    # Save table
    write.csv(tab, paste0(dir_path, "output/tab_descriptive.csv"),row.names = F, 
      na = "")   

    
#...............................................................................
### Imputing missing data
#...............................................................................

  #...................................      
  ## Create multiple datasets with imputed missing variables (age, month_death)
    
    # Prepare the data for imputation
    df$month_death <- factor(month.abb[df$month_death])
    df$gender <- factor(df$gender)
    df <- subset(df, select = -gov_ns)

    # Do multiple imputation (random forest for age and sex, sample of 
        # non-missing data for month of death)
    imp <- mice(df, m = 20, maxit = 5, 
      defaultMethod = c("rf", "rf", "sample", "rf") )

    # Inspect convergence
    plot(imp)  ## looks OK- different imputations criss-cross, no obvious trend
    
    # Go up to 40 iterations and see whether we're still OK
    imp40 <- mice.mids(imp, maxit = 35, print = T)
    plot(imp40)  ## still OK, so we can revert to 5 iterations

    
  #...................................      
  ## Compare distributions of imputed and non-missing values
    
    # Set up datasets for graphing
    combi <- complete(imp, "long")
    combi$source <- "imputed"
    combi$age_imputed <- rep(is.na(df$age), max(combi$.imp))
    combi$gender_imputed <- rep(is.na(df$gender), max(combi$.imp))
    combi$month_death_imputed <- rep(is.na(df$month_death), max(combi$.imp))
    complete <- df[complete.cases(df), ]
    complete$source <- "non-missing"
    complete[, c("age_imputed", "gender_imputed", "month_death_imputed", 
      ".imp", ".id")] <- NA
    combi_age <- rbind(subset(combi, age_imputed), complete[, colnames(combi)])
    combi_age$age_cat <- cut(combi_age$age, c(0, 15, 30, 45, 60, 120), 
      include.lowest = T, right = F, labels = c("0 to 14y", "15 to 29y",
        "30 to 44y", "45 to 59y", "60+y"))
    combi_gender <- rbind(subset(combi, gender_imputed), 
      complete[, colnames(combi)])
    combi_month_death <- rbind(subset(combi, month_death_imputed), 
      complete[, colnames(combi)])
    combi_month_death$month_death <- month.abb[combi_month_death$month_death]
    combi_month_death$month_death <- factor(combi_month_death$month_death,
      levels = month.abb, labels = month.abb)
    
    # Graphs for age, gender and month of death + combined graph
    pl_age <- ggplot(combi_age,
      aes(x = age_cat, colour = source, fill = source)) +
      geom_bar(alpha = 0.5) +
      facet_wrap(source ~., nrow = 2, scales = "free_y") +
      scale_x_discrete("age category") +
      scale_y_continuous("frequency", labels = comma) +
      theme_bw() +
      theme(legend.position = "top", axis.text.x = element_text(angle = 45,
        hjust = 1, vjust = 1)) +
      scale_colour_manual("source", values = palette_gen[c(6,12)]) +
      scale_fill_manual("source", values = palette_gen[c(6,12)])
    pl_gender <- ggplot(combi_gender,
      aes(x = gender, colour = source, fill = source)) +
      geom_bar(alpha = 0.5) +
      facet_wrap(source ~., nrow = 2, scales = "free_y") +
      scale_x_discrete("sex") +
      scale_y_continuous("frequency", labels = comma) +
      theme_bw() +
      theme(legend.position = "top", axis.text.x = element_text(angle = 45,
        hjust = 1, vjust = 1)) +
      scale_colour_manual("source", values = palette_gen[c(6,12)]) +
      scale_fill_manual("source", values = palette_gen[c(6,12)])
    pl_month <- ggplot(combi_month_death,
      aes(x = month_death, colour = source, fill = source)) +
      geom_bar(alpha = 0.5) +
      facet_wrap(source ~., nrow = 2, scales = "free_y") +
      scale_x_discrete("month of death") +
      scale_y_continuous("frequency", labels = comma) +
      theme_bw() +
      theme(legend.position = "top") +
      scale_colour_manual("source", values = palette_gen[c(6,12)]) +
      scale_fill_manual("source", values = palette_gen[c(6,12)])

    ggarrange(pl_age, pl_gender, pl_month, ncol = 3, labels = c(NA, NA, NA),
      align = "hv", widths = c(1, 0.5, 1), common.legend = T)
    ggsave(paste0(dir_path, "output/imputed_vs_nonmissing.png"), dpi = "print",
      units = "cm", height = 15, width = 23)    

    
  #...................................      
  ## Now run desired number of iterations
    
    # Run iterations
    imp <- mice(df, m = n_imp, maxit = 5, method = "rf")
    
    # Extract, manage and save imputed datasets in long format
    long <- complete(imp, "long")
    
    # Categorise age
    long$age_cat <- cut(long$age, c(0, 15, 30, 45, 60, 120), 
      include.lowest = T, right = F, labels = c("0 to 14y", "15 to 29y",
        "30 to 44y", "45 to 59y", "60+y"))
    
    # Generate single stratification variables (age-sex, month of death)
    long$agesex <- paste(long$age_cat, long$gender, sep="_")
    long$agesex <- factor(long$agesex, levels = sort(unique(long$agesex)))
    long$month_death <- factor(as.character(long$month_death), 
      levels = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun"))

    # Save      
    saveRDS(long, paste0(dir_path, "output/df_imp.rds"))


#...............................................................................
### Implementing capture-recapture analysis
#...............................................................................
    
  #...................................      
  ## Check which stratifications the data allow for
      # (for this, just use unimputed dataset)
    
    # Categorise age and create stratum variables
    df2 <- df
    df2$age_cat <- cut(df2$age, c(0, 15, 30, 45, 60, 120), 
      include.lowest = T, right = F)
    df2$stratum_all <- paste(df2$age_cat, df2$gender, df2$month_death, sep="_")
    df2$stratum_agesex <- paste(df2$age_cat, df2$gender, sep="_")

    # Remove missing values
    df2 <- df2[complete.cases(df2[, c("age_cat", "gender", "month_death")]), ]
    
    # Come up with contingency cell names
    x <- as.data.frame(table(df2[, c("list1", "list2", "list3")]))
    tab_names <- paste0("x", x$list1, x$list2, x$list3)
    
    # Check overlap for age-sex-month strata
    x <- by(df2, df2$stratum_all, FUN = function(xx) 
      {as.data.frame(table(xx[, c("list1", "list2", "list3")]))$Freq})
    x <- as.data.frame(do.call(rbind, x))
    colnames(x) <- tab_names
    x$n_zeroes <- apply(x, 1, function(xx) {length(which(xx == 0))})
    table(x$n_zeroes) # cannot stratify by age, sex and month
  
    # Check overlap for age-sex strata
    x <- by(df2, df2$stratum_agesex, FUN = function(xx) 
      {as.data.frame(table(xx[, c("list1", "list2", "list3")]))$Freq})
    x <- as.data.frame(do.call(rbind, x))
    colnames(x) <- tab_names
    x$n_zeroes <- apply(x, 1, function(xx) {length(which(xx == 0))})
    table(x$n_zeroes) # OK to stratify by age and sex
  
  #...................................      
  ## Test whether models with adjustment perform better than those without
    
    # Set up output of test
    out_test_agesex <- data.frame(n_imp = 1:5, mean_aic_with = NA, 
      mean_aic_without = NA, mean_unlisted_with = NA, mean_unlisted_without =NA)
    out_test_month_death <- out_test_agesex
    
    # Loop through first 5 imputations (should be enough for this test)
    for (i in 1:5) {
      
      # progress
      print(paste0("now doing test on imputation ", i))
      
      # estimate mortality by age-sex with and without adjustment
      out <- f_prepare(data_f = long[which(long$.imp == i), ], 
        confounders = "month_death")
      out_with <- f_model(stratum = "agesex", 
        confounders = "month_death", verbose = F)
      out <- f_prepare(data_f = long[which(long$.imp == i), ], 
        confounders = NA)
      out_without <- f_model(stratum = "agesex", 
        confounders = NA, verbose = F)
      out_test_agesex[which(out_test_agesex$n_imp == i), 2:5] <- 
        c(mean(out_with$aic, na.rm = T), 
          mean(out_without$aic, na.rm = T),
          mean(out_with$unlisted_est, na.rm = T), 
          mean(out_without$unlisted_est, na.rm = T)
        )
      
      # estimate mortality by month with and without adjustment
      out <- f_prepare(data_f = long[which(long$.imp == i), ], 
        confounders = c("age", "gender"))
      out_with <- f_model(stratum = "month_death", 
        confounders = c("age", "gender"), verbose = F)
      out <- f_prepare(data_f = long[which(long$.imp == i), ], 
        confounders = NA)
      out_without <- f_model(stratum = "month_death", 
        confounders = NA, verbose = F)
      out_test_month_death[which(out_test_month_death$n_imp == i), 2:5] <- 
        c(mean(out_with$aic, na.rm = T), 
          mean(out_without$aic, na.rm = T),
          mean(out_with$unlisted_est, na.rm = T), 
          mean(out_without$unlisted_est, na.rm = T)
        )
    }
    
    # Save results of test
    write.csv(out_test_agesex, 
      paste0(dir_path, "output/out_test_agesex.csv"), row.names = F)
    write.csv(out_test_month_death, 
      paste0(dir_path, "output/out_test_month_death.csv"), row.names = F)

        
  #...................................      
  ## Loop through all imputation datasets, performing stratified (age-sex-month)
      # capture-recapture analysis for each
    
    # Initialise outputs
      # age-sex stratification, adjusted for month (categorical)
      est_agesex <- expand.grid(n_imp = 1:max(long$.imp), 
        agesex = levels(long$agesex), n_model = 1:8)
      x <- c("model", "formula", "stratum", "eligible", "aic", "aic_delta",
        "post_prob", "list1", "list2", "list3", "listed",
        "unlisted_est", "unlisted_lci", "unlisted_uci")
      est_agesex[, x] <- NA
      est_agesex <- est_agesex[order(est_agesex$n_imp, 
        est_agesex$agesex, est_agesex$n_model), ]
      
      # age-sex stratification, unadjusted
      est_agesex_unadj <- est_agesex
      
      # month stratification, adjusted for age (continuous) and sex
      est_month_death <- expand.grid(n_imp = 1:max(long$.imp), 
        month_death = levels(long$month_death), n_model = 1:8)
      x <- c("model", "formula", "stratum", "eligible", "aic", "aic_delta",
        "post_prob", "list1", "list2", "list3", "listed",
        "unlisted_est", "unlisted_lci", "unlisted_uci")
      est_month_death[, x] <- NA
      est_month_death <- est_month_death[order(est_month_death$n_imp, 
        est_month_death$month_death, est_month_death$n_model), ]
      
    # Progress bar
    pb <- txtProgressBar(min = 1, max = max(long$.imp), style = 3)
    
    # Loop through
    for (i in 1:max(long$.imp)) {
      
      # progress update
      setTxtProgressBar(pb, i)
          
      # estimate mortality by age-sex (adjusted) and update output dataframe
      out <- f_prepare(data_f = long[which(long$.imp == i), ], 
        confounders = "month_death")
      out_agesex <- f_model(stratum = "agesex", 
        confounders = "month_death", verbose = F)
      est_agesex[which(est_agesex$n_imp==i),colnames(out_agesex)] <- out_agesex
      
      # estimate mortality by age-sex (unadjusted) and update output dataframe
      out <- f_prepare(data_f = long[which(long$.imp == i), ], 
        confounders = NA)
      out_agesex_unadj <- f_model(stratum = "agesex", 
        confounders = NA, verbose = F)
      est_agesex_unadj[which(est_agesex_unadj$n_imp==i),
        colnames(out_agesex_unadj)] <- out_agesex_unadj
      
      # estimate mortality by month and update output dataframe
      out <- f_prepare(data_f = long[which(long$.imp == i), ], 
        confounders = c("age", "gender"))
      out_month_death <- f_model(stratum = "month_death", 
        confounders = c("age", "gender"), verbose = F)
      est_month_death[which(est_month_death$n_imp == i), 
        colnames(out_month_death)] <- out_month_death
    }
    close(pb)

    # Save outputs
    saveRDS(est_agesex, paste0(dir_path, "output/est_agesex.rds"))
    saveRDS(est_agesex_unadj, paste0(dir_path, "output/est_agesex_unadj.rds"))
    saveRDS(est_month_death, paste0(dir_path, "output/est_month_death.rds"))

    
    
#...............................................................................
### Averaging models and visualising results
#...............................................................................

  #...................................      
  ## Which stratification generally fits the data better? Use it to compute 
      # overall est.
    
    # Compare AICs
    mean(est_agesex$aic) # this model has lower AICs
    mean(est_month_death$aic)

  #...................................      
  ## Average models by stratum
    
    # Age-sex stratification
    x <- by(est_agesex, est_agesex[, c("n_imp", "stratum")], function(xx) {
      return(c(unique(xx$n_imp), unique(xx$stratum), 
        apply(xx[which(xx$eligible), c("list1","list2","list3","listed", 
          "unlisted_est","unlisted_lci","unlisted_uci")], 
        2, weighted.mean, w = xx$post_prob)))
    })
    x <- as.data.frame(do.call(rbind, x))
    colnames(x)[1:2] <- c("n_imp", "stratum")
    x[, grep("list", colnames(x))] <- apply(x[, grep("list", colnames(x))], 2,
      as.numeric)
    x[, c("unlisted_est","unlisted_lci","unlisted_uci")] <- 
      apply(x[, c("unlisted_est","unlisted_lci","unlisted_uci")], 2, 
        function(xx) {round(as.numeric(xx), 0)})
    ave_agesex <- x

    # Age-sex stratification (unadjusted)
    x <- by(est_agesex_unadj, est_agesex_unadj[, c("n_imp", "stratum")], 
      function(xx) {return(c(unique(xx$n_imp), unique(xx$stratum), 
        apply(xx[which(xx$eligible), c("list1","list2","list3","listed", 
          "unlisted_est","unlisted_lci","unlisted_uci")], 
        2, weighted.mean, w = xx$post_prob)))
    })
    x <- as.data.frame(do.call(rbind, x))
    colnames(x)[1:2] <- c("n_imp", "stratum")
    x[, grep("list", colnames(x))] <- apply(x[, grep("list", colnames(x))], 2,
      as.numeric)
    x[, c("unlisted_est","unlisted_lci","unlisted_uci")] <- 
      apply(x[, c("unlisted_est","unlisted_lci","unlisted_uci")], 2, 
        function(xx) {round(as.numeric(xx), 0)})
    ave_agesex_unadj <- x
        
    # Month of death stratification
    x <- by(est_month_death, est_month_death[, c("n_imp", "stratum")], 
      function(xx) {return(c(unique(xx$n_imp), unique(xx$stratum), 
        apply(xx[which(xx$eligible), c("list1","list2","list3","listed", 
          "unlisted_est","unlisted_lci","unlisted_uci")], 
        2, weighted.mean, w = xx$post_prob)))
    })
    x <- as.data.frame(do.call(rbind, x))
    colnames(x)[1:2] <- c("n_imp", "stratum")
    x[, grep("list", colnames(x))] <- apply(x[, grep("list", colnames(x))], 2,
      as.numeric)
    x[, c("unlisted_est","unlisted_lci","unlisted_uci")] <- 
      apply(x[, c("unlisted_est","unlisted_lci","unlisted_uci")], 2, 
        function(xx) {round(as.numeric(xx), 0)})
    ave_month_death <- x

    # Compute totals (listed + unlisted)
    x <- paste("total", stats, sep = "_")
    ave_agesex[, x] <- ave_agesex$listed + 
      ave_agesex[, paste("unlisted", stats, sep = "_")]
    ave_agesex_unadj[, x] <- ave_agesex_unadj$listed + 
      ave_agesex_unadj[, paste("unlisted", stats, sep = "_")]
    ave_month_death[, x] <- ave_month_death$listed + 
      ave_month_death[, paste("unlisted", stats, sep = "_")]
    
  #...................................      
  ## Compute and save median and confidence intervals for different desired
        # stratifications of mortality and list sensitivity

    # Which variables to aggregate
    vars <- c("list1","list2","list3","listed",
      paste("unlisted", stats, sep = "_"),
      paste("total", stats, sep = "_"))
    
    # Mortality by age and sex
    ave_agesex[, c("age_cat", "gender")] <- 
      t(sapply(strsplit(as.character(ave_agesex$stratum), "_"), rbind))
    x <- aggregate(ave_agesex[, vars], 
      by = ave_agesex[, c("age_cat", "gender")], median)
    write.csv(x, paste0(dir_path, "output/est_by_age_sex.csv"), row.names = F)
    
    # Mortality by age and sex (unadjusted)
    ave_agesex_unadj[, c("age_cat", "gender")] <- 
      t(sapply(strsplit(as.character(ave_agesex_unadj$stratum), "_"), rbind))
    x <- aggregate(ave_agesex_unadj[, vars], 
      by = ave_agesex_unadj[, c("age_cat", "gender")], median)
    write.csv(x, paste0(dir_path,"output/est_by_age_sex_unadj.csv"),row.names=F)
    
    # List sensitivity by age and sex
    sens <- x
    for (i in c("list1", "list2", "list3", "listed")) {
      for (j in stats) {
        sens[, paste("sens", i, j, sep = "_")] <- 
          percent(sens[, i] / sens[, paste("total", j, sep = "_")], 0.1)
      }
    }
    write.csv(
      sens[, c("age_cat", "gender", grep("sens", colnames(sens), value = T))],
      paste0(dir_path, "output/list_sens_by_age_sex.csv"), row.names = F)
        
    # Mortality by sex
    x <- aggregate(x[, vars], by = list(gender = x$gender), sum)
    write.csv(x, paste0(dir_path, "output/est_by_sex.csv"), row.names = F)
    
    # Mortality overall
    x <- rbind(vars, colSums(x[, vars]))
    write.csv(x, paste0(dir_path, "output/est_overall.csv"), row.names = F)

    # Sensitivity overall
    sens <- x
    for (i in c("list1", "list2", "list3", "listed")) {
      for (j in stats) {
        sens[paste("sens", i, j, sep = "_")] <- 
          percent(as.numeric(sens[i]) / 
            as.numeric(sens[paste("total", j, sep = "_")]), 0.1)
      }
    }
    x <- data.frame(variable = grep("sens", names(sens), value = T),
      value = sens[grep("sens", names(sens), value = T)])
    write.csv(x,paste0(dir_path, "output/list_sens_overall.csv"), row.names = F)
    
    # Mortality by month
    x <- aggregate(ave_month_death[, vars], 
      by = list(month_death = ave_month_death$stratum), median)
    write.csv(x, paste0(dir_path, "output/est_by_month.csv"), row.names = F)

    # Sensitivity by month
    sens <- x
    for (i in c("list1", "list2", "list3", "listed")) {
      for (j in stats) {
        sens[, paste("sens", i, j, sep = "_")] <- 
          percent(sens[, i] / sens[, paste("total", j, sep = "_")], 0.1)
      }
    }
    write.csv(
      sens[, c("month_death", grep("sens", colnames(sens), value = T))],
      paste0(dir_path, "output/list_sens_by_month.csv"), row.names = F)

  #...................................      
  ## Save imputed datasets for each age-sex stratum (for RShiny app model
      # comparison; here, take mode of imputations as imputed value)
    
    # Take mode of imputed value
    x <- long[, c(".imp", "id", "list1", "list2", "list3", "agesex")]
    df <- x[which(x$.imp == 1), c("id", "list1", "list2", "list3")]
    x <- aggregate(list(agesex = x$agesex), by = list(id = x$id),
      function(xx) {
        x1 <- table(xx)
        return(names(x1)[which.max(x1)])}
    )
    df <- merge(df, x, by = "id")

    # Create new directory
    dir.create(paste0(dir_path, "for_rshiny"))
    
    # Split into strata, aggregate and save
    for (i in unique(df$agesex)) {
      x <- df[which(df$agesex == i), c("list1", "list2", "list3")]
      x <- as.data.frame(table(x[, c("list1", "list2", "list3")]))
      x <- apply(x, 2, as.numeric)
      x <- x[which(rowSums(x[, c("list1", "list2", "list3")]) > 0), ]
      colnames(x) <- c(list_names$list_name, i)
      write.csv(x, paste0(dir_path, "for_rshiny/", i, ".csv"), row.names = F)      
    }
        
#...............................................................................
### ENDS
#...............................................................................
    
