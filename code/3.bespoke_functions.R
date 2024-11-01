#...............................................................................
### +++++ CAPTURE-RECAPTURE ANALYSIS OF MORTALITY DATA - GENERIC CODE ++++++ ###
#...............................................................................

#...............................................................................
## --------- BESPOKE FUNCTIONS TO IMPLEMENT DIFFERENT ANALYSIS TASKS  ------- ##
#...............................................................................


#...............................................................................
### Function to prepare data for 3-list model averaging
    # as per Rossi et al. https://rivista-statistica.unibo.it/article/view/9854 
#...............................................................................

f_prepare <- function(data_f = df, list_names_f = list_names, confounders = NA){
  
  #...................................      
  ## Preparatory steps
      # create unique id for each observation
      data_f$key <- paste0("id", 1:nrow(data_f))
      
      # create indicator variable for outcome (all = 1)
      data_f$y <- 1
      
      # rename list columns
      colnames(data_f) <- gsub("list", "x", colnames(data_f))

      # defactor confounders
      if (! is.na(confounders[1]) ) {
        for (ii in confounders) {
          if (is.factor(data_f[, ii]) ) {
            data_f[, ii] <- as.character(data_f[, ii])
          }
        }
      }
    
  #...................................      
  ## Prepare data for modelling (one row = one observation)
  
    # Prepare possible 'profiles' for each individual observation
      # identify all possible list profiles (cells in contingency table)
      profiles <- as.data.frame(gtools::permutations(n = 2, r = 3, 
        v = c(0, 1), repeats.allowed = T))
      colnames(profiles) <- as.character(1:3)
      
      # columns for which observations appear on which combinations of lists
      x <- gtools::combinations(n = 3, r = 2, v = 1:3, repeats.allowed = F)
      for (ii in 1:nrow(x)) { profiles[, paste(x[ii, ], collapse = "")] <- 
        rowSums(profiles[, x[ii, ]]) }
      profiles[, 4:6] <- apply(profiles[, 4:6], 2, 
        function(x) {ifelse(x == 2, 1, 0)})
      colnames(profiles) <- paste0("x", colnames(profiles))

    # Set profiles for each observation, based on the data
      # create expanded dataframe with all possible profiles for each obs.
      df <- expand_grid(data_f[, "key"], profiles)
      colnames(df) <- c("key", colnames(profiles))
      df <- as.data.frame(df)
      
      # match each observation to possible profiles 
        # (except for which lists the observation is in)
      x <- paste0("x", 1:3)
      df <- merge(df, data_f[, ! colnames(data_f) %in% c(x,"y")], 
        by = "key", all.x = T)
      
      # determine which profile the observation has
      # outcome indicator = 1 if observation falls within a given profile, 
         # 0 otherwise and NA for x000(0) profile)
      df <- merge(df, data_f[, c("key", x, "y")], by = c("key", x), all.x = T)
      df[which(is.na(df$y)), "y"] <- 0
      df[which(rowSums(df[, x]) == 0), "y"] <- NA
    
    # Add columns for interactions between confounder(s) (if present) 
        # and lists (note: omit interactions among confounders)
    if (! is.na(confounders[1])) {
      for (ii in confounders) {df[, paste(colnames(profiles), ii, sep = ":")] <- 
        ifelse(df[, colnames(profiles)] == 1, df[, ii], 0)} 
    }
    
  #...................................      
  ## Define candidate models
  
    # Possible combinations of terms...
    x <- apply(gtools::combinations(3, 2, list_names_f$list_name), 1, paste, 
      collapse = " x " )
    terms <- c()
    for (ii in 3:1) {
      terms <- c(terms, apply(gtools::combinations(length(x), ii, x) , 1, paste, 
        collapse = ", " ) )
    }
    out <- data.frame("model" = c("no interactions", terms) )

    # Derive model formulae from the above
    out$formula <- paste("y ~ ",paste(colnames(profiles[1:3]),collapse = " + "))
    x <- out$model  
    for (ii in 1:nrow(list_names)) {x <- gsub(list_names_f[ii, "list_name"],
      list_names_f[ii, "list"], x)}
    x <- gsub("x", ":", x)
    x <- gsub(", ", " + ", x)
    x <- gsub("list", "x", x)
    out$formula <- paste(out$formula, x, sep = " + ")
    out[1, "formula"] <- paste("y ~ ",paste(colnames(profiles[1:3]),
      collapse = " + "))
    
    # Lastly, add terms for confounders and interactions of these with profiles
    if (! is.na(confounders[1])) {
      for (ii in 1:nrow(out)) {  
        for (jj in confounders) {
          x <- all.vars(as.formula(out[ii, "formula"]))[-1]
          x <- paste(x, jj, sep = ":")
          x <- paste(x, collapse = " + ")
          out[ii, "formula"] <- paste(out[ii, "formula"], jj, x, sep = " + ")
        }
      }
    }

  #...................................      
  ## Return output and dataframe used for model fitting
  x <- list(out, df)
  names(x) <- c("out", "df")
  return(x)
}      



#...............................................................................
### Function to fit all candidate models and compute statistics for each
#...............................................................................

f_model <- function(out_f = out, list_names_f = list_names,
  stratum = NA, confounders = NA, plausibility_f = plausibility, verbose = T) {
  
  #...................................      
  ## Preparatory steps
  
    # Dataframe for model fitting
    df <- out_f[["df"]]
    if (! is.na(stratum)) {df[, stratum] <- factor(df[, stratum],
      levels = sort(unique(df[, stratum])))}
    
      # select data with 000 profile
      df0 <- df[which(is.na(df$y)), ]
      
    # Set up output of model averaging (one set per stratum level)
    x <- out_f[["out"]]
    out_ave <- x[rep(seq_len(nrow(x)), length(levels(df[, stratum]))), ]
    out_ave$stratum <- rep(levels(df[, stratum]), each = nrow(x))
      
      # model eligibility and statistics
      out_ave[, c("eligible", "aic", "aic_delta", "post_prob")] <- NA
      stats <- c("est", "lci", "uci")
      out_ave[, c("list1", "list2", "list3", "listed")] <- NA
      out_ave[, paste("unlisted", stats, sep = "_")] <- NA      

  #...................................      
  ## Fit each model by stratum, compute AIC and predict n unlisted
  for (ii in 1:nrow(out_ave)) {
    
    # Progress
    if (verbose) {print(paste0("fitting model ", ii, " of ", nrow(out_ave)))}
    
    # Which data
    df_ii <- df[which(df[, stratum] == out_ave[ii, "stratum"]), ]
    
    # Fill in listed variables
    x <- df_ii[which(df_ii$y == 1), ]
    out_ave[ii, "list1"] <- sum(x$x1)
    out_ave[ii, "list2"] <- sum(x$x2)
    out_ave[ii, "list3"] <- sum(x$x3)
    out_ave[ii, "listed"] <- nrow(x)
    
    # Fit model      
    fit <- try(glm(formula = as.formula(out_ave[ii, "formula"]), 
      family = poisson(), data = df_ii), silent = T)

    # Check: if model has not fit or any coefficients is NA, skip to next model
    if (class("fit")[1] == "try-error" ) {next}
    if (any(is.na(coef(fit))) ) {next}

    # Compute model's AIC
    out_ave[ii, "aic"] <- round(AIC(fit), digits = 2)
    
    # Predict unlisted cases
      # prediction data
      df0_ii <- df0[which(df0[, stratum] == out_ave[ii, "stratum"]), ]
    
      # contribution to total unlisted for each observation: estimate + 95%CI
      x <- predict(fit, df0_ii, se.fit = T)
      df0_ii$unlisted_est <- exp(x[[1]])
      df0_ii$unlisted_lci <- exp(x[[1]] - 1.96 * x[[2]])
      df0_ii$unlisted_uci <- exp(x[[1]] + 1.96 * x[[2]])

      # unlisted for stratum
      for (jj in stats) {out_ave[ii, paste("unlisted", jj, sep = "_")] <- 
        sum(df0_ii[, paste("unlisted", jj, sep = "_")], na.rm = T)
      }
  }
      
  #...................................      
  ## Select models for averaging    
    # Define eligibility
    out_ave$eligible <- T

    # If any unlisted estimate > K times the number listed, exclude
    x <- which((out_ave$unlisted_est / out_ave$listed) > plausibility_f)
    out_ave[x, "eligible"] <- F

    # If any estimates are NA or "Inf", exclude
    x <- which(is.na(out_ave$unlisted_est) | is.na(out_ave$unlisted_lci) |
      is.na(out_ave$unlisted_uci) | out_ave$unlisted_est == "Inf" |
      out_ave$unlisted_lci == "Inf" | out_ave$unlisted_uci == "Inf")
    out_ave[x, "eligible"] <- F

  #...................................      
  ## Calculate 'posterior probabilities' (weights) of eligible models 
      # from their AICs (based on Rossi, 2010: 
      # https://rivista-statistica.unibo.it/article/view/3593/2945 )
  
  for (ii in levels(df[, stratum])) {
    # Calculate an AIC delta based on lowest one
    x <- which(out_ave$eligible & out_ave$stratum == ii)
    out_ave[x, "aic_delta"] <- out_ave[x, "aic"] - min(out_ave[x, "aic"])
    
    # Then calculate weights / posterior probabilities 
    tot_prob <- sum(exp(- out_ave[x, "aic_delta"] / 2))
    out_ave[x, "post_prob"] <- exp(- out_ave[x, "aic_delta"] / 2) / tot_prob
  }  

  #...................................      
  ## Return output
  return(out_ave)
}

