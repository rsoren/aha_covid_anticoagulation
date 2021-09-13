# 
# functions.R
#
# Reed Sorensen
# September 2021
#




recode_variables <- function(dat, dat_new_variables, varnames) {
  if (FALSE) {
    dat <- df1
    dat_new_variables <- df_vars2
    varnames <- new_analytic_vars
  }
  
  newdat_tmp <- sapply(new_analytic_vars, function(newvar) {
    if (FALSE) {
      # newvar <- "sex_male"
      # newvar <- "facility_display_id"
      # newvar <- "medhist_afib"
      newvar <- "admission_ddimer_units"
    }
    cat(newvar, "\n")
    require(dplyr)
    require(lubridate)
    
    dat <- as.data.frame(dat)
    dat_new_variables <- as.data.frame(dat_new_variables)
    dat_newvar_tmp <- dat_new_variables %>% filter(new_var == newvar)
    orig_var_tmp <- unique(dat_newvar_tmp$orig_var)
    
    if (length(orig_var_tmp) != 1) stop(paste0("New variable '", var_tmp, "' has more than 1 original variable specified"))
    if (length(unique(dat_newvar_tmp$type)) != 1) stop(paste0("New variable '", var_tmp, "' has more than 1 variable type specified"))
    
    dat[, "new__variable"] <- NA
    if (unique(dat_newvar_tmp$type) != "Date" & is.na(unique(dat_newvar_tmp$orig_level)[1]) ) {
      cat(1, "\n")
      dat$new__variable <- dat[, orig_var_tmp]
    } else if (unique(dat_newvar_tmp$type) %in% c("Binary", "Categorical", "Unit label")) {
      cat(2, "\n")
      for (row in 1:nrow(dat_newvar_tmp)) {
        indices_tmp <- dat[, orig_var_tmp] %in% dat_newvar_tmp[row, "orig_level"]
        dat[indices_tmp, "new__variable"] <- dat_newvar_tmp[row, "new_level"]
      }
    } else if (unique(dat_newvar_tmp$type) == "Date") {
      cat(3, "\n")
      dat$new__variable <- lubridate::as_date(dat[, orig_var_tmp])
    } else {
      cat(4, "\n")
      stop(paste0("Variable '", newvar, "' does not conform to input requirements"))
    }
    dat$new__variable
    
  }, simplify = FALSE)
  
  df_out <- bind_cols(newdat_tmp)
  return(df_out)
}

