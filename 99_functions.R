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




#####

make_new_variables <- function(dat, dat_new_variables) {
  if (FALSE) {
    dat = df3
    dat_new_variables = df_newvars2
  }
  
  dat <- as.data.frame(dat)
  dat_new_variables <- as.data.frame(dat_new_variables)
  
  if (any(dat$new_variable_name %in% c(NA, ""))) {
    stop("'new_variable_name' cannot having missing or blank values")
  }
  # for (x in unique(dat_new_variables$new_variable_name)) {
  for (i in 1:nrow(dat_new_variables)) {
    if (FALSE) {
      i <- 30
    }
    x <- dat_new_variables[i, "new_variable_name"]
    try({
      definition_tmp <- as.character(dat_new_variables[i, "definition"])
      cat(paste0("\n", x, " = ", definition_tmp, "\n"))
      
      if (dat_new_variables[i, "rowwise"] == 1) {
        dat <- dat %>% rowwise(.)
      }
      dat <- dat %>%
        mutate(
          tmpvar = eval(parse(text = definition_tmp)) ) %>%
        as.data.frame(.)
      
      dat[, x] <- dat$tmpvar
    })
  }
  
  return(dat)
}



summarize_categorical <- function(dat, variable_tmp, label_tmp, notes_tmp, condition_tmp = NA) {
  if (FALSE) {
    dat <- df_in
    # condition_tmp <- "Species != 'versicolor'"
    condition_tmp <- "Species != 'versicolor' | is.na(Species)"
    # condition_tmp <- NA
    variable_tmp <- "Species"
    label_tmp <- "tmp label"
    notes_tmp <- "tmp notes"
  }
  require(dplyr)
  if (!is.na(condition_tmp)) dat <- filter(dat, eval(parse(text = condition_tmp)))
  tbl_tmp <- table(as.data.frame(dat)[, as.character(variable_tmp)], useNA = "ifany")
  df_out <- as.data.frame(tbl_tmp) %>%
    mutate(variable = variable_tmp, label = label_tmp, notes = notes_tmp, condition = condition_tmp) %>%
    select(variable, level = Var1, count = Freq, condition, label, notes)
  return(df_out)
}





