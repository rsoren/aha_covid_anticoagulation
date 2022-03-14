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
      newvar <- "sex_male"
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
  df_out2 <- cbind(dat, df_out)
  return(df_out2)
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



make_table_cont <- function(dat, df_labels, type_variable, group_var) {
  if (FALSE) {
    dat <- df4
    df_labels <- df_tmp
    type_variable <- "type2"
    group_var <- "highest_dose_beforeicu"
  }
  
  df_labels_cont <- df_labels %>%
    mutate(type_variable_tmp = eval(parse(text = type_variable))) %>%
    filter(type_variable_tmp %in% c("Continuous", "Integer"))
  
  dat_cont <- lapply(1:nrow(df_labels_cont), function(i) {
    if (FALSE) {
      i <- 1
    }
    varname_i <- df_labels_cont[i, "variable_name"]
    dat_tmp <- dat 
    dat_tmp$vartmp <- dat_tmp[, varname_i]
    dat_tmp2 <- dat_tmp %>%
      group_by(eval(parse(text = group_var))) %>%
      summarize(
        mean_tmp = mean(vartmp, na.rm = TRUE),
        sd_tmp = sd(vartmp, na.rm = TRUE),
        median_tmp = quantile(vartmp, probs = 0.5, na.rm = TRUE),
        first_quartile = quantile(vartmp, probs = 0.25, na.rm = TRUE),
        third_quartile = quantile(vartmp, probs = 0.75, na.rm = TRUE),
        n_nonmissing = n() - sum(is.na(vartmp)),
        n_missing = sum(is.na(vartmp)) ) %>%
      as.data.frame(.) %>%
      mutate(
        var_name = varname_i,
        var_label = df_labels_cont[i, "label"]
      )
    return(dat_tmp2)
  })
  
  df_cont <- bind_rows(dat_cont)
  return(df_cont)
}



make_table_binary <- function(dat, df_labels, type_variable, group_var) {
  if (FALSE) {
    dat <- df4
    df_labels <- df_tmp
    type_variable <- "type2"
    group_var <- "highest_dose_beforeicu"
  }
  
  df_labels_cat <- df_labels %>%
    mutate(type_variable_tmp = eval(parse(text = type_variable))) %>%
    filter(type_variable_tmp %in% c("Binary"))
  
  dat_cat <- lapply(1:nrow(df_labels_cat), function(i) {
    if (FALSE) {
      i <- 1
    }
    varname_i <- df_labels_cat[i, "variable_name"]
    dat_tmp <- as.data.frame(dat)
    dat_tmp$vartmp <- dat_tmp[, varname_i]
    dat_tmp2 <- dat_tmp %>%
      group_by(eval(parse(text = group_var))) %>%
      # filter(highest_dose_beforeicu == "Full") %>%
      summarize(
        n_nonmissing = n() - sum(is.na(vartmp)),
        n_total = n(),
        prop_among_all = sum(vartmp == 1, na.rm = TRUE) / n_total,
        prop_among_nonmissing = sum(vartmp == 1, na.rm = TRUE) / n_nonmissing ) %>%
      as.data.frame(.) %>%
      mutate(
        var_name = varname_i,
        var_label = df_labels_cat[i, "label"]
      )
    return(dat_tmp2)
  })
  
  df_cat <- bind_rows(dat_cat)
  return(df_cat)
}



process_table_cont <- function(dat) {
  if (FALSE) {
    dat <- df_out_cont
  }
  
  groups <- c("None", "Low", "Intermediate", "Full")
  
  dat_list <- lapply(groups, function(x) {
    if (FALSE) {
      x <- "None"
    }
    dat_tmp <- dat[dat[,1] == x, ] %>%
      mutate(
        # mean_tmp2 = signif(mean_tmp, digits = 3),
        # mean_tmp3 = sprintf(mean_tmp, fmt = '%#.3f')
        mean_tmp2 = round(mean_tmp, 2),
        sd_tmp2 = round(sd_tmp, 2),
        median_tmp2 = round(median_tmp, 2),
        first_quartile2 = round(first_quartile, 2),
        third_quartile2 = round(third_quartile, 2),
        levelvar = paste0(median_tmp2, " (", first_quartile2, ", ", third_quartile2, ")")) %>%
      select(var_label, levelvar, n_nonmissing)
    # mutate(groupvar = x)
  })
  
  df_list <- purrr::reduce(dat_list, left_join, by = "var_label")
  
  return(df_list)
}




process_table_binary <- function(dat) {
  if (FALSE) {
    dat <- df_out_binary
  }
  
  groups <- c("None", "Low", "Intermediate", "Full")
  
  dat_list <- lapply(groups, function(x) {
    if (FALSE) {
      x <- "None"
    }
    dat_tmp <- dat[dat[,1] == x, ] %>%
      mutate(
        percent = round(prop_among_all * 100, 1),
        count = n_total ) %>%
      select(var_label, percent, count)
    # mutate(groupvar = x)
  })
  
  df_list <- purrr::reduce(dat_list, left_join, by = "var_label")
  
  return(df_list)
}

process_table_binary2 <- function(dat) {
  if (FALSE) {
    dat <- df_out_binary
  }
  
  groups <- c("None", "Low", "Intermediate", "Full")
  
  dat_list <- lapply(groups, function(x) {
    if (FALSE) {
      x <- "None"
    }
    dat_tmp <- dat[dat[,1] == x, ] %>%
      mutate(
        percent = round(prop_among_nonmissing * 100, 1),
        count = n_nonmissing ) %>%
      select(var_label, percent, count)
    # mutate(groupvar = x)
  })
  
  df_list <- purrr::reduce(dat_list, left_join, by = "var_label")
  
  return(df_list)
}





anticoag_table_continuous <- function(dat, df_labels, type_variable, group_var) {
  if (0) {
    dat = df4
    df_labels = df_tmp
    type_variable = "type2"
    group_var = "highest_dose_beforeicu"
  }
  df_out_cont <- make_table_cont(
    dat = dat,
    df_labels = df_labels,
    type_variable = type_variable,
    group_var = group_var
  )
  
  df_table_cont <- process_table_cont(df_out_cont)
  names(df_table_cont) <- c(" ", "Median (IQR)", "n", "Median (IQR)", "n", "Median (IQR)", "n", "Median (IQR)", "n")
  
  kbl(df_table_cont) %>%
    kable_classic() %>%
    add_header_above(c(" " = 1, "None" = 2, "Low" = 2, "Intermediate" = 2, "Full" = 2))
  
}





anticoag_table_binary <- function(dat, df_labels, type_variable, group_var, nonmissing_denom = FALSE) {
  if (0) {
    dat = df4
    df_labels = df_tmp
    type_variable = "type2"
    group_var = "highest_dose_beforeicu"
    nonmissing_denom = FALSE
  }
  
  df_out_binary <- make_table_binary(
    dat = dat,
    df_labels = df_labels,
    type_variable = type_variable,
    group_var = group_var
  )
  
  if (nonmissing_denom) {
    df_table_binary <- process_table_binary(df_out_binary)
  } else {
    df_table_binary <- process_table_binary2(df_out_binary)
  }
  
  names(df_table_binary) <- c(" ", "Percent", "n", "Percent", "n", "Percent", "n", "Percent", "n")
  
  kbl(df_table_binary) %>%
    kable_classic() %>%
    add_header_above(c(" " = 1, "None" = 2, "Low" = 2, "Intermediate" = 2, "Full" = 2))
  
  
}







