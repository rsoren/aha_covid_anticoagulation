#
# rsoren_checking_v4_dataset.R
#
# August 2021
#

library(haven)
library(lubridate)
library(openxlsx)
library(ggplot2)

# file paths to read in data
dataFile <- "/mnt/workspace/GWTG/COVID19/v4_v4_2021-05/data/v4_2021_05_aha_covid19_cvd.sas7bdat"
formatsFile <- "/mnt/workspace/GWTG/COVID19/v4_v4_2021-05/data/formats.sas7bcat"

aha_doc <- "/mnt/workspace/GWTG/COVID19/v4_v4_2021-05/data/v4_2021_05_covid19_hosp_characteristics.csv"
# documentation <- '/mnt/workspace/GWTG/COVID19/10-2020/limited/documentation/oct20_aha_covid19_cvd_contents_formats.xlsx'
documentation <- '/mnt/workspace/GWTG/COVID19/v4_v4_2021-05/documentation/v4_2021_05_aha_covid19_cvd_data_dictionary.csv'

df_doc <- read.csv(documentation)

df_in <- read_sas(dataFile)


library(RCurl)
url_vars <- "https://raw.githubusercontent.com/rsoren/aha_covid_input_data/main/01_variable_encoding.csv"
url_newvars <- "https://raw.githubusercontent.com/rsoren/aha_covid_input_data/main/02_create_new_variables.csv"
url_summarize_cat <- "https://raw.githubusercontent.com/rsoren/aha_covid_input_data/main/summarize_categorical.csv"

df_vars <- read.csv(text = RCurl::getURL(url_vars), as.is = TRUE)
df_newvars <- read.csv(text = RCurl::getURL(url_newvars), as.is = TRUE)
df_cat <- read.csv(text = RCurl::getURL(url_summarize_cat), as.is = TRUE)


analytic_vars <- as.character(unique(df_vars$orig_var))
missing_analytic_vars <- analytic_vars[!analytic_vars %in% names(df_in)]
analytic_vars2 <- analytic_vars[analytic_vars %in% names(df_in)]

df1 <- as.data.frame(df_in)[, analytic_vars2]

df_vars2 <- df_vars %>% 
  filter(orig_var %in% analytic_vars2)

new_analytic_vars <- unique(df_vars2$new_var)


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
  

df2 <- recode_variables(
  dat = df1,
  dat_new_variables = df_vars2,
  varnames = new_analytic_vars ) %>%
  mutate(pid = paste0(facility_display_id, "__", patient_display_id))
  
df_patients <- df2 %>%
  group_by(pid) %>% 
  summarize(
    n_cases = n(),
    n_unique_adm_dates = length(unique(admission_date)),
    adm_dates = paste(admission_date, collapse = ";")) %>%
  as.data.frame(.) %>%
  arrange(desc(n_cases)) %>%
  mutate(has_duplicate_adm_dates = n_cases != n_unique_adm_dates)

df3 <- df2 %>%
  left_join(df_patients[, c("pid", "has_duplicate_adm_dates")]) %>%
  arrange(pid, admission_date) %>%
  group_by(pid) %>%
  mutate(is_earliest_admission_date = admission_date == min(admission_date)) %>%
  as.data.frame(.)

df_newvars2 <- df_newvars %>%
  filter(new_variable_name != "")


make_new_variables <- function(dat, dat_new_variables) {
  if (FALSE) {
    dat = df3
    dat_new_variables = df_newvars2
  }
  
  dat_new_variables <- as.data.frame(dat_new_variables)
  if (any(dat$new_variable_name %in% c(NA, ""))) {
    stop("'new_variable_name' cannot having missing or blank values")
  }
  for (x in dat_new_variables$new_variable_name) {
    if (FALSE) {
      x <- "has_afib_at_admission"
    }
    definition_tmp <- dat_new_variables[dat_new_variables$new_variable_name == x, "definition"]
    cat(paste0(x, " = ", definition_tmp, "\n"))
    dat <- within(data = dat, expr = assign(x, eval(parse(text = definition_tmp))))
  }

  return(dat)
}

df4_tmp <- make_new_variables(dat = df3, dat_new_variables = df_newvars2)

# NOTE:
# 15 subjects had PE during the hospitalization, with no history of PE, but there wasn't a diagnosis date; kept these in
df4 <- df4_tmp %>%
  filter(!has_duplicate_adm_dates) %>%
  filter(is_earliest_admission_date) %>%
  filter(!meets_exclusion_criteria %in% 1)



df_tmp <- df4 %>%
  filter(is.na(meets_exclusion_criteria))
  
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


# summarize_categorical(
#   dat = df_in,
#   variable_tmp = "MEDHISTO_01",
#   label_tmp = "Example label",
#   notes_tmp = "Example notes",
#   condition_tmp = ""
# )


names(df_in)[names(df_in) %like% "ANTCOG"]

hist(as_date(df_in$ADMDT), breaks = "months", main = "Date of admission", xlab = "Date")
max(as_date(df_in$ADMDT))

sheets1 <- openxlsx::getSheetNames(documentation)
df_codebook_in <- openxlsx::read.xlsx(documentation, sheet = sheets1[1])
df_codebook <- df_codebook_in %>%
  mutate(label = "", type = "", notes = "")

df_hist <- df_in %>%
  select(ADMDT) %>%
  mutate(
    date_tmp = as_date(ADMDT),
    month_tmp = month(date_tmp) ) %>%
  group_by()

summarize_categorical <- function(dat, variable_tmp, label_tmp, notes_tmp, condition_tmp = NA) {
  if (FALSE) {
    dat <- df2
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

summarize_numeric <- function(dat, variable_tmp, label_tmp, notes_tmp, condition_tmp = NA) {
  if (FALSE) {
    dat <- df2
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


