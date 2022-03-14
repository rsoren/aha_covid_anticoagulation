#
# rsoren_checking_v4_dataset.R
#
# August 2021
#

library(haven)
library(lubridate)
library(openxlsx)
library(ggplot2)
library(dplyr)

output_dir <- "/mnt/workspace/GWTG/COVID19/ihme_diagnostics/thrombosis/tmp_outputs/"

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

# read in custom functions
source("/mnt/workspace/GWTG/COVID19/ihme_code/thrombosis/aha_covid_anticoagulation/99_functions.R")

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



df4_tmp <- make_new_variables(dat = df3, dat_new_variables = df_newvars2)

if (0) {
  # latest_lowdose_date_beforehe
  # 
  definintion_tmp <- "max(lowdose_date1_beforehe, lowdose_date2_beforehe, na.rm = TRUE)"
  definition_tmp2 <- "select(lowdose_date1_beforehe, lowdose_date2_beforehe) %>% apply(., 2, function(x) max(x, na.rm = TRUE))"
  
  system.time({
    df_tmp <- df4_tmp %>%
      rowwise(.) %>%
      # .[1:1000, ] %>%
      mutate(
        tmpvar = eval(parse(text = definition_tmp)) ) %>%
      as.data.frame(.)
  })
  
  system.time({
    df_tmp <- df4_tmp %>%
      select(lowdose_date1_beforehe, lowdose_date2_beforehe) %>%
      as.data.frame(.)
    
    df_tmp[, "tmpvar2"] <- apply(df_tmp, 1, function(x) max(x, na.rm = TRUE))
  })
  
  
}



# NOTE:
# 15 subjects had PE during the hospitalization, with no history of PE, but there wasn't a diagnosis date; kept these in
df4 <- df4_tmp %>%
  filter(!has_duplicate_adm_dates) %>%
  filter(is_earliest_admission_date) %>%
  filter(!meets_exclusion_criteria %in% 1)

if (FALSE) {
  lapply(list(df1, df2, df4_tmp, df4), function(x) {nrow(x)})
  
  saveRDS(df1, file.path(output_dir, "df1.RDS"))
  saveRDS(df2, file.path(output_dir, "df2.RDS"))
  saveRDS(df4_tmp, file.path(output_dir, "df4_tmp.RDS"))
  saveRDS(df4, file.path(output_dir, "df4.RDS"))
}




# df_tmp <- df4 %>%
#   filter(is.na(meets_exclusion_criteria))
  


# summarize_categorical(
#   dat = df_in,
#   variable_tmp = "MEDHISTO_01",
#   label_tmp = "Example label",
#   notes_tmp = "Example notes",
#   condition_tmp = ""
# )


# names(df_in)[names(df_in) %like% "ANTCOG"]
# 
# hist(as_date(df_in$ADMDT), breaks = "months", main = "Date of admission", xlab = "Date")
# max(as_date(df_in$ADMDT))
# 
# sheets1 <- openxlsx::getSheetNames(documentation)
# df_codebook_in <- openxlsx::read.xlsx(documentation, sheet = sheets1[1])
# df_codebook <- df_codebook_in %>%
#   mutate(label = "", type = "", notes = "")
# 
# df_hist <- df_in %>%
#   select(ADMDT) %>%
#   mutate(
#     date_tmp = as_date(ADMDT),
#     month_tmp = month(date_tmp) ) %>%
#   group_by()
# 
# summarize_categorical <- function(dat, variable_tmp, label_tmp, notes_tmp, condition_tmp = NA) {
#   if (FALSE) {
#     dat <- df2
#     # condition_tmp <- "Species != 'versicolor'"
#     condition_tmp <- "Species != 'versicolor' | is.na(Species)"
#     # condition_tmp <- NA
#     variable_tmp <- "Species"
#     label_tmp <- "tmp label"
#     notes_tmp <- "tmp notes"
#   }
#   require(dplyr)
#   if (!is.na(condition_tmp)) dat <- filter(dat, eval(parse(text = condition_tmp)))
#   tbl_tmp <- table(as.data.frame(dat)[, as.character(variable_tmp)], useNA = "ifany")
#   df_out <- as.data.frame(tbl_tmp) %>%
#     mutate(variable = variable_tmp, label = label_tmp, notes = notes_tmp, condition = condition_tmp) %>%
#     select(variable, level = Var1, count = Freq, condition, label, notes)
#   return(df_out)
# }
# 
# summarize_numeric <- function(dat, variable_tmp, label_tmp, notes_tmp, condition_tmp = NA) {
#   if (FALSE) {
#     dat <- df2
#     # condition_tmp <- "Species != 'versicolor'"
#     condition_tmp <- "Species != 'versicolor' | is.na(Species)"
#     # condition_tmp <- NA
#     variable_tmp <- "Species"
#     label_tmp <- "tmp label"
#     notes_tmp <- "tmp notes"
#   }
#   require(dplyr)
#   if (!is.na(condition_tmp)) dat <- filter(dat, eval(parse(text = condition_tmp)))
#   tbl_tmp <- table(as.data.frame(dat)[, as.character(variable_tmp)], useNA = "ifany")
#   df_out <- as.data.frame(tbl_tmp) %>%
#     mutate(variable = variable_tmp, label = label_tmp, notes = notes_tmp, condition = condition_tmp) %>%
#     select(variable, level = Var1, count = Freq, condition, label, notes)
#   return(df_out)
# }
# 

