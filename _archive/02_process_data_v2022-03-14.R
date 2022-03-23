#
# 01_process_data_v2022-03-14.R 
#
# March 2022
#

library(lubridate)
library(dplyr)
library(arrow)



rm(list = ls())

process_data_version_id <- "v2022-03-14"
code_dir <- "/mnt/workspace/GWTG/COVID19/ihme_code/thrombosis/aha_covid_anticoagulation/"
write_processed_data <- 0


inputdata_config_path <- c(
  "/mnt/workspace/GWTG/COVID19/ihme_diagnostics/thrombosis/working_dirs/v2022-03-14/config.RDS"
)

config1 <- readRDS(inputdata_config_path)
datapath <- file.path(config1$working_dir, "orig_data")
specpath <- file.path(config1$working_dir, "variable_specs")


config2 <- config1
config2$process_data_version_id <- process_data_version_id
process_data_dir <- file.path(config1$dir_tmpdata, config2$process_data_version_id, "processed_data")
config2$process_data_dir <- process_data_dir

if (!dir.exists(process_data_dir)) dir.create(process_data_dir, recursive = TRUE)
saveRDS(object = config2, file = file.path(process_data_dir, "config2.RDS"))


if (0) {
  # reload input data?
  cmd <- paste(
    paste0("/opt/R/4.0.2/lib/R/bin/Rscript ", file.path(code_dir, "01_load_data_v2022-03-14.R")),
    paste0("--code_dir ", code_dir), 
    paste0("--write_data ", 0),
    paste0("--write_specs ", 1)
  )
  system(cmd)
}


#####
# process data

df_in <- read_parquet(file.path(datapath, "sas_inputdata.parquet"))

if (0) {
  # check whether certain variables are in the original dataset
  var_tmp <- toupper("subqufhdt")
  var_tmp %in% names(df_in)
  
  vars2 <- names(df_in)[grepl(var_tmp, names(df_in))]
  vars2
}


df_vars <- read_parquet(file.path(specpath, "vars.parquet"))
df_newvars <- read_parquet(file.path(specpath, "newvars.parquet"))

analytic_vars <- as.character(unique(df_vars$orig_var))
missing_analytic_vars <- analytic_vars[!analytic_vars %in% names(df_in)]
analytic_vars2 <- analytic_vars[analytic_vars %in% names(df_in)]

df1 <- as.data.frame(df_in)[, analytic_vars2]

df_vars2 <- df_vars %>% 
  filter(orig_var %in% analytic_vars2)

new_analytic_vars <- unique(df_vars2$new_var)

# read in custom functions
source(file.path(code_dir, "99_analytic_functions.R"))


# apply the recoding specified by the github file
df2 <- recode_variables(
  dat = df1,
  dat_new_variables = df_vars2,
  varnames = new_analytic_vars ) %>%
  mutate(pid = paste0(facility_display_id, "__", patient_display_id))

# for patients with multiple visits, track information about when and how many
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
  mutate(row_id = 1:nrow(.)) %>%
  left_join(df_patients[, c("pid", "has_duplicate_adm_dates")]) %>%
  arrange(pid, admission_date) %>%
  group_by(pid) %>%
  mutate(is_earliest_admission_date = admission_date == min(admission_date)) %>%
  as.data.frame(.)


# make new variables as specified by the github file

# -- subset to non-blank rows
df_newvars2 <- df_newvars %>%
  filter(new_variable_name != "")

if (0) {
  s
}

df4_tmp <- make_new_variables(dat = df3, dat_new_variables = df_newvars2)



# NOTE:
# 15 subjects had PE during the hospitalization, with no history of PE, but there wasn't a diagnosis date; kept these in
df4 <- df4_tmp %>%
  filter(!has_duplicate_adm_dates) %>%
  filter(is_earliest_admission_date) %>%
  filter(!meets_exclusion_criteria %in% 1)


if (write_processed_data) {
  lapply(list(df1, df2, df4_tmp, df4), function(x) {nrow(x)})
  
  output_dir <- config2$process_data_dir
  
  write_parquet(df1, file.path(output_dir, "df1.parquet"))
  write_parquet(df2, file.path(output_dir, "df2.parquet"))
  write_parquet(df4_tmp, file.path(output_dir, "df4_tmp.parquet"))
  write_parquet(df4, file.path(output_dir, "df4.parquet"))
}





