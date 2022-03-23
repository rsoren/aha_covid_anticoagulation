#
# 01_load_data_v2022-03-14.R 
#
# March 2022
#

rm(list = ls())

library(argparse)


#####
# inputs

if (interactive()) {
  
  args <- list(
    code_dir = "/mnt/workspace/GWTG/COVID19/ihme_code/thrombosis/aha_covid_anticoagulation/",
    write_data = 0,
    write_specs = 0
  )
  
} else {
  
  parser <- argparse::ArgumentParser()
  parser$add_argument("--code_dir", required = TRUE, help = "File path to local git repo")
  parser$add_argument("--write_data", type = "integer", default = 0, required = FALSE, help = "Save input data to disk?")
  parser$add_argument("--write_specs", type = "integer", default = 0, required = FALSE, help = "Save specifications file to disk?")
  args <- parser$parse_args()
  
}

list2env(args, envir = globalenv())


#####
# configurations

source(file.path(code_dir, "98_io_functions.R"))

config1 <- make_config(
  version_id = "v2022-03-14",
  dir_tmpdata = "/mnt/workspace/GWTG/COVID19/ihme_diagnostics/thrombosis/working_dirs/", 
  sas_inputdata = "/mnt/workspace/GWTG/COVID19/v4_v4_2021-05/data/v4_2021_05_aha_covid19_cvd.sas7bdat",
  sas_formats = "/mnt/workspace/GWTG/COVID19/v4_v4_2021-05/data/v4_2021_05_aha_covid19_cvd_fmts.sas7bdat",
  csv_hospinfo = "/mnt/workspace/GWTG/COVID19/v4_v4_2021-05/data/v4_2021_05_covid19_hosp_characteristics.csv",
  csv_documentation = "/mnt/workspace/GWTG/COVID19/v4_v4_2021-05/documentation/v4_2021_05_aha_covid19_cvd_data_dictionary.csv"
)



#####
# load and save data

if (write_data) {
  
  
  dat1 <- read_datafiles(config = config1)
  
  # input data
  save_datafiles(
    dat_list = dat1[c("sas_inputdata", "sas_formats", "csv_hospinfo", "csv_documentation")],
    outdir = file.path(config1$working_dir, "orig_data")
  )
  
}


if (write_specs) {
  
  urls <- list(
    vars = "https://raw.githubusercontent.com/rsoren/aha_covid_input_data/main/01_variable_encoding.csv",
    newvars = "https://raw.githubusercontent.com/rsoren/aha_covid_input_data/main/02_create_new_variables.csv",
    table1_vars = "https://raw.githubusercontent.com/rsoren/aha_covid_input_data/main/table1_table2_variable_list.csv",
    table3_vars = "https://raw.githubusercontent.com/rsoren/aha_covid_input_data/main/table3_variable_list.csv"
  )
  
  specs1 <- read_specfiles(url_list = urls)
  
  # specifications
  save_datafiles(
    dat_list = specs1[c("vars", "newvars", "table1_vars", "table3_vars")],
    outdir = file.path(config1$working_dir, "variable_specs")
  )
  
}



# config file
saveRDS(object = config1, file = file.path(config1$working_dir, "config.RDS"))


