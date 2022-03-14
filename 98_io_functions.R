#
# 98_structural_functions.R
# 



make_config <- function(
  version_id,
  dir_tmpdata, 
  sas_inputdata=NA,
  sas_formats=NA,
  csv_hospinfo=NA,
  csv_documentation=NA ) {
  
  working_dir <- file.path(dir_tmpdata, version_id)
  if (!dir.exists(working_dir)) dir.create(working_dir, recursive = TRUE)
  as.list(environment())
}





read_datafiles <- function(config) {
  if (0) {
    config <- config1
  }
  
  require(haven)
  require(openxlsx)
  require(data.table)
  config_subset <- config[!is.na(config)]
  
  lastn <- function(x, n) substr(x, nchar(x)-n, nchar(x))
  is_csv <- function(x) grepl("\\.csv", lastn(x=x, n=4))
  is_sas <- function(x) grepl("\\.sas7bdat", lastn(x=x, n=9))
  is_xlsx <- function(x) grepl("\\.xlsx", lastn(x=x, n=9))
  is_notafile <- function(x) !grepl("\\.", x)
  
  dat_out <- sapply(names(config_subset), function(x) {
    if (0) {
      x <- "sas_formats"
    }
    cat(x, "\n")
    try({
      y <- config_subset[[x]]
      if (is_notafile(y)) out <- y
      if (is_csv(y)) out <- as.data.frame(data.table::fread(y))
      if (is_sas(y)) out <- as.data.frame(haven::read_sas(y))
      if (is_xlsx(y)) out <- as.data.frame(openxlsx::read.xlsx(y))
      return(out)
    })
  })
  
  return(dat_out)
}




save_datafiles <- function(dat_list, outdir) {
  if (0) {
    dat_list <- dat1[c("sas_inputdata", "sas_formats", "csv_hospinfo", "csv_documentation")]
    outdir <- file.path(dataconfig1$working_dir, "orig_data")
  }
  
  require(arrow)
  if (!dir.exists(outdir)) dir.create(outdir)
  
  for (x in names(dat_list)) {
    if (0) {
      x <- names(dat_list[1])
    }
    cat(x, "\n")
    if (!is.data.frame(dat_list[[x]])) stop(paste0(x, " is not a data frame"))
    outpath <- file.path(outdir, paste0(x, ".parquet"))
    dat_tmp <- dat_list[[x]]
    arrow::write_parquet(x = dat_tmp, sink = outpath)
  }
  
}






read_specfiles <- function(url_list) {
  if (0) {
    url_list <- list(
      url_vars = "https://raw.githubusercontent.com/rsoren/aha_covid_input_data/main/01_variable_encoding.csv",
      url_newvars = "https://raw.githubusercontent.com/rsoren/aha_covid_input_data/main/02_create_new_variables.csv",
      url_summarize_cat = "https://raw.githubusercontent.com/rsoren/aha_covid_input_data/main/summarize_categorical.csv"
    )
  }
  require(RCurl)
  out <- sapply(url_list, function(x) {
    read.csv(text = RCurl::getURL(x), as.is = TRUE)
  }, simplify = FALSE)
  return(out)
}
