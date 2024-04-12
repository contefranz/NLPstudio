if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "jcol", "item", "fyear", ".", "filing_date",
                             "period_of_report", "accession_number",
                             "text", "year_filed", "date_filed", "fyear_end", "sic") )
}
#' Convert JSON to data.frame
#'
#' Convenient function to covert a container list of JSON files to data.frame structures.
#'
#' @param json_list A list of JSON files as built by \code{\link{get_json_files}}.
#' @param ncores The number of cores to assign to \code{\link[parallel]{makeCluster}}. Default to 1.
#'
#' @returns A single \code{data.table} containing
#' several identification columns in addition to the document itself.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @import data.table foreach quanteda
#' @importFrom stringr str_c str_which str_replace str_extract
#' @importFrom jsonlite fromJSON
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom iterators iter
#' @importFrom cli cli_h1 cli_h2 cli_alert cli_alert_success cli_alert_danger
#' @export

from_json_to_df = function(json_list, ncores = 1) {

  followup = str_extract(names(json_list), "\\d+")
  big_bucket = vector("list", length(followup))
  names(big_bucket) = str_c("fyear_", followup)

  cli_h1("Flattening JSONs")
  for ( i_year in seq_along(json_list) ) {

    current_year = str_extract(names(json_list)[i_year], "\\d+")
    cli_h2("Processing batch {current_year}")
    current_list = json_list[[i_year]]

    cli_alert("Reading JSON files")
    temp = lapply(current_list, fromJSON)
    cli_alert("Converting to data.table")
    df = lapply(temp, as.data.table)

    # TO DO --- THIS CODE MIGHT BE REMOVED IN FUTURE RELEASES
    # the following if-statement checks the location of column "item_1" which marks the starting
    # point of the textual data. This is due to some observations missing the column
    # "state_of_inc".
    # In some cases, the wording might be off and instead of "item" it's "section". That's why
    # I included an OR in the regex.
    id_col_item1 = sapply(df, function(x) str_which(names(x), "item_1\\b|section_1\\b")) - 1L
    # TO DO --- THIS CODE MIGHT BE REMOVED IN FUTURE RELEASES

    # This internal loop is parallel because it represents the bottleneck in this function.
    # Check the core assignment as it is not super efficient at the moment.
    cli_alert("Fixing column names and melting")
    n_df = length(df)
    it = iter(seq_len(n_df), by = "row")
    cl = makeCluster(ncores)
    registerDoParallel(cl)
    df_melt = foreach(
      jcol = it,
      .packages = c("iterators", "data.table", "stringr")
    ) %dopar% {
      columns_to_fix = 1L:id_col_item1[jcol]
      old_col_names = names(df[[jcol]])[columns_to_fix]
      melt(df[[jcol]],
           id.vars = old_col_names,
           variable.name = "item",
           value.name = "text")
    }
    stopCluster(cl)

    cli_alert("Binding into one data.table")
    out = rbindlist(df_melt, fill = TRUE)

    cli_alert("Casting correct column classes and creating accession_number")
    # convert cik to integer
    out[ , cik := as.integer(cik)]
    # convert sic to integer
    out[ str_detect(sic, "\\D"), sic := NA_character_]
    out[ , sic := as.integer(sic)]
    # convert date_filed and fyear_end to IDat
    out[ , `:=` (filing_date = as.IDate(filing_date),
                 period_of_report = as.IDate(period_of_report))]
    out[ , fyear := year(period_of_report)]
    # This check is to avoid any late filers and to keep everything as consistent as possible
    out[ , year_filed := year(filing_date)]
    out = out[ year_filed <= fyear + 1L ]
    out[ , year_filed := NULL]
    # move the column fyear where it belongs
    setcolorder(out, neworder = "fyear", after = "period_of_report")
    # extract the accession number useful for checking duplicate observations and to trace
    # back the filing on SEC EDGAR.
    out[ , accession_number := str_extract(filename, "\\d{10}-\\d{2}-\\d{6}")]
    setcolorder(out, neworder = "accession_number", after = "filename")

    # collect output and put it in the final list
    big_bucket[[ i_year ]] = out
    rm(out)

  }

  # this way of returning is not ideal because if one processes a long time series, you saturate
  # the RAM at one point.
  # SOLUTION: add a parameter that controls whether one wants to save to disk
  # the data.table at each iteration. This would require a path out and a potential naming convention.
  # On the latter, I much prefer to impose our internal naming convention like: filingtype_fyear_df.rds
  #
  bind_bucket = rbindlist(big_bucket)
  cli_alert_success("Conversion has been successful")
  return(bind_bucket[])
}

