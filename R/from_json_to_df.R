if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c( "jcol", "item", "fyear", ".", "col1",
                             "col2", "col4", "col5", "col6", "col10", "col11", "col12",
                             "text", "year_filed", "date_filed", "fyear_end", "sic") )
}
#' Convert JSON to data.frame
#'
#' Convenient function to covert a container list of JSON files to data.frame structures.
#'
#' @param json_list A list of JSON files as built by \code{\link{get_json_files}}.
#' @param ncores The number of cores to assign to \code{\link[parallel]{makeCluster}}. Default to 1.
#'
#' @return A list of data.table where each element represents a fiscal year. Each data.table contains
#' several identification columns in addition to the document itself.
#'
#' @author Francesco Grossetti \email{francesco.grossetti@@unibocconi.it}
#'
#' @import data.table foreach quanteda
#' @importFrom stringr str_which str_replace
#' @importFrom jsonlite fromJSON
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom iterators iter
#' @export

from_json_to_df = function(json_list, ncores = 1) {

  followup = str_extract(names(json_list), "\\d+")
  big_bucket = vector("list", length(followup))
  names(big_bucket) = str_c("fyear_", followup)

  for ( i_year in seq_along(json_list) ) {

    current_year = str_extract(names(json_list)[i_year], "\\d+")
    message("# # # Processing batch ", current_year, " # # #")
    current_list = json_list[[i_year]]

    message("Reading JSON files")
    temp = lapply(current_list, fromJSON)
    message("Converting to data.table")
    df = lapply(temp, as.data.table)

    # TO DO --- THIS CODE MIGHT BE REMOVED IN FUTURE RELEASES
    # the following if-statement checks whether the filings are from 2006.
    # This is because some columns were missing during the initial retrieval.
    if ( current_year == "2006" ) {
      id_col_item1 = sapply(df, function(x) str_which(names(x), "section_1\\b")) - 1L
    } else {
      id_col_item1 = sapply(df, function(x) str_which(names(x), "item_1\\b")) - 1L
    }
    # TO DO --- THIS CODE MIGHT BE REMOVED IN FUTURE RELEASES

    # This internal loop is parallel because it represents the bottleneck in this function.
    # Check the core assignment as it is not super efficient at the moment.
    message("Fixing column names and melting")
    n_df = length(df)
    it = iter(seq_len(n_df), by = "row")
    cl = makeCluster(ncores)
    registerDoParallel(cl)
    df_melt = foreach(
      jcol = it,
      .packages = c("iterators", "data.table", "stringr")
    ) %dopar% {
      columns_to_fix = 1L:id_col_item1[jcol]
      columns_fixed = str_c("col", columns_to_fix)
      setnames(df[[jcol]], columns_to_fix, columns_fixed)
      melt(df[[jcol]],
           id.vars = columns_fixed,
           variable.name = "item",
           value.name = "text")
    }
    stopCluster(cl)

    message("Binding into one data.table")
    out = rbindlist(df_melt, fill = TRUE)
    if ( current_year == "2006" ) {
      setnames(out, 1L:3L, c("filename", "cik", "fyear"))
      out[ , item := str_replace(item, "section", "item")]
    } else {
      out[ , fyear := as.integer(current_year)]
      out = out[ , .(col1, col2, fyear, col4, col5, col6, item, col10, col11, col12, text)]
      setnames(out, new = c("cik", "cname", "fyear", "date_filed", "fyear_end", "sic", "item",
                            "filing_detail", "filing_html", "filing_txt", "text"))
      # This check is to avoid any late filers and to keep everything as consistent as possible
      out[ , year_filed := year(date_filed)]
      out = out[ year_filed <= as.integer(current_year) + 1L ]
      out[ , year_filed := NULL]
      # convert date_filed and fyear_end to IDat
      out[ , `:=` (date_filed = as.IDate(date_filed),
                   fyear_end = as.IDate(fyear_end))]
      # convert cik to integer
      out[ , cik := as.integer(cik)]
      # convert sic to integer
      out[ str_detect(sic, "\\D"), sic := NA_character_]
      out[ , sic := as.integer(sic)]
    }

    big_bucket[[ i_year ]] = out
    rm(out)

    # cat("Saving\n")a
    # saveRDS(out, str_c("Data/10K_JSON_splits_df/", "AR_", current_year, "_byitem_df.rds"))
    # rm(list = c("current_list", "json_list", "df", "df_melt", "out"))
    # cat("Done!\n")
  }

  # this way of returning is not ideal because if one processes a long time series, you saturate
  # the RAM at one point.
  # SOLUTION: add a parameter that controls whether one wants to save to disk
  # the data.table at each iteration. This would require a path out and a potential naming convention.
  # On the latter, I much prefer to impose our internal naming convention like: filingtype_fyear_df.rds
  #
  return(big_bucket)

}

