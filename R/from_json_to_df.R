#' Convert JSON to data.frame
#'
#' Convenient function to covert a container list of JSON files to data.frame structures.
#'
#' @param json_list A list of JSON files as built by \code{\link{get_json_files}}.
#'
#' @return I don't remember yet...check in later...
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

from_json_to_df = function(json_list) {

  for ( i_year in seq_along(json_list) ) {
    message("# # #")
    message("Processing batch ", names(json_list)[i_year])
    current_list = json_list[[i_year]]

    message("Reading JSON files")
    json_list = lapply(current_list, fromJSON)
    message("Converting to data.table")
    df = lapply(json_list, as.data.table)

    # the following if-statement checks whether the filings are before or after 2006.
    # This is when SEC introduces SOX and mandated to include Item 1A
    # It could be also due to an issue in 2006
    if ( followup[i_year] == "2006" ) {
      id_col_item1 = sapply(df, function(x) str_which(names(x), "section_1\\b")) - 1L
    } else {
      id_col_item1 = sapply(df, function(x) str_which(names(x), "item_1\\b")) - 1L
    }
    n_df = length(df)

    message("Fixing column names and melting")
    it = iter( seq_len(n_df), by = "row" )
    cl = makeCluster( ncores )
    registerDoParallel( cl )
    df_melt = foreach(
      jcol = it,
      .packages = c( "iterators", "data.table", "quanteda", "stringr", "lubridate", "jsonlite" )
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
    if ( followup[i_year] == "2006" ) {
      setnames(out, 1L:3L, c("filename", "cik", "fiscal_year"))
      out[ , item := str_replace(item, "section", "item")]
    } else {
      out[ , fiscal_year := as.integer(followup[i_year])]
      out = out[ , .(col1, col2, fiscal_year, col4, col5, col6, item, text)]
      setnames(out, 1L:6L, c("cik", "cname", "fiscal_year", "date_filed", "fiscal_year_end", "sic"))
      out[ , year_filed := year(date_filed)]
      cat("Keeping t+1 observations only\n")
      out = out[ year_filed <= as.integer(followup[i_year]) + 1L ]
      out[ , year_filed := NULL]
    }

    # cat("Saving\n")
    # saveRDS(out, str_c("Data/10K_JSON_splits_df/", "AR_", followup[i_year], "_byitem_df.rds"))
    # rm(list = c("current_list", "json_list", "df", "df_melt", "out"))
    # cat("Done!\n")
  }


}

