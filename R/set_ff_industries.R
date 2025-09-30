if ( getRversion() >= "2.15.1" ) {
  utils::globalVariables( c("sic_min", "sic_max", "ff_ind", "ff_ind_short_desc", "ff_ind_desc") )
}
#' Map Fama–French Industries to a Corpus
#'
#' Assign Fama–French industry classifications to each document in a
#' [corpus][quanteda::corpus] object based on its SIC code. Industry groupings
#' are drawn from the datasets provided in the [**farr**](https://cran.r-project.org/web/packages/farr/index.html) 
#' package.
#'
#' @param x A [corpus][quanteda::corpus] object with a `sic` column in its
#'   document variables (`docvars`).
#' @param ind Character. One of `"12"`, `"17"`, `"30"`, `"38"`, `"48"`, or
#'   `"49"`, specifying which Fama–French industry grouping to apply. See 'Details'.
#' @param fill_category Logical. If `TRUE`, documents with unmatched SIC codes
#'   are assigned to a new category labelled `"Unclassified"`. Defaults to `FALSE`.
#' @param ... Currently unused.
#' 
#' @details
#' The Fama-French industry classifications are available in several versions, with the most 
#' commonly used being the 12-industry (FF12), 17-industry (FF17), and 48-industry (FF48) groupings. 
#' These are widely adopted in empirical asset pricing research due to their balance between 
#' interpretability and granularity. The 49-industry classification (FF49), which originates 
#' from Fama and French’s 1997 Journal of Financial Economics paper, remains a frequently used 
#' option, especially for studies requiring finer industry resolution. An older 30-industry 
#' scheme (FF30) also exists but is largely deprecated and not recommended for current research 
#' applications. A 38-industry classification (FF38) has been introduced more recently and is 
#' occasionally used by Kenneth French in the construction of anomaly portfolios and other datasets. 
#' Each scheme groups SIC codes into economically meaningful categories with varying levels of 
#' detail, allowing users to tailor industry aggregation to their research needs.
#' 
#' Supported industry schemes are:
#' - **FF12**, **FF17**, **FF30**, **FF38**, **FF48**, **FF49**  
#' Each scheme aggregates SIC codes into economically meaningful industry
#' groups of different granularity.
#'
#' For documents whose `sic` does not match any range in the selected scheme:
#' - If `fill_category = FALSE` (default), the FF variables will be `NA`.
#' - If `fill_category = TRUE`, a fallback `"Unclassified"` category is created.
#' 
#' @return A [corpus][quanteda::corpus] object with additional `docvars`:
#' - `ff_ind`: Numeric industry identifier.
#' - `ff_ind_short_desc`: Short description.
#' - `ff_ind_desc`: Full description.
#'
#' @note This function requires the [**farr**](https://cran.r-project.org/web/packages/farr/index.html) 
#' package, which must be installed separately. 
#' It provides the Fama–French industry classification mappings via [farr::get_ff_ind()].
#'
#' @import data.table
#' @importFrom quanteda is.corpus corpus docvars ndoc docnames
#' @importFrom stats setNames
#' @importFrom glue glue
#' @importFrom stringr str_glue
#' @importFrom cli cli_alert_info cli_alert_success
#' @export

set_ff_industries <- function(x, ind, fill_category = FALSE, ...) {
  
  if (!requireNamespace("farr", quietly = TRUE)) {
    stop("Package 'farr' is required for set_ff_industries(). Please install it.", call. = FALSE)
  }
  if (!is.corpus(x)) {
    stop("x must be a corpus")
  }
  if (!is.numeric(ind)) {
    stop("ind must be a numeric value")
  }
  
  # Negate it! 
  `%nin%` = Negate(`%in%`)
  
  corp_dt = data.table(doc_id = docnames(x), docvars(x), text = as.character(x))
  setcolorder(corp_dt, neworder = "item", after = "filing_type")
  input_size = nrow(corp_dt)
  
  if("sic" %nin% names(corp_dt)) {
    stop("A column \"sic\" containing industry codes must be stored in the input corpus x")
  }
  
  ind = as.character(ind)
  ind = match.arg(ind, choices = c("12", "17", "30", "38", "48", "49"))
  ind = as.numeric(ind)
  
  cli_alert_info("Pulling {ind} Fama-French industries")
  get_ff_ind <- getExportedValue("farr", "get_ff_ind")
  ff = get_ff_ind(ind = ind)
  setDT(ff)
  
  # Build column expression list from input data structure
  expr_list = setNames(
    lapply(names(corp_dt), function(col) as.name(paste0("i.", col))),
    names(corp_dt)
  )
  
  # Add FF mapping columns
  expr_list$ff_ind = quote(ff_ind)
  expr_list$ff_ind_short_desc = quote(ff_ind_short_desc)
  expr_list$ff_ind_desc = quote(ff_ind_desc)
  
  cli_alert_info("Mapping Fama-French industries and rebuilding the corpus")
  # Evaluate non-equi join and keep all desired columns
  mapped = ff[
    corp_dt,
    on = .(sic_min <= sic, sic_max >= sic),
    mult = "first",
    eval(as.call(c(quote(`.`), expr_list)))
  ]
  
  # # Impose row-ordering
  setorder(mapped, cik, fyear, item)
  # Improve column order
  setcolorder(mapped, c("ff_ind", "ff_ind_short_desc", "ff_ind_desc"), after = "sic")
  
  if (fill_category) {
    cli_alert_info("Filling unclassified industries")
    mapped[
      is.na(ff_ind), `:=` (
        ff_ind = ind + 1L,
        ff_ind_short_desc = "Unclassified",
        ff_ind_desc = str_glue("Unclassified -- Unmatched industries when ind = {ind}")
      )
    ]
  }
  
  out = corpus(mapped, docid_field = "doc_id", text_field = "text")
  output_size = ndoc(out)
  if( input_size != output_size ) {
    warning(glue("Input size was {input_size} documents while output size is {output_size} documents"))
  }
  cli_alert_success("Fama-French industries successfully mapped")
  return(out)
  
}
