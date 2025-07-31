core <- c("quanteda", "quanteda.textstats", "spacyr", "readtext", "data.table", "text2vec",
          "stringr", "jsonlite", "foreach", "future")


.onAttach <- function(...) {
  attached <- NLPstudio_attach()
  packageStartupMessage(NLPstudio_attach_message(attached))
}

is_attached <- function(x) {
  paste0("package:", x) %in% search()
}

is_loading_for_tests <- function() {
  !interactive() && identical(Sys.getenv("DEVTOOLS_LOAD"), "NLPstudio")
}

core_unloaded <- function() {
  search <- paste0("package:", core)
  core[!search %in% search()]
}

# Attach the package from the same package library it was loaded from before.
same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  library(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
}

NLPstudio_attach <- function() {
  to_load <- core_unloaded()

  suppressPackageStartupMessages(
    lapply(to_load, same_library)
  )

  invisible(to_load)
}

NLPstudio_attach_message <- function(to_load) {
  if (length(to_load) == 0) {
    return(NULL)
  }

  header <- cli::rule(
    left = cli::style_bold("Attaching core NLPstudio packages"),
    right = paste0("NLPstudio ", package_version_h("NLPstudio"))
  )

  to_load <- sort(to_load)
  versions <- vapply(to_load, package_version_h, character(1))

  packages <- paste0(
    cli::col_green(cli::symbol$tick), " ", cli::col_br_cyan(format(to_load)), " ",
    cli::ansi_align(versions, max(cli::ansi_nchar(versions)))
  )

  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  paste0(header, "\n", paste(info, collapse = "\n"))
}

package_version_h <- function(pkg) {
  highlight_version(utils::packageVersion(pkg))
}

highlight_version <- function(x) {
  x <- as.character(x)

  is_dev <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    !is.na(x) & x >= 9000
  }

  pieces <- strsplit(x, ".", fixed = TRUE)
  pieces <- lapply(pieces, function(x) ifelse(is_dev(x), cli::col_red(x), x))
  vapply(pieces, paste, collapse = ".", FUN.VALUE = character(1))
}
