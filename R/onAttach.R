.onAttach <- function(libname, pkgname) {
  ver <- utils::packageVersion("NLPstudio")
  packageStartupMessage(
    cli::rule(
      left  = cli::style_bold(paste0("NLPstudio ", ver)),
      right = "https://github.com/contefranz/NLPstudio"
    ), "\n",
    "Requires: cli, data.table, ggplot2, Matrix, methods, quanteda,\n",
    "          quanteda.textstats, stringr, text2vec\n",
    "Use library(<pkg>) to attach any of these to your session."
  )
}
