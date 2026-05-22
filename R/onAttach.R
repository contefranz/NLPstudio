.onAttach <- function(libname, pkgname) {
  ver <- utils::packageVersion("NLPstudio")
  packageStartupMessage(
    cli::rule(
      left  = cli::style_bold(paste0("NLPstudio ", ver)),
      right = "https://github.com/contefranz/NLPstudio"
    ), "\n",
    "Core imports: cli, data.table, ggplot2, Matrix, methods, quanteda,\n",
    "              quanteda.textstats\n",
    "Optional backends: text2vec, topicmodels, seededlda, stm,\n",
    "                   topicmodels.etm, torch, spacyr, tidytext,\n",
    "                   RcppSimdJson, uwot\n",
    "Use library(<pkg>) to attach any of these to your session.\n",
    "Optional packages are only needed for the functions that use them."
  )
}
