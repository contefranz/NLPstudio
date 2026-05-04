#' Loughran and McDonald Firm Complexity Dictionary
#'
#' A [dictionary] containing the 2024 Loughran and McDonald firm complexity
#' word list. The object has a single top-level entry, `Firm_Complexity`, with
#' 53 terms for complexity-oriented text analysis.
#' @source 
#' <https://sraf.nd.edu/complexity/>
#' 
#' <https://www.cambridge.org/core/journals/journal-of-financial-and-quantitative-analysis/article/measuring-firm-complexity/D737FD0A697AF699C5AADD62842ACAB8>
#' @references
#'   Loughran, T. & McDonald, B. (2024). 
#'   [Measuring Firm Complexity](https://www.cambridge.org/core/journals/journal-of-financial-and-quantitative-analysis/article/measuring-firm-complexity/D737FD0A697AF699C5AADD62842ACAB8),
#'   *Journal of Financial and Quantitative Analysis*, 
#'   2023:1-28.
#' @keywords data
"data_dictionary_LoughranMcDonald_Complexity"

#' Feng Li Forward Looking Statement Dictionary
#'
#' A [dictionary] containing Feng Li's 2010 forward-looking statement word
#' list. The object has a single top-level entry, `FLS_Li`, with 18 terms.
#' @source DOI: 10.1111/j.1475-679X.2010.00382.x
#' @references
#'   Li, F. (2010). The Information Content of Forward-Looking Statements in
#'   Corporate Filings—A Naïve Bayesian Machine Learning Approach,
#'   *Journal of Accounting Research*, 48 (5), 1049--1102.
#' @keywords data
"data_dictionary_Li_FLS"

#' Bozanic Roulstone VanBuskirk Forward Looking Statement Dictionary
#'
#' A [dictionary] containing the 2018 Bozanic, Roulstone, and Van Buskirk
#' forward-looking statement word list. The object has a single top-level
#' entry, `FLS_BozanicRoulstoneVanBuskirk`, with 90 terms.
#' @source <https://www.sciencedirect.com/science/article/abs/pii/S0165410117300733>
#' @references
#'   Bozanic, Z., Roulstone, D.T., Van Buskirk, A. (2018), Management earnings forecasts and other
#'   forward-looking statements, *Journal of Accounting and Economics*, Volume 65, Issue 1, 2018,
#'   Pages 1-20.
#' @keywords data
"data_dictionary_BozanicRoulstoneVanBuskirk_FLS"

#' Cannon, Ling, Wang, and Watanabe CSR Dictionary
#'
#' A [dictionary] containing the Cannon, Ling, Wang, and Watanabe corporate
#' social responsibility word list. The object has four top-level categories
#' and mixes unigrams with multi-word expressions.
#'
#' @details
#' The dictionary contains four categories:
#' * `Philantropy`
#' * `BusinessPractice`
#' * `Product`
#' * `General`
#'
#' @source DOI: 10.1080/09638180.2019.1670223
#' @references
#'   Cannon, J. N., Ling, Z., Wang, Q., & Watanabe, O. V. (2020).
#'   10-K disclosure of corporate social responsibility and firms’ competitive advantages.
#'   _European Accounting Review_, 29(1), 85-113.
#' @keywords data
"data_dictionary_Cannon_Ling_Wang_Watanabe"

#' U.N. Sustainable Development Goals (SDG) Mapping Dictionary
#'
#' A [dictionary] containing key terms for mapping research text to
#' Sustainable Development Goal themes following Wang et al. (2023). The object
#' has 16 top-level entries named `SDG1` through `SDG16`.
#'
#' @details
#' The Auckland Approach, an enhanced method for mapping research publications to the Sustainable
#' Development Goals (SDGs), utilizes advanced text-mining techniques and n-gram analyses to
#' refine and expand keyword lists derived from publication metadata. This approach, built on
#' foundational work by Elsevier, Sustainable Development Solutions Network, and the
#' United Nations (UN), improves the precision and coverage of SDG
#' mapping by incorporating both globally standardized and locally relevant keywords.
#' The methodology ensures a more comprehensive, contextualized, and data-driven mapping process,
#' effectively tailoring global SDG themes to specific local research needs and narratives.
#'
#' @source
#' <https://www.researchsquare.com/article/rs-2544385/v2>
#'
#' <https://www.sdgmapping.auckland.ac.nz/>
#' @references
#'   Wang, W., Kang, W., & Mu, J. (2023). Mapping research to the sustainable development goals
#'   _Working Paper_ available at <https://www.researchsquare.com/article/rs-2544385/v2>.
#' @keywords data
"data_dictionary_SDG"
