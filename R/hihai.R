#' Healthcare-Associated Infections in Germany (2011 PPS)
#'
#' This dataset provides estimated annual numbers of healthcare-associated infections
#' (HAIs) in Germany based on the 2011 ECDC Point Prevalence Survey.
#'
#' @format A tibble with 6 rows (5 infection types + "All") and 18 columns:
#' \describe{
#'   \item{hai_type}{Abbreviated infection type — UTI, HAP, SSI, CDI, BSI, All.}
#'   \item{hai_name}{Full descriptive infection name.}
#'   \item{sample}{Survey type ("PPS").}
#'   \item{cases, deaths, dalys, yll, yld}{Annual estimated.}
#'   \item{cases_lower, cases_upper}{Lower / upper 95% uncertainty intervals for cases.}
#'   \item{deaths_lower, deaths_upper}{Lower / upper 95% uncertainty intervals for deaths.}
#'   \item{dalys_lower, dalys_upper}{Lower / upper 95% uncertainty intervals for DALYs.}
#'   \item{yll_lower, yll_upper}{Lower / upper 95% uncertainty intervals for YLLs.}
#'   \item{yld_lower, yld_upper}{Lower / upper 95% uncertainty intervals for YLDs.}
#' }
#'
#' @source Zacher et al. (2019) \url{https://doi.org/10.2807/1560-7917.ES.2019.24.46.1900135}
"hai_data"

#' Germany vs EU/EEA HAI Comparison
#'
#' This dataset contains per 100000 population rates of HAIs, deaths and DALYs,
#' comparing German PPS, German Convenience, and ECDC PPS (EU/EEA) estimates.
#'
#' @format A tibble with 18 rows (5 HAI types × 3 sample set) and 13 columns.
#' \describe{
#'   \item{hai_type}{Abbreviated infection type — UTI, HAP, SSI, CDI, BSI, All.}
#'   \item{hai_name}{Full descriptive infection name.}
#'   \item{sample}{Sample type ("German PPS", "German Convenience", or "ECDC PPS (EU/EEA)").}
#'   \item{cases_100k, deaths_100k, dalys_100k}{Rates per 100,000 population.}
#'   \item{cases_lower, cases_upper}{Lower / upper 95% uncertainty intervals for cases.}
#'   \item{deaths_lower, deaths_upper}{Lower / upper 95% uncertainty intervals for deaths.}
#'   \item{dalys_lower, dalys_upper}{Lower / upper 95% uncertainty intervals for DALYs.}
#' }
#' @source Zacher et al. (2019) \url{https://doi.org/10.2807/1560-7917.ES.2019.24.46.1900135}
"hai_comparison"
