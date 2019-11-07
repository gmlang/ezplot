#' @title Films data downloaded from IMDB
#'
#' @description
#' A dataset containing the budgets, boxoffices, ratings and other attributes of
#' 5944 films from 1913 to 2014 obtained from IMDB.com.
#'
#' @format A data frame with 5944 rows and 53 variables:
#' \describe{
#'      \item{title}{Title of the movie.}
#'      \item{year}{Year of release.}
#'      \item{budget}{Total budget in US dollars.}
#'      \item{length}{Length in minutes.}
#'      \item{rating}{Average IMDB user rating. These ratings are on a scale of 1 (worst) to 10 (best).}
#'      \item{votes}{Number of IMDB users who rated this movie.}
#'      \item{r1-10}{Distribution of votes for each rating, to mid point of nearest decile: 0 = no votes, 4.5 = 1-9 votes, 14.5 = 11-19 of votes, etc.  Due to rounding errors these may not sum to 100.}
#'      \item{mpaa}{MPAA rating (ex: R, PG-13).}
#'      \item{boxoffice}{Total Ticket sales in US dollars.}
#'      \item{actor, actor, actress, director, and writer}{String variables giving these people's names.}
#'      \item{action, animation, comedy, drama, documentary, romance, short, and etc}{Binary variables representing if movie was classified as belonging to that genre.}
#' }
#' @source \url{http://www.imdb.com/interfaces/}
"films"

#' @title Aggregated Annual Budget and Boxoffice
#'
#' @description
#' A dataset containing the ggregated annual Budget and Boxoffice of
#' 5944 films beween 1913 and 2014 from IMDB.com.
#'
#' @format A data frame with 202 rows and 4 variables:
#' \describe{
#'      \item{year}{Year of release.}
#'      \item{type}{"Budget" or "Boxoffice".}
#'      \item{tot}{Total Amount in US Dollars.}
#'      \item{avg}{Average Amount in US Dollars.}
#' }
"btbo_by_year"

#' @title Boxoffice/Budget Ratio
#'
#' @description
#' A dataset containing the Boxoffices/Budget ratio from 1913 to 2014 of
#' 5944 films obtained from IMDB.com.
#'
#' @format A data frame with 101 rows and 2 variables:
#' \describe{
#'      \item{year}{Year of release.}
#'      \item{bo_bt_ratio}{Boxoffice/Budget Ratio}
#' }
"bo_bt_ratio_by_year"

#' @title Internet Ads Revenue
#'
#' @description
#' A dataset containing internet ads revenue from 2000 to 2014.
#' Source: iab.net
#'
#' @format A data frame with 15 rows and 13 variables:
"ads"

#' @title Cancer Survival Rates
#'
#' @description
#' A dataset containing the survival rates of 24 different cancers.
#'
#' @format A data frame with 96 rows and 3 variables:
#' \describe{
#'      \item{group}{Different cancer types}
#'      \item{year}{x years since cancer onset}
#'      \item{value}{survival rates, proportion of patients survived}
#' }
"cancer"

#' @title NBA Player Performance Statistics
#'
#' @description
#' A dataset containing 21 different performance statistics of 50 NBA players
#' obtained from flowingdata.com.
#'
#' @format A data frame with 50 rows and 21 columns:
"nba"

#' @title Internet attacks data, aggregated over all countries
#'
#' @description
#' A dataset containing logs of internet attacks of all countries for each hour
#' of a day during a fixed period. It's derived from this raw data source:
#' https://github.com/hrbrmstr/facetedcountryheatmaps/tree/master/data
#'
#' @format A data frame with 168 rows and 3 columns:
#' @source \url{https://github.com/hrbrmstr/facetedcountryheatmaps/tree/master/data}
"attacks_all_countries"

#' @title Internet attacks data by each country
#'
#' @description
#' A dataset containing logs of internet attacks by country for each hour
#' of a day during a fixed period. It's derived from this raw data source:
#' https://github.com/hrbrmstr/facetedcountryheatmaps/tree/master/data
#'
#' @format A data frame with 1680 rows and 5 columns:
#' @source \url{https://github.com/hrbrmstr/facetedcountryheatmaps/tree/master/data}
"attacks_by_country"

#' @title Arab Baromteter Wave III Survey
#'
#' @description
#' A dataset containing estimates for twelve countries on a Likert-type rating
#' of confidence in the future economy. Respondents were asked,
#' "What do you think will be the economic situation in your country during the
#' next few years (3-5 years) compared to the current situation?"
#'
#' Dataset is obtained from http://rnotr.com/likert/ggplot/barometer/likert-plots/.
#'
#' @format A data frame with 12 rows and 6 columns:
"ab3"

#' @title Fake data for testing mk_facet_lineplot()
#'
#' @description
#' A dataset containing fake powers and sample sizes when testing main effect
#' between male and female. See inst/examples/ex-mk_facet_lineplot.R
#'
#' @format A data frame with 448 rows and 5 columns:
"power_n_ssize_gender"

#' @title A dataset of information at birth for 44 babies
#'
#' @description
#' On December 18, 1997, 44 babies were born in a hospital in Brisbane, Australia.
#' This dataset contains their birth times and other information at birth.
#'
#' @format A data frame with 44 rows and 4 variables, sorted in ascending order
#' of birth time. (The time window is 24 hours, i.e., 44 babies were born in 24
#' hours.)
#' \describe{
#'      \item{sex}{Gender of the baby.}
#'      \item{weight_g}{Weight (in grams) at birth.}
#'      \item{minutes}{Time of birth converted to minutes since midnight.}
#'      \item{diffs}{Difference (in minutes) between consecutive birth times.}
#' }
#' @source Dunn, "A Simple Dataset for Demonstrating Common Distributions," Journal of Statistics Education v.7, n.3 (1999)
"births"

