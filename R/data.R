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
#'
#' @format A data frame with 15 rows and 13 variables:
#' \describe{
#'      \item{year}{Year.}
#'      \item{search}{Ads revenue from Search.}
#'      \item{mobile}{Ads revenue from Mobile.}
#'      \item{banner}{Ads revenue from Banner.}
#'      \item{digital_video}{Ads revenue from Digital Video.}
#'      \item{classifieds}{Ads revenue from Classifieds.}
#'      \item{lead_generation}{Ads revenue from Lead Generation.}
#'      \item{rich_media}{Ads revenue from Rich Media.}
#'      \item{sponsorship}{Ads revenue from Sponsorship.}
#'      \item{email}{Ads revenue from Email.}
#'      \item{referals}{Ads revenue from Referals.}
#'      \item{slotting_fees}{Ads revenue from Slotting Fees.}
#'      \item{other}{Ads revenue from Other internet channels.}
#' }
#' @source \url{https://www.iab.com/}
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
#' \describe{
#'      \item{Name}{Name of the player.}
#'      \item{G}{}
#'      \item{MIN}{}
#'      \item{PTS}{}
#'      \item{FGM}{}
#'      \item{FGA}{}
#'      \item{FGP}{}
#'      \item{FTM}{}
#'      \item{FTA}{}
#'      \item{FTP}{}
#'      \item{X3PM}{}
#'      \item{X3PA}{}
#'      \item{X3PP}{}
#'      \item{ORB}{}
#'      \item{DRB}{}
#'      \item{TRB}{}
#'      \item{AST}{}
#'      \item{STL}{}
#'      \item{BLK}{}
#'      \item{TO}{}
#'      \item{PF}{}
#' }
#' @source \url{https://flowingdata.com/}
"nba"

#' @title Internet attacks data, aggregated over all countries
#'
#' @description
#' A dataset containing logs of internet attacks of all countries for each hour
#' of a day during a fixed period.
#'
#' @format A data frame with 168 rows and 3 columns:
#' \describe{
#'      \item{wkday}{Week day: Monday, Tuesday, ..., Saturday, Sunday.}
#'      \item{hour}{00 - 24 hour of a day.}
#'      \item{n}{Number of attacks.}
#' }
#' @source \url{https://github.com/hrbrmstr/facetedcountryheatmaps/tree/master/data}
"attacks_all_countries"

#' @title Internet attacks data by each country
#'
#' @description
#' A dataset containing logs of internet attacks by country for each hour
#' of a day during a fixed period.
#'
#' @format A data frame with 1680 rows and 5 columns:
#' \describe{
#'      \item{country}{Country name.}
#'      \item{wkday}{Week day: Monday, Tuesday, ..., Saturday, Sunday.}
#'      \item{hour}{00 - 24 hour of a day.}
#'      \item{country_code}{2-letter country code.}
#'      \item{n}{Number of attacks.}
#' }
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
#' @format A data frame with 12 rows and 6 columns:
#' \describe{
#'      \item{Country}{Country name.}
#'      \item{Much worse}{Proportion of respondents said 'Much worse'.}
#'      \item{Somewhat worse}{Proportion of respondents said 'Somewhat worse'.}
#'      \item{Almost the same}{Proportion of respondents said 'Almost the same'.}
#'      \item{Somewhat better}{Proportion of respondents said 'Somewhat better'.}
#'      \item{Much better}{Proportion of respondents said 'Much better'.}
#' }
#' @source \url{http://rnotr.com/likert/ggplot/barometer/likert-plots/}
"ab3"

#' @title Fake data for testing mk_facet_lineplot()
#'
#' @description
#' A dataset containing fake powers and sample sizes when testing main effect
#' between male and female mice.
#'
#' @format A data frame with 448 rows and 5 columns:
#' \describe{
#'      \item{ssize}{Sample size.}
#'      \item{csize}{Number of cells per animal.}
#'      \item{delta}{Difference in outcome between male and female mice.}
#'      \item{rho}{Correlation coefficient within animal.}
#'      \item{Power}{Statistical power.}
#' }
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

#' @title A dataset of towns/cities with their populations in the US.
#'
#' @description
#' The U.S. Census Bureau publishes the population of every incorporated city
#' and town in the United States. This dataset contains their population
#' estimates as of July 1, 2012.
#'
#' @format A data frame with 19,516 rows and 2 variables.
#' \describe{
#'      \item{town}{Name of the town and city.}
#'      \item{pop}{Population estimate in thousands.}
#' }
#' @source http://www.census.gov/popest/data/cities/totals/2012/SUB-EST2012-3.html
"pops"

#' @title Fake data for testing mk_forestplot()
#'
#' @description
#' A dataset containing fake point estimates and their confidence intervals of
#' different groups. See inst/examples/ex-mk_forestplot.R
#'
#' @format A data frame with 26 rows and 5 columns:
#' \describe{
#'      \item{model}{Name of the model tried.}
#'      \item{group}{Group.}
#'      \item{est}{Coefficient estimate.}
#'      \item{lwr}{Lower bound of the 95\% confidence interval.}
#'      \item{upr}{Upper bound of the 95\% percent confidence interval.}
#' }
"ests_CIs"

#' @title U.S. Presidents Days in Office
#'
#' @description
#' A dataset containing the number of days in office for all U.S. presidents
#' until 2020.
#'
#' @format A data frame with 44 rows and 2 columns:
#' \describe{
#'      \item{president}{Name of the president.}
#'      \item{days}{Number of days in office.}
#' }
#' @source \url{https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States_by_time_in_office}
"days_in_office"
