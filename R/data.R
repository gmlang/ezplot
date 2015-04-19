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