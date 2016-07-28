#' @title Calculates the freq and pct of each levels in a categorical variable 
#'        that exists in a data frame
#'        
#' @description
#' Before you plot a bar chart, you first need to count the frequencies of 
#' each level in the categorical variable. \code{tally1way} makes that easy. 
#' It takes a data frame as input and returns a function, which takes the 
#' name of the categorical variable of your interest that exists in the data  
#' frame, and returns the frequencies and percentages of the categories.
#' 
#' @param df A data frame.
#' 
#' @return
#' \code{function(xvar)}
#' \itemize{
#'      \item xvar     :  string, name of a categorical variable
#' }
#' 
#' @seealso \code{\link{mk_barplot}}.
#' @export
#' @examples
#' data(films)
#' f = tally1way(films)
#' f("year_cat")
#' f("make_money")
tally1way = function(dat) {
        # dat: a data frame
        function(xvar) {
                # Calculates the freq and pct of each level in a categorical variable.
                #
                # xvar: string, name of a categorical variable in dat
                tbl_cnt = table(dat[[xvar]])
                tbl_pct = prop.table(tbl_cnt)
                df_cnt = data.frame(tbl_cnt); names(df_cnt) = c(xvar, "cnt")
                df_pct = data.frame(tbl_pct); names(df_pct) = c(xvar, "pct")
                merge(df_cnt, df_pct)
        }
}


#' @title Calculates the freq and pct of each combination of the levels in 
#'        two categorical variables that exists in a data frame
#'        
#' @description
#' \code{tally2way} makes it really easy to calculate the frequencies and 
#' percentages of the combinations of the levels in two categorical variables.  
#' It takes a data frame as input and returns a function, which takes the 
#' names of any two categorical variables of your interest that exist in the data  
#' frame, and returns the frequencies and percentages of each of the combinations 
#' of the categories.
#' 
#' @param df A data frame.
#' 
#' @return
#' \code{function(xvar, yvar)}
#' \itemize{
#'      \item xvar     :  string, name of a categorical variable
#'      \item yvar     :  string, name of a categorical variable
#' }
#' 
#' @seealso \code{\link{mk_barplot}}.
#' @export
#' @examples
#' data(films)
#' f = tally2way(films)
#' f("year_cat", "make_money")
tally2way = function(dat) {
        # dat: a data frame
        function(xvar, yvar) {
                # Calculates the freq and pct of each levels in a categorical variable
                #
                # xvar, yvar: strings, names of categorical variables in dat
                tbl_cnt = table(dat[[xvar]], dat[[yvar]])
                tbl_pct = prop.table(tbl_cnt, margin = 1)
                df_cnt = data.frame(tbl_cnt); names(df_cnt) = c(xvar, yvar, "cnt")
                df_pct = data.frame(tbl_pct); names(df_pct) = c(xvar, yvar, "pct")
                merge(df_cnt, df_pct)
        }
}