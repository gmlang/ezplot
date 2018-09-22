#' @title Helper function for \code{mk_lollipop}. Not for external use.
#'
#' @description \code{prep_data_lollipop} takes in the original data frame, the
#' x, y variables, and instruction on how to order the y levels. It returns a
#' data frame for making and labeling a horizontal lollipop chart.
#'
#' @param df data frame of the original data.
#' @param xvar string, name of x numeric variable.
#' @param yvar string, name of y categorical (character or factor) variable.
#' @param yorder string, possible values are "alphanumeric", "descend" or
#'   "ascend". If "alphanumeric", order y levels in alphanumerical order
#'   along y-axis. If "ascend"/"descend", order y levels in ascending/descending
#'   order of the sum of absolute x-values along y-axis.
#'
#' @return a data frame for making and labeling a horizontal lollipop chart.
#'
#' @seealso \code{\link{mk_lollipop}}.
prep_data_lollipop = function(df, xvar, yvar, yorder) {

        # --- order y levels
        #       We're doing things opposite here (for example,
        #       when asked to order them in descending order, we implement in
        #       ascending order) because we're drawing plot horizontally.
        # ---

        if (yorder == "alphanumeric")
                df[[yvar]] = factor(df[[yvar]],
                                    sort(unique(df[[yvar]]), decreasing = T))
        if (yorder == "descend")
                # reorder y levels in descending order of their counts
                df[[yvar]] = reorder(df[[yvar]], df[[yvar]],
                                     function(y) length(y))
        if (yorder == "ascend")
                # reorder y levels in ascending order of their counts
                df[[yvar]] = reorder(df[[yvar]], df[[yvar]],
                                     function(y) -length(y))


        # aggregate x over ylevels
        dplyr::summarise(
                dplyr::group_by(df, !!as.name(yvar)),
                !!as.name(xvar) := sum(!!as.name(xvar), na.rm = T),
                mid_pos = !!as.name(xvar) / 2
                )
}
