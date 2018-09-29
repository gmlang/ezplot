#' @title Helper function for \code{mk_dumbbell}. Not for external use.
#'
#' @description \code{prep_data_dumbbell} takes in the original data frame, the
#' left and right end point x variables, the y variable, and the
#' instruction on how to order the y levels. It returns a data frame with y
#' ordered according to the supplied instruction, and a new column called
#' diff (difference between the right and left end point xvars) appended.
#'
#' @param df data frame of the original data.
#' @param xvar_left string, name of a numeric variable, left end-point on x.
#' @param xvar_right string, name of a numeric variable, right end-point on x.
#' @param yvar string, name of y categorical (character or factor) variable.
#' @param yorder string, possible values are "alphanumeric", "descend" or
#'   "ascend". If "alphanumeric", order y levels in alphanumerical order
#'   along y-axis. If "ascend"/"descend", order y levels in ascending/descending
#'   order of the difference between xvar_right and xvar_left.
#'
#' @return the original data frame with y ordered according to instruction and a
#' new column called diff appended.
#'
#' @seealso \code{\link{mk_dumbbell}}.
prep_data_dumbbell = function(df, xvar_left, xvar_right, yvar, yorder) {

        # calc the difference between right and left end points
        df$diff = df[[xvar_right]] - df[[xvar_left]]

        # --- order y levels
        #       We're doing things opposite here (for example,
        #       when asked to order them in descending order, we implement in
        #       ascending order) because we're drawing plot horizontally.
        # ---

        if (yorder == "alphanumeric")
                df[[yvar]] = factor(df[[yvar]],
                                    sort(unique(df[[yvar]]), decreasing = T))

        if (yorder == "descend")
                df[[yvar]] = reorder(df[[yvar]], df$diff)

        if (yorder == "ascend")
                df[[yvar]] = reorder(df[[yvar]], -df$diff)

        df
}
