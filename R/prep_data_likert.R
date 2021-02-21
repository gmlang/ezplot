#' @title Helper function for \code{mk_likertplot}. Not for external use.
#'
#' @description \code{prep_data_likert} takes in the original data frame, the
#' x, y and fillby variable, the supplied levels of the fillby variable, and
#' instruction on how to order the y levels. It returns a list of 2 data frames
#' (df_neg and df_pos), along with some plot elements for making a likert chart,
#' a.k.a., horizontal diverging bar chart.
#'
#' @param df Data frame of the original data.
#' @param xvar String, name of x numeric variable.
#' @param yvar String, name of y categorical (character or factor) variable.
#' @param fillby String, name of a second categorical variable for sub-dividing
#'   and coloring the bars.
#' @param fillby_lvls Character vector, levels of the fillby variable that the
#'   fillby variable should be ordered accordingly.
#' @param rawcnt_var. String, name of the variable that contains the raw
#'   count behind each tier. Default = NULL because not all input dataframe
#'   has such a variable.
#' @param yorder String, possible values are "alphanumeric", "descend" or
#'   "ascend". If "alphanumeric", order y levels in alphanumerical order
#'   along y-axis. If "ascend"/"descend", order y levels in ascending/descending
#'   order of the sum of absolute x-values along y-axis.
#'
#' @return A list of 2 data frames (df_neg and df_pos), breaks and labels for
#' the continuous axis, and palette for coloring the bars (pal).
#'
#' @seealso \code{\link{mk_likertplot}}.
prep_data_likert = function(df, xvar, yvar, fillby, fillby_lvls,
                            rawcnt_var, yorder) {

        # --- order y levels
        #       We're doing things opposite here (for example,
        #       when asked to order them in descending order, we implement in
        #       ascending order) because we're drawing plot horizontally.
        # ---
        if (is.null(yorder)) {
                # order the y levels in their order in the data
                lvls = rev(unique(df[[yvar]]))
                df[[yvar]] = factor(df[[yvar]], levels = lvls)
        } else if (yorder == "alphanumeric") {
                lvls = sort(unique(df[[yvar]]), decreasing=T)
                df[[yvar]] = factor(df[[yvar]], levels = lvls)

        } else if (yorder == "descend") {
                # reorder y levels in descending order of total abs(x) val
                df[[yvar]] = reorder(df[[yvar]], abs(df[[xvar]]),
                                     sum, na.rm = T)
        } else if (yorder == "ascend") {
                # reorder y levels in ascending order of total abs(x) val
                df[[yvar]] = reorder(df[[yvar]], -abs(df[[xvar]]),
                                     sum, na.rm = T)
        } else {
                # do nothing
        }

        # --- split df into 3 subsets: negative, positive and middle based on
        #     fillby_lvls
        # ---

        # order fillby variable according to fillby_lvls
        df[[fillby]] = factor(df[[fillby]], fillby_lvls)

        nlvls = length(fillby_lvls)
        if (nlvls %% 2 == 0) { # if there're even number of levels
                idx_mid_lvl  = NULL
                idx_neg_lvls = 1:(nlvls/2)
                idx_pos_lvls = (nlvls/2 + 1):nlvls
        } else { # if there're odd number of levels
                idx_mid_lvl  = (1 + nlvls) / 2 # find mid level first
                idx_neg_lvls = 1:(idx_mid_lvl-1)
                idx_pos_lvls = (idx_mid_lvl+1):nlvls
        }

        df_neg = dplyr::filter(
                df, !!as.name(fillby) %in% fillby_lvls[idx_neg_lvls])
        df_pos = dplyr::filter(
                df, !!as.name(fillby) %in% fillby_lvls[idx_pos_lvls])

        if (!is.null(idx_mid_lvl)) {
                # if there's odd number of levels, divide the x values of the
                #       mid level by 2 and append mid-level records to both pos
                #       and neg sets
                df_mid = dplyr::filter(
                        df, !!as.name(fillby) == fillby_lvls[idx_mid_lvl])
                df_mid[[xvar]] = df_mid[[xvar]] / 2
                df_neg = rbind(df_neg, df_mid)
                df_pos = rbind(df_mid, df_pos)
        }


        # --- fill x values of missing ylevels with 0 --- #

        ylvls_in_neg_not_pos = setdiff(unique(df_neg[[yvar]]),
                                       unique(df_pos[[yvar]]))
        if (length(ylvls_in_neg_not_pos) > 0) {
                add_pos = setNames(expand.grid(ylvls_in_neg_not_pos,
                                               fillby_lvls[idx_pos_lvls]),
                                   names(df_pos)[1:2])
                for (vname in names(df_pos)[-(1:2)]) add_pos[[vname]] = 0
                df_pos = rbind(df_pos, add_pos)
        }

        ylvls_in_pos_not_neg = setdiff(unique(df_pos[[yvar]]),
                                       unique(df_neg[[yvar]]))
        if (length(ylvls_in_pos_not_neg) > 0) {
                add_neg = setNames(expand.grid(ylvls_in_pos_not_neg,
                                               fillby_lvls[idx_neg_lvls]),
                                   names(df_neg)[1:2])
                for (vname in names(df_neg)[-(1:2)]) add_neg[[vname]] = 0
                df_neg = rbind(df_neg, add_neg)
        }

        # --- change x values of the negative set to negative if not already so
        #     create limits, breaks and labels for the continuous axis
        # ---

        use_pos_label_for_neg_axis = F

        if (all(df_neg[[xvar]] >= 0)) {
                df_neg[[xvar]] = -1 * df_neg[[xvar]]
                use_pos_label_for_neg_axis = T
        }

        xneg = dplyr::pull(
                dplyr::summarise(
                        dplyr::group_by(df_neg, !!as.name(yvar)),
                        x = sum(!!as.name(xvar))), x)
        xpos = dplyr::pull(
                dplyr::summarise(
                        dplyr::group_by(df_pos, !!as.name(yvar)),
                        x = sum(!!as.name(xvar))), x)
        con_axis_breaks = pretty(c(xneg, xpos), n = 10)
        if (use_pos_label_for_neg_axis) {
                con_axis_labs = abs(con_axis_breaks)
        } else { con_axis_labs = con_axis_breaks }
        con_axis_limits = c(min(con_axis_breaks), max(con_axis_breaks))

        # --- pick good bar colors --- #

        if (nlvls == 2) { # use colorblind friendly blue and orange
                pal = c("#fc7d0b", "#1170aa")
        } else {
                pal = RColorBrewer::brewer.pal(nlvls, "RdBu")
                if (!is.null(idx_mid_lvl)) # make middle level gray
                        pal[idx_mid_lvl] = "#DFDFDF"
        }
        names(pal) = fillby_lvls # crucial for correct legend


        # --- calc the x positions of the bar labels --- #

        df_neg_pos = rbind(
                df_neg %>% group_by(!!as.name(yvar)) %>%
                        arrange(desc(!!as.name(fillby))) %>%
                        mutate(cumval = cumsum(!!as.name(xvar))) %>%
                        arrange(!!as.name(fillby)) %>%
                        mutate(mid_pos = cumval - !!as.name(xvar) / 2),
                df_pos %>% group_by(!!as.name(yvar)) %>%
                        arrange(!!as.name(fillby)) %>%
                        mutate(cumval = cumsum(!!as.name(xvar))) %>%
                        mutate(mid_pos = cumval - !!as.name(xvar) / 2)) %>%
                group_by(!!as.name(yvar), !!as.name(fillby))

        if (is.null(rawcnt_var)) {
                df_neg_pos = df_neg_pos %>%
                        summarise(!!xvar := sum(abs(!!as.name(xvar))),
                                  mid_pos = sum(mid_pos))
        } else {
                df_neg_pos = df_neg_pos %>%
                        summarise(!!xvar := sum(abs(!!as.name(xvar))),
                                  mid_pos = sum(mid_pos),
                                  !!rawcnt_var := sum(!!as.name(rawcnt_var))
                                  )
        }

        # return
        list(df_neg = df_neg, df_pos = df_pos,
             df_neg_pos = df_neg_pos,
             con_axis_limits = con_axis_limits,
             con_axis_breaks = con_axis_breaks,
             con_axis_labs = con_axis_labs,
             pal = pal)
}
