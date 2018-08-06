#' @title Create a function that draws ggplot2 heat maps.
#
#' @description
#' \code{mk_heatmap} takes a data frame as input and returns a function that
#' can be used to make heat maps using variables in the data frame.
#'
#' @param df A data frame.
#'
#' @return
#' \code{function(xvar, yvar, fillby, facet_by = NULL, facet_ncol = 1,
#'                palette = "C", font_size = 8, xlab = NULL, ylab = NULL, ...)}
#' \itemize{
#'      \item xvar   : string, name of a categorical variable for x-axis.
#'      \item yvar   : string, name of another categorical variable for y-axis.
#'      \item fillby : string, name of a continuous variable, and its values
#'                     are used to color the tiles.
#'      \item facet_by: string, name of a categorical variable for grouping and
#'                      creating facets. Default = NULL.
#'      \item facet_ncol: number of columns when facetting. Default = 1.
#'                        Only works when facet_by is not NULL.
#'      \item palette   : string, the colormap option to use. Possible values
#'                        are "A", "B", "C" (default), "D" and "E".
#'      \item font_size : overall font size. Default = 8. The font size of the
#'                        axes and legend text is a fraction of this value.
#'      \item xlab   : string, the x-axis label. Default is NULL.
#'      \item ylab   : string, the y-axis label. Default is NULL.
#'      \item ...    : other arguments for ggplot2::labs(), for example,
#'                     title, subtitle, caption and etc.
#' }
#'
#' @export
#' @examples
#' library(ezplot)
#' library(dplyr)
#'
#' # --- single heatmap --- #
#'
#' f = mk_heatmap(films %>% count(made_money, year_cat))
#' f("year_cat", "made_money", fillby = "n", ylab = "Made money?")
#'
#' df = films %>% group_by(action, year_cat) %>%
#'      summarise(avg_rating = mean(rating))
#' f = mk_heatmap(df)
#' f("year_cat", "action", fillby = "avg_rating", font_size = 14, palette = "D",
#'   ylab = "Is action film?",
#'   title = "Average ratings of action vs. non-action films")
#'
#' f = mk_heatmap(attacks_all_countries)
#' f("hour", "wkday", fillby = "n", font_size = 12, xlab = "nth Hour in a day",
#'   title = "Events per weekday & time of day")
#'
#' # --- multiple heatmaps --- #
#'
#' f = mk_heatmap(attacks_by_country)
#' f("hour", "wkday", fillby ="n", facet_by = "country", facet_ncol = 2,
#'   title = "Events per weekday & time of day by country")
mk_heatmap = function(df) {
        function(xvar, yvar, fillby, facet_by = NULL, facet_ncol = 1,
                 palette = "C", font_size = 8, xlab = NULL, ylab = NULL, ...) {

                # --- Main Plot --- #

                p = ggplot2::ggplot(df, ggplot2::aes_string(x = xvar, y = yvar,
                                                            fill = fillby)) +
                        ggplot2::geom_tile(color = "white", size = 0.1) +
                        ggplot2::scale_fill_viridis_c(
                                name = fillby, label = scales::comma,
                                option = palette, direction = -1) +
                        ggplot2::coord_equal()

                # --- Facet --- #

                if (!is.null(facet_by))
                        p = p + ggplot2::facet_wrap(
                                ggplot2::vars(!!as.name(facet_by)),
                                ncol = facet_ncol)

                # --- Customize Theme --- #

                p + ggplot2::labs(x = xlab, y = ylab, ...) +
                        cowplot::theme_cowplot(font_size = font_size) +
                        ggplot2::theme(
                                axis.line = ggplot2::element_blank(),
                                axis.ticks = ggplot2::element_blank(),

                                # rm gray background in header when faceting
                                strip.background = ggplot2::element_blank()

                                # # shorten distances between xy text and axes
                                # axis.text.x = ggplot2::element_text(
                                #         margin = ggplot2::margin(t = -2)),
                                # axis.text.x.top = ggplot2::element_text(
                                #         margin = ggplot2::margin(b = -2)),
                                # axis.text.y = ggplot2::element_text(
                                #         margin = ggplot2::margin(r = -2)),
                                # axis.text.y.right = ggplot2::element_text(
                                #         margin = ggplot2::margin(l = -2))
                                )

        }
}

