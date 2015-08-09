    

mk_areaplot = function(df) {
        function(xvar, yvar, fillby, 
                 xlab="", ylab="", main="", legend=T, 
                 arealab=NULL, arealab_use_pct=F, decimals=2,
                 arealab_at_top=F, arealab_size=3) {
                
                p = ggplot2::ggplot(df, ggplot2::aes_string(x = xvar, y = yvar, 
                                                            fill = fillby,
                                                            order = fillby)) + 
                        ggplot2::geom_area(position = "stack") + 
                        ggplot2::labs(x = xlab, y = ylab, title = main) +
                        ggplot2::theme_bw() +
                        ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))
                
                if (!legend) p = p + ggplot2::guides(fill = FALSE)
                
                if (!is.null(arealab)) {  
                        if (arealab_use_pct) 
                                df$label = format_as_pct(df[[arealab]], 
                                                             digits=decimals+2)
                        else df$label = df[[arealab]]
                        
                        if (arealab_at_top)
                                arealab_pos = paste(yvar, "pos_top", sep="_")
                        else arealab_pos = paste(yvar, "pos_mid", sep="_")
                        
                        p = p + ggplot2::geom_text(data=df, 
                                                   ggplot2::aes_string(
                                                           label = "label",
                                                           y = arealab_pos), 
                                                   size = arealab_size)
                }
                p
        }
}