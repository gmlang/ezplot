facet1="", facet2="", 

if (facet1 != "" && facet2 != "")
        p = p + ggplot2::facet_grid(
                as.formula(paste(facet1, "~", facet2)))


p = plt("event", "cnt", fillby="color", facet1=".", 
        facet2 = "event_relationship",
        main=title, ylab="count", 
        label_bars=T, labelvar="cnt", posvar = "pos_top") 
