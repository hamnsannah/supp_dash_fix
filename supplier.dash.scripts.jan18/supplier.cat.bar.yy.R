# Need to build out YTD vs. YTD or Full Year

#sample usage: supplier.cat.bar.yy("ACOMO JEWELRY")

supplier.cat.bar.yy <- function(name, hierarchy = "Supplier", data.object.mutated){
  
require(ggplot2)
require(dplyr)
require(lubridate)
require(scales)
  require(RColorBrewer)
  
  data.both<- data.object.mutated
  supp.cat.agg <- aggregate(Total.Sales ~ Supplier + Categ.By.Year + Category + Year, data.both, sum)
  
  one.supp.agg <- supp.cat.agg %>%
    filter(Supplier == name) %>%
    arrange(desc(Total.Sales))
  top20.cats <- unique(one.supp.agg$Category)[1:20]
  
  print(top20.cats)
    one.supp.agg <- filter(one.supp.agg, Category %in% top20.cats)
    print(head(one.supp.agg, 3))
    print(dim(one.supp.agg))
    print(tail(one.supp.agg, 6))
    
  one.supp.bar <- ggplot(data = one.supp.agg, aes(Categ.By.Year)) +
    geom_bar(aes(fill=Category, weight=Total.Sales)) + 
    coord_flip() + theme(legend.position = "none") + ylab("Total Sales in $") + xlab("Category") + 
    ggtitle(paste("Comparison of Categories for", name))+
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = colorRampPalette(brewer.pal(20, "RdBu"))(length(top20.cats)),
                      guide = guide_legend()) +
    #scale_fill_distiller(palette = "BuPu")+
    theme_dark()

  print(one.supp.bar)
}