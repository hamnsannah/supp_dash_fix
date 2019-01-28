# Need to build out YTD vs. YTD or Full Year

#sample usage: supplier.bar.yy("ACOMO JEWELRY")

supplier.bar.yy <- function(name, hierarchy = "Supplier", data.object.mutated){
  
  require(ggplot2)
  require(dplyr)
  require(lubridate)
  require(scales)
  
  data.both <- data.object.mutated

  supp.cat.agg <- aggregate(Total.Sales ~ Supplier + Categ.By.Year + Category + Year, data.both, sum)
  supp.agg <- aggregate(Total.Sales ~ Supplier + Year, data.both, sum)
  supp.agg$Year <- as.factor(supp.agg$Year)
  
  one.supp.all.agg <- filter(supp.agg, Supplier == name)
  one.supp.all.bar <- ggplot(data = one.supp.all.agg, aes(Year)) +
    geom_bar(aes(fill=Year, weight=Total.Sales)) + 
    #coord_flip() + 
    theme(legend.position = "none") + 
    ylab("Total Sales in $") + xlab("Year Over Year") + ggtitle(paste("Full Year Sales Comparison for", name))+
    scale_y_continuous(labels = scales::comma) +
    scale_fill_brewer(palette = "RdBu")+
    #scale_fill_manual(values = "RdBu") +
    theme_dark()

  print(one.supp.all.bar)
}