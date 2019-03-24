product.facet <- function(filtered.data, freemium.end.date){
  data.dive <- filtered.data
  data.dive$Product.By.Year <- paste(data.dive$Description, data.dive$Year)
  product.agg <- aggregate(Total.Sales ~ Product.By.Year + Description + Year, data.dive, sum)
  
  cy <- year(freemium.end.date)
  #py <- year(freemium.end.date)-1
  #product.agg.cy <- filter(product.agg, Year == cy)
  #product.cy.sum <- sum(product.agg.cy$Total.Sales)
  #product.agg.cy <- mutate(product.agg.cy, "Perc.Whole" = round((Total.Sales/product.cy.sum)*100, 2))
  #product.agg.cy <- arrange(product.agg.cy, desc(Total.Sales))
  
  #product.agg.cy <- rename(product.agg.cy, "Product" = Description)
  #product.agg.py <- filter(product.agg, Year == py)
  #product.merge.py <- product.agg.py[,c(2,4)]
  #colnames(product.merge.py) <- c("Product", "Prior Yr")
  #product.merge <- merge(product.agg.cy, product.merge.py, by.x="Product", all.x=TRUE, all.y=FALSE)
  #product.merge <- mutate(product.merge, "Growth" = Total.Sales - `Prior Yr`, "Perc.Growth" = 
  #                          paste0(round((Growth/`Prior Yr`)*100,1),"%")) %>%
  #  arrange(desc(Total.Sales)) %>%
  #  select(1,4:8)
  #colnames(product.merge)[c(2,4)] <- c("Sales Current Yr", "Sales Prior Yr")
  #product.merge[is.na(product.merge)] <- 0
  
  product.agg.18.vec <- product.agg %>% filter(Year == year(freemium.end.date)) %>%
    arrange(desc(Total.Sales)) %>%
    select(Description, Total.Sales)
  
  how.many.in.top <- 12 #select how many should be in plot, contingent on # of years
  product.top.cy.vec <- unique(product.agg.18.vec$Description)[1:how.many.in.top]
  product.agg.top <- product.agg[product.agg$Description %in% product.top.cy.vec,]
  
  gfacet <- ggplot(data = product.agg.top, aes(x = Year, y = Total.Sales, group = Description))+ 
    geom_line(color = "white", size = 3) + geom_point(color = "white", size = 3) +
    facet_wrap(~Description) + theme_dark()
  
  #g <- ggplot(data = product.agg.top, aes(x = Product.By.Year, fill = factor(Year))) + 
  #geom_bar(stat = "identity", aes(y = Total.Sales)) + coord_flip() +
  #scale_y_continuous(labels = scales::dollar) +
  #scale_fill_brewer(direction = 1, palette = "RdBu", name = "Year") + theme_dark() + 
  #labs(title = "Sales From Top Products By Year", y = "Total Sales", x = "Product By Year") 
  (gfacet)
}