product.table <- function(filtered.data, num.to.include){
  data.dive <- filtered.data
  data.dive$Product.By.Year <- paste(data.dive$Description, data.dive$Year)
  product.agg <- aggregate(Total.Sales ~ Product.By.Year + Description + Year + ItemID, data.dive, sum)
  product.agg <- mutate(product.agg, "Product" = paste0(Description, " (",ItemID,")"))
  
  cy <- year(max(filtered.data$Date.Sold))
  py <- cy-1
  product.agg.cy <- filter(product.agg, Year == cy)
  product.cy.sum <- sum(product.agg.cy$Total.Sales)
  product.agg.cy <- mutate(product.agg.cy, "Perc.Whole" = round((Total.Sales/product.cy.sum)*100, 2))
  product.agg.cy <- arrange(product.agg.cy, desc(Total.Sales))
  
  #product.agg.cy <- mutate(product.agg.cy, "Product" = paste0(Description, " (",ItemID,")"))
  product.agg.py <- filter(product.agg, Year == py)
  product.merge.py <- product.agg.py[,c(6,5)]
  colnames(product.merge.py) <- c("Product", "Prior Yr")
  product.merge <- merge(product.agg.cy, product.merge.py, by.x="Product", all.x=TRUE, all.y=FALSE)
  product.merge <- mutate(product.merge, "Growth" = Total.Sales - `Prior Yr`, "Perc.Growth" = 
                            paste0(round((Growth/`Prior Yr`)*100,1),"%")) %>%
    arrange(desc(Total.Sales)) %>%
    select(1,6:10)
  colnames(product.merge)[c(2,4)] <- c("Sales Current Yr", "Sales Prior Yr")
  product.merge[is.na(product.merge)] <- 0
  
  product.agg.cy.vec <- product.agg %>% filter(Year == cy) %>%
    arrange(desc(Total.Sales)) %>%
    select(Description, Total.Sales)
  
  #how.many.in.top <- 12 #select how many should be in plot, contingent on # of years
  #product.top.cy.vec <- unique(product.agg.cy.vec$Description)[1:how.many.in.top]
  #product.agg.top <- product.agg[product.agg$Description %in% product.top.cy.vec,]
  
  how.many.in.top <- num.to.include
  product.agg.cy.pretty <- product.merge
  product.agg.cy.pretty$Perc.Whole <- paste0(product.agg.cy.pretty$Perc.Whole, "%")
  product.agg.cy.pretty$Growth <- paste0("$",prettyNum(round(product.agg.cy.pretty$Growth), big.mark = ","))
  product.agg.cy.pretty$`Sales Prior Yr` <- paste0("$",prettyNum(round(product.agg.cy.pretty$`Sales Prior Yr`), big.mark = ","))
  product.agg.cy.pretty$`Sales Current Yr` <- paste0("$",prettyNum(round(product.agg.cy.pretty$`Sales Current Yr`), big.mark = ",")) ## Switch back
  if(nrow(product.agg.cy.pretty) < how.many.in.top){
    kable.products <- product.agg.cy.pretty
  } else {
    kable.products <- product.agg.cy.pretty[1:how.many.in.top,]
  }
  kable.products
}