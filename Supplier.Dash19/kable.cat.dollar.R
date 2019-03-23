kable.cat.dollar <- function(filtered.data){
  require(knitr)
  cat.dollar <- aggregate(Total.Sales ~ Categ.By.Year + Category + Year, filtered.data, sum)
  cat.dollar <- arrange(cat.dollar, desc(Year))
  cat.year.vec <- unique(cat.dollar$Year)
  cat.kable.dollar <- data.frame("Category" = unique(cat.dollar$Category))
  
  for(i in 1:length(cat.year.vec)){
    cat.dollar.i <- cat.dollar %>% filter(Year == cat.year.vec[i]) %>% select(2,4)#%>% rename("2018" = Total.Sales)
    colnames(cat.dollar.i)[2] <- cat.year.vec[i]
    cat.dollar.i[,2] <- paste0("$", prettyNum(round(cat.dollar.i[,2]), big.mark = ","))
    cat.kable.dollar <- full_join(cat.kable.dollar, cat.dollar.i)
    #paste0("$",prettyNum(round(cat.agg.pretty$`Total.Sales`), big.mark = ","))
    
  }
  
  #colnames(cat.kable.dollar) <- c("Category", cat.year.vec)
  cat.kable.dollar[is.na(cat.kable.dollar)] <- 0
  cat.kable.dollar <- (cat.kable.dollar)
  cat.kable.dollar
}