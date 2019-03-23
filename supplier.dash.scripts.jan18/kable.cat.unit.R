#this script is incomplete.  Still working on a loop so I don't have to manually change the years

kable.cat.unit <- function(filtered.data){
  require(knitr)
  cat.unit <- aggregate(Total.Sales ~ Categ.By.Year + Category + Year, filtered.data, length)
  cat.unit <- arrange(cat.unit, desc(Year))
  cat.year.vec <- unique(cat.unit$Year)
  cat.kable.unit <- data.frame("Category" = unique(cat.unit$Category))
  
  for(i in 1:length(cat.year.vec)){
    cat.unit.i <- cat.unit %>% filter(Year == cat.year.vec[i]) %>% select(2,4)#%>% rename("2018" = Total.Sales)
    cat.unit.i[,2] <- round(cat.unit.i[,2])
    colnames(cat.unit.i)[2] <- cat.year.vec[i]
    cat.kable.unit <- full_join(cat.kable.unit, cat.unit.i)

  }
  
  #colnames(cat.kable.unit) <- c("Category", cat.year.vec)
  cat.kable.unit[is.na(cat.kable.unit)] <- 0
  cat.kable.unit <- (cat.kable.unit)
  cat.kable.unit
}
