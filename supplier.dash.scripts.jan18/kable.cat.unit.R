#this script is incomplete.  Still working on a loop so I don't have to manually change the years

kable.cat.unit <- function(filtered.data){
  require(knitr)
  cat.unit <- aggregate(Total.Sales ~ Categ.By.Year + Category + Year, filtered.data, length)
  cat.unit <- arrange(cat.unit, desc(Year))
  cat.year.vec <- unique(cat.unit$Year)
  cat.kable.unit <- data.frame("Category" = unique(cat.unit$Category))
  
  for(i in 1:length(cat.year.vec)){
    cat.unit.i <- cat.unit %>% filter(Year == cat.year.vec[i]) %>% select(2,4)#%>% rename("2018" = Total.Sales)
    colnames(cat.unit.i)[2] <- cat.year.vec[i]
    cat.kable.unit <- full_join(cat.kable.unit, cat.unit.i)

  }
  
  #colnames(cat.kable.unit) <- c("Category", cat.year.vec)
  cat.kable.unit[is.na(cat.kable.unit)] <- 0
  cat.kable.unit <- kable(cat.kable.unit)
  cat.kable.unit
}
  
  cat.2018.unit <- cat.unit %>% filter(Year == 2018) %>% select(2,4) %>% rename("2018" = Total.Sales)
  cat.2017.unit <- cat.unit %>% filter(Year == 2017) %>% select(2,4) %>% rename("2017" = Total.Sales)
  cat.2016.unit <- cat.unit %>% filter(Year == 2016) %>% select(2,4) %>% rename("2016" = Total.Sales)
  cat.2015.unit <- cat.unit %>% filter(Year == 2015) %>% select(2,4) %>% rename("2015" = Total.Sales)
  
  cat.kable.unit <- merge(cat.2018.unit, cat.2017.unit)
  cat.kable.unit <- merge(cat.kable.unit, cat.2016.unit)
  cat.kable.unit <- merge(cat.kable.unit, cat.2015.unit)
  
  
}