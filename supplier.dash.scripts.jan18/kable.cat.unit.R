#this script is incomplete.  Still working on a loop so I don't have to manually change the years

kable.cat.unit <- function(filtered.data){
  
  cat.unit <- aggregate(Total.Sales ~ Categ.By.Year + Category + Year, filtered.data, length)
  cat.unit <- arrange(cat.unit, Year)
  cat.vec <- unique(cat.unit$Year)
  
  for(i in 1:length(cat.vec)){
    cat.unit.i <- cat.unit %>% filter(Year == cat.vec[i]) %>% select(2,4)#%>% rename("2018" = Total.Sales)
    cat.kable.unit <- full_join()
  }
  
  cat.2018.unit <- cat.unit %>% filter(Year == 2018) %>% select(2,4) %>% rename("2018" = Total.Sales)
  cat.2017.unit <- cat.unit %>% filter(Year == 2017) %>% select(2,4) %>% rename("2017" = Total.Sales)
  cat.2016.unit <- cat.unit %>% filter(Year == 2016) %>% select(2,4) %>% rename("2016" = Total.Sales)
  cat.2015.unit <- cat.unit %>% filter(Year == 2015) %>% select(2,4) %>% rename("2015" = Total.Sales)
  
  cat.kable.unit <- merge(cat.2018.unit, cat.2017.unit)
  cat.kable.unit <- merge(cat.kable.unit, cat.2016.unit)
  cat.kable.unit <- merge(cat.kable.unit, cat.2015.unit)
  
  
}