
#sample usage: supplier.kable("ACOMO JEWELRY", data.object.mutated = mutated.four.years)

supplier.kable <- function(name, hierarchy = "Supplier", data.object.mutated){
    require(knitr)
    require(dplyr)
  data.both.supplier <- data.object.mutated
  data.both.supplier$Supp.By.Year <- paste(data.both.supplier$Supplier, data.both.supplier$Year)

  supplier.agg <- aggregate(Total.Sales ~ Supp.By.Year + Supplier + Year, data.both.supplier, sum)
  supplier.agg17 <- filter(supplier.agg, Year == 2017)
  supp17sum <- sum(supplier.agg17$Total.Sales)
  supplier.agg17 <- mutate(supplier.agg17, "Perc.Whole" = round((Total.Sales/supp17sum)*100, 2))
  supplier.agg17 <- arrange(supplier.agg17, desc(Total.Sales))

  supplier.agg16 <- filter(supplier.agg, Year == 2016)
  sup.merge16 <- supplier.agg16[,c(2,4)]
  colnames(sup.merge16) <- c("Supplier", "2016")
  sup.merge <- merge(supplier.agg17, sup.merge16, by.x="Supplier", all.x=TRUE, all.y=FALSE)
  sup.merge <- mutate(sup.merge, "Growth" = Total.Sales - `2016`, "Perc.Growth" = paste0(round((Growth/`2016`)*100,1),"%")) %>%
  arrange(desc(Total.Sales)) %>%
  select(1,4:8)
  colnames(sup.merge)[c(2,4)] <- c("Sales.2017", "Sales.2016")
  supplier.agg17.pretty <- sup.merge 
  supplier.agg17.pretty$Perc.Whole <- paste0(supplier.agg17.pretty$Perc.Whole, "%")
  supplier.agg17.pretty$Growth <- paste0("$",prettyNum(round(supplier.agg17.pretty$Growth), big.mark = ","))
  supplier.agg17.pretty$Sales.2016 <- paste0("$",prettyNum(round(supplier.agg17.pretty$Sales.2016), big.mark = ","))
  supplier.agg17.pretty$Sales.2017 <- paste0("$",prettyNum(round(supplier.agg17.pretty$Sales.2017), big.mark = ",")) ## Switch back
  supplier.agg17.pretty$Rank.2017 <- rownames(supplier.agg17.pretty)
  supplier.agg17.pretty <- select(supplier.agg17.pretty, Supplier, Rank.2017, Sales.2017, Sales.2016:Perc.Growth, Perc.Whole)
  colnames(supplier.agg17.pretty) <- c("Supplier", "Sales Rank 2017", "Sales 2017", "Sales 2016", "$ Growth", "% Growth", "% of All Sales")
  filter(supplier.agg17.pretty, Supplier == name)

}