#need to keep working on this to receive the output from yy.filter

#sample usage: supplier.kable("ACOMO JEWELRY", data.filtered = mutated.four.years)

supplier.summary.table <- function(data.filtered){
    require(knitr)
    require(dplyr)
  cy.year <- year(max(data.filtered$Date.Sold))
  py.year <- cy.year - years(1)
  data.both.supplier <- data.filtered
  data.both.supplier$Supp.By.Year <- paste(data.both.supplier$Supplier, data.both.supplier$Year)

  supplier.agg <- aggregate(Total.Sales ~ Supp.By.Year + Supplier + Year, data.both.supplier, sum)
  supplier.agg.cy <- filter(supplier.agg, Year == cy.year)
  supp.sum.cy <- sum(supplier.agg.cy$Total.Sales)
  supplier.agg.cy <- mutate(supplier.agg.cy, "Perc.Whole" = round((Total.Sales/supp.sum.cy)*100, 2))
  supplier.agg.cy <- arrange(supplier.agg.cy, desc(Total.Sales))

  supplier.agg.py <- filter(supplier.agg, Year == py.year)
  sup.merge.py <- supplier.agg.py[,c(2,4)]
  colnames(sup.merge.py) <- c("Supplier", "Prior.Year")
  sup.merge <- merge(supplier.agg.cy, sup.merge.py, by.x="Supplier", all.x=TRUE, all.y=FALSE)
  sup.merge <- mutate(sup.merge, "Growth" = Total.Sales - Prior.Year, "Perc.Growth" = paste0(round((Growth/`2016`)*100,1),"%")) %>%
  arrange(desc(Total.Sales)) %>%
  select(1,4:8)
  colnames(sup.merge)[c(2,4)] <- c("Sales.Current.Yr", "Sales.Prior.Yr")
  supplier.agg.cy.pretty <- sup.merge 
  supplier.agg.cy.pretty$Perc.Whole <- paste0(supplier.agg.cy.pretty$Perc.Whole, "%")
  supplier.agg.cy.pretty$Growth <- paste0("$",prettyNum(round(supplier.agg.cy.pretty$Growth), big.mark = ","))
  supplier.agg.cy.pretty$Prior.Year <- paste0("$",prettyNum(round(supplier.agg.cy.pretty$Prior.Year), big.mark = ","))
  supplier.agg.cy.pretty$Sales.2017 <- paste0("$",prettyNum(round(supplier.agg.cy.pretty$`Sales Prior Yr`), big.mark = ",")) ## Switch back
  supplier.agg.cy.pretty$Rank.2017 <- rownames(supplier.agg.cy.pretty)
  supplier.agg.cy.pretty <- select(supplier.agg.cy.pretty, Supplier, Rank.2017, Sales.2017, Sales.2016:Perc.Growth, Perc.Whole)
  colnames(supplier.agg.cy.pretty) <- c("Supplier", "Sales Rank Current Yr", "Sales Current Yr", "Sales Prior Yr", "$ Growth", "% Growth", "% of All Sales")
  filter(supplier.agg.cy.pretty, Supplier == name)

}