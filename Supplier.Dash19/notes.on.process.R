# Notes on preparing new data from Jim Harte for the Supplier Dash

# 2 forms needed:


# supplier data aggregated by month.  Also needs a Date.Sold field in it for exploratory.jh.time.series.R to work properly
library(lubridate)
library(dplyr)
sales.data$Month <- month(sales.data$TransactionTime)
sales.data$Year <- year(sales.data$TransactionTime)
sales.data <- rename(sales.data, "Total.Sales" = Price)

sales.data <- aggregate(Total.Sales ~ Department + Category + Supplier + ItemID + Description + Year + Month, sales.data, sum)

sales.data$Date.Sold <- paste0(sales.data$Year, "-", sales.data$Month, "-01")
sales.data$Dept.By.Year <- paste(sales.data$Department, sales.data$Year)
sales.data$Categ.By.Year <- paste(sales.data$Category, sales.data$Year)
sales.data <- select(sales.data, c("Department", "Category", "Supplier", "ItemID", "Description", "Year", "Month", 
                                   "Dept.By.Year", "Categ.By.Year", "Total.Sales", "Date.Sold"))
write.csv(sales.data, "Supplier.Dash19/sales.data.monthly2.csv", row.names = FALSE)

# supplier.vec (list of all suppliers for current and previous year)
#supplier.vec also should be alphabetized
new.supplier.vec <- data.frame("Supplier" = unique(sales.data$Supplier))
new.supplier.vec <- arrange(new.supplier.vec, Supplier)
write.csv(new.supplier.vec, "Supplier.Dash19/wt.supplier.vec2.csv", row.names = FALSE)