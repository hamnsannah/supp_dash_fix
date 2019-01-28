#sample usage supplier.prophet("ACOMO JEWELRY", data.object.mutated = mutated.four.years)

supplier.prophet <- function(supplier.name, data.object.mutated){
  library(prophet)
  require(dplyr)
  require(ggplot2)
  require(lubridate)
  
  data.both <- data.object.mutated
  #data.both <- filter(data.object.mutated, Year %in% c(2016, 2017))
  data.both$Date <- as_date(data.both$Date.Sold)
  data.date.agg <- aggregate(Total.Sales ~ Date + Supplier, data.both, sum)
  colnames(data.date.agg) <- c("ds", "Supplier", "y")
  print(head(data.date.agg))
  prophet.data <- filter(data.date.agg, Supplier == supplier.name)
  #print(head(prophet.data, 10))
  prophet.data <- select(prophet.data, ds, y)
  model <- prophet(prophet.data, yearly.seasonality = TRUE)
  future <- make_future_dataframe(model, periods = 365)
  forecast <- predict(model, future)
  prophet.plot.obj <- prophet_plot_components(model, forecast)
  #print(prophet.plot.obj)
}