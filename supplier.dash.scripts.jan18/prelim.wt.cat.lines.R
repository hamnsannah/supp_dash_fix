prelim.wt.cat.lines <- function(agg.data, supplier.name){
  cat.agg <- aggregate(Total.Sales ~ Categ.By.Year + Category + Year, agg.data, sum)
  cats.line <- ggplot(cat.agg, aes(x = Year, color = Category)) +
    geom_line(aes(y = Total.Sales), size = 3) + 
    theme_dark() + labs(y = "Sales", title = paste("Sales by Category for", supplier.name)) +
    scale_y_continuous(labels = scales::dollar)
  cats.line
}