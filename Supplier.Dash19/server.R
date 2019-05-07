#need to keep working supplier.summary.table.R this to receive the output from yy.filter

#was able to get old dash set up with new wt data and supplier.vec. 
#I disabled the prophet functions because they were useless.
#Most of the way there with product.facet.R but not confident in filtering because too many facets missing lines, ergo probably not top 10
# ^ still an issue.  Note that been changing functions in Shiny but not in original source file, so fix that.

library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(fpp2)
library(knitr)

#read in data

#setwd("D://Users/SPritchard/Music/Documents/R/allocate/whaletale/")
#data4years <- read.csv("mutated.data.14.17decpart.csv", stringsAsFactors = FALSE)
#data4years <- read.csv("fake.wt.fixed.csv", stringsAsFactors = FALSE)  # attempting to replace with fake data file
#data4years <- read.csv("fake.compressed.csv", stringsAsFactors = FALSE)  # attempting to replace with fake data file
#data4years <- read.csv("fake.monthly.csv", stringsAsFactors = FALSE)  # attempting to replace with fake data file
data4years <- read.csv("sales.data.wt.monthly.csv", stringsAsFactors = FALSE)  # attempting to replace with fake data file
data4years <- filter(data4years, Year >= 2015)

#data4years <- read.csv("/srv/shiny-server/ab-trail/mutated.data1417.csv", stringsAsFactors = FALSE)
#datatwoyears <- filter(data4years, Year %in% c(2017, 2016))

freemium.end.date <- date("2019-04-30")
data4years <- filter(data4years, Date.Sold <= freemium.end.date)
#read in functions

#2019 functions to read in

supplier.kable.any.year <- function(name, data.object.mutated){
  require(knitr)
  require(dplyr)
  data.both.supplier <- data.object.mutated
  data.both.supplier$Supp.By.Year <- paste(data.both.supplier$Supplier, data.both.supplier$Year)
  
  supplier.agg <- aggregate(Total.Sales ~ Supp.By.Year + Supplier + Year, data.both.supplier, sum)
  supplier.agg.cy <- filter(supplier.agg, Year == max(supplier.agg$Year))
  supp.cy.sum <- sum(supplier.agg.cy$Total.Sales)
  supplier.agg.cy <- mutate(supplier.agg.cy, "Perc.Whole" = round((Total.Sales/supp.cy.sum)*100, 2))
  supplier.agg.cy <- arrange(supplier.agg.cy, desc(Total.Sales))
  
  supplier.agg.py <- filter(supplier.agg, Year == min(supplier.agg$Year))
  sup.merge.py <- supplier.agg.py[,c(2,4)]
  colnames(sup.merge.py) <- c("Supplier", "Prior")
  sup.merge <- merge(supplier.agg.cy, sup.merge.py, by.x="Supplier", all.x=TRUE, all.y=FALSE)
  sup.merge <- mutate(sup.merge, "Growth" = Total.Sales - Prior, "Perc.Growth" = paste0(round((Growth/Prior)*100,1),"%")) %>%
    arrange(desc(Total.Sales)) %>%
    select(1,4:8)
  colnames(sup.merge)[c(2,4)] <- c("Sales.Current.Yr", "Sales.Prior.Yr")
  supplier.agg.cy.pretty <- sup.merge 
  supplier.agg.cy.pretty$Perc.Whole <- paste0(supplier.agg.cy.pretty$Perc.Whole, "%")
  supplier.agg.cy.pretty$Growth <- paste0("$",prettyNum(round(supplier.agg.cy.pretty$Growth), big.mark = ","))
  supplier.agg.cy.pretty$Sales.Prior.Yr <- paste0("$",prettyNum(round(supplier.agg.cy.pretty$Sales.Prior.Yr), big.mark = ","))
  supplier.agg.cy.pretty$Sales.Current.Yr <- paste0("$",prettyNum(round(supplier.agg.cy.pretty$Sales.Current.Yr), big.mark = ",")) ## Switch back
  supplier.agg.cy.pretty$Rank.Current.Yr <- rownames(supplier.agg.cy.pretty)
  supplier.agg.cy.pretty <- select(supplier.agg.cy.pretty, Supplier, Rank.Current.Yr, Sales.Current.Yr, Sales.Prior.Yr:Perc.Growth, Perc.Whole)
  colnames(supplier.agg.cy.pretty) <- c("Supplier", "Rank Current Yr", "Sales Current Yr", "Sales Prior Yr", "$ Growth", "% Growth", "% of All Sales")
  filter(supplier.agg.cy.pretty, Supplier == name)
  
}

yy.filter <- function(yy.filter.input, data.to.use){
  require(lubridate)
  data.to.use$Date.Sold <- date(data.to.use$Date.Sold)
  max.data.date <- max(data.to.use$Date.Sold)
  
  if(yy.filter.input == "Year-To-Date vs. Full Prior Year"){
    df.cy <- filter(data.to.use, Date.Sold >= floor_date(max.data.date, unit = "years"))
    df.py <- filter(data.to.use, Date.Sold >= (floor_date(max.data.date, unit = "years")-years(1)), 
                    Date.Sold <= (floor_date(max.data.date, unit = "years")-days(1)))
    yy.df <- rbind(df.cy, df.py)
    
  }else if(yy.filter.input == "Year-To-Date vs. Year-To-Date Prior Year"){ #done
    df.cy <- filter(data.to.use, Date.Sold >= floor_date(max.data.date, unit = "years"))
    df.py <- filter(data.to.use, Date.Sold >= floor_date((max.data.date - years(1)), unit = "years"),
                    Date.Sold <= (max.data.date - years(1)))
    yy.df <- rbind(df.cy, df.py)
    
  }else if(yy.filter.input == "Last Full Year vs. Prior Year"){
    df.cy <- filter(data.to.use, Date.Sold >= (floor_date(max.data.date, unit = "years")-years(1)), 
                    Date.Sold <= (floor_date(max.data.date, unit = "years")-days(1)))
    df.py <- filter(data.to.use, Date.Sold >= floor_date((max.data.date - years(2)), unit = "years"),
                    Date.Sold <= (max.data.date - years(1) - days(1)))
    yy.df <- rbind(df.cy, df.py)
  }
  yy.df
}

exploratory.jh.time.series <- function(clean.df, freq = 365){
  # includes day, week, and year frequency options
  if(freq == 52){print("Error: use freq = 53 for weekly to account for partial week at EOY")}
  require(lubridate)
  require(dplyr)
  clean.df <- arrange(clean.df, Date.Sold)
  clean.df$Day <- date(clean.df$Date.Sold) # df should have been arranged in cleaning step but doing it here too in case not
  start.year <- year(clean.df$Day)[1]
  
  if(freq == 365){
    start.day <- date(head(clean.df$Day,1)) - floor_date(head(clean.df$Day,1), unit = "year")+1 # subtracts earliest day from first day of year plus one
    sales.agg <- aggregate(Total.Sales ~ Day, clean.df, sum)
    all.dates <- seq.Date(from = as.Date(min(sales.agg$Day, na.rm = TRUE)), to = as.Date(max(sales.agg$Day, na.rm = TRUE)),by = 1)
    sales.agg.all <- left_join(data.frame(Day = all.dates), sales.agg)
    sales.ts <- ts(sales.agg.all$Total.Sales, start = c(start.year, start.day), frequency = 365)
  }
  if(freq == 12){
    start.month <- month(head(clean.df$Month,1))
    sales.agg <- aggregate(Total.Sales ~ Month + Year + Day, clean.df, sum)
    print(head(sales.agg, 25))
    first.of.month.seq <- seq.Date(from = as.Date(min(sales.agg$Day, na.rm = TRUE)), to = as.Date(max(sales.agg$Day, na.rm = TRUE)), by = "month")
    first.of.month.df <- data.frame("Day" = first.of.month.seq, "Month" = month(first.of.month.seq), "Year" = year(first.of.month.seq))
    sales.agg.all <- left_join(first.of.month.df, sales.agg)
    sales.ts <- ts(sales.agg.all$Total.Sales, start = c(start.year, start.month), frequency = 12)
  }
  if(freq == 53){
    clean.df$Week <- week(clean.df$Date.Sold)
    start.week <- head(clean.df$Week,1)
    sales.agg <- aggregate(Total.Sales ~ Week + Year + Day, clean.df, sum)
    print(head(sales.agg, 25))
    first.of.week.seq <- seq.Date(from = as.Date(min(sales.agg$Day, na.rm = TRUE)), to = as.Date(max(sales.agg$Day, na.rm = TRUE)), by = "week")
    first.of.week.df <- data.frame("Day" = first.of.week.seq, "Week" = week(first.of.week.seq), "Year" = year(first.of.week.seq))
    sales.agg.all <- left_join(first.of.week.df, sales.agg)
    #sales.agg.all <- left_join(sales.agg, data.frame(Week = 1:53))
    sales.ts <- ts(sales.agg.all$Total.Sales, start = c(start.year, start.week), frequency = 53)
    
    
  }
  sales.ts[is.na(sales.ts)] <- 0
  
  sales.ts
}

prelim.wt.multicolor.line <- function(agg.data, ts.data, years.in.data){
  #years.in.data should be a vector of the actual years as numerics
  
  if(length(years.in.data) == 1){
    
    man.pal <- c("#67a9cf") #manual palette
    
    all.bar <- ggplot(data = agg.data, aes(y = Total.Sales, x = factor(Year), fill = factor(Year))) + 
      geom_bar(stat = "identity") + scale_y_continuous(labels = scales::dollar) + 
      scale_fill_manual(name = "Year", values = man.pal) + theme_dark()+
      labs(x = "Year", y = "Sales") + 
      theme(legend.position="none")
    
    year1.ts <- ts.data
    
    all.line <- autoplot(ts.data) + autolayer(year1.ts, color = man.pal[1], size = 3) + theme_dark()+ 
      #labs(title = "Total Sales from Selected Supplier by Month") + # removed title because including them in ui.R
      labs(y = "Sales") +
      scale_y_continuous(labels = scales::dollar)
    
  }
  
  if(length(years.in.data) == 2){
    
    man.pal <- c("#f7f7f7", "#67a9cf") #manual palette
    
    all.bar <- ggplot(data = agg.data, aes(y = Total.Sales, x = factor(Year), fill = factor(Year))) + 
      geom_bar(stat = "identity") + scale_y_continuous(labels = scales::dollar) + 
      scale_fill_manual(name = "Year", values = man.pal) + theme_dark()+
      labs(x = "Year", y = "Sales") + theme(legend.position="none")
    
    
    year1.ts <- window(ts.data, end = c(years.in.data[2],1))
    year2.ts <- window(ts.data, start = c(years.in.data[2],1))
    all.line <- autoplot(ts.data) + autolayer(year1.ts, color = man.pal[1], size = 3) + 
      autolayer(year2.ts, color = man.pal[2], size = 3) + theme_dark()+ 
      #labs(title = "Total Sales from Selected Supplier by Month") + # removed title because including them in ui.R
      labs(y = "Sales") +
      scale_y_continuous(labels = scales::dollar)
  }
  
  if(length(years.in.data) == 3){
    
    all.bar <- ggplot(data = agg.data, aes(y = Total.Sales, x = factor(Year), fill = factor(Year))) + 
      geom_bar(stat = "identity") + scale_y_continuous(labels = scales::dollar) + 
      scale_fill_brewer(name = "Year", palette = "RdBu") + theme_dark()+
      labs(x = "Year", y = "Sales") + theme(legend.position="none")
    
    man.pal <- c("#ef8a62", "#f7f7f7", "#67a9cf") #manual palette
    year1.ts <- window(ts.data, end = c(years.in.data[2],1))
    year2.ts <- window(ts.data, start = c(years.in.data[2],1), end = c(years.in.data[3],1))
    year3.ts <- window(ts.data, start = c(years.in.data[3],1))
    
    all.line <- autoplot(ts.data) + autolayer(year1.ts, color = man.pal[1], size = 3) + 
      autolayer(year2.ts, color = man.pal[2], size = 3) + 
      autolayer(year3.ts, color = man.pal[3], size = 3) + theme_dark() + 
      #labs(title = "Total Sales from Selected Supplier by Month") + # removed title because including them in ui.R
      labs(y = "Sales") +
      scale_y_continuous(labels = scales::dollar)
    
  }
  
  if(length(years.in.data) == 4){
    #c(#ca0020, #f4a582, #92c5de, #0571b0)
    
    all.bar <- ggplot(data = agg.data, aes(y = Total.Sales, x = factor(Year), fill = factor(Year))) + 
      geom_bar(stat = "identity") + scale_y_continuous(labels = scales::dollar) + 
      scale_fill_brewer(name = "Year", palette = "RdBu") + theme_dark()+
      labs(x = "Year", y = "Sales") + theme(legend.position="none")
    
    man.pal <- c("#ca0020", "#f4a582", "#92c5de", "#0571b0") #manual palette
    year1.ts <- window(ts.data, end = c(years.in.data[2],1))
    year2.ts <- window(ts.data, start = c(years.in.data[2],1), end = c(years.in.data[3],1))
    year3.ts <- window(ts.data, start = c(years.in.data[3],1), end = c(years.in.data[4],1))
    year4.ts <- window(ts.data, start = c(years.in.data[4],1))
    
    all.line <- autoplot(ts.data) + autolayer(year1.ts, color = man.pal[1], size = 3) + 
      autolayer(year2.ts, color = man.pal[2], size = 3) + 
      autolayer(year3.ts, color = man.pal[3], size = 3) + 
      autolayer(year4.ts, color = man.pal[4], size = 3) + theme_dark()+ 
      #labs(title = "Total Sales from Selected Supplier by Month") + # removed title because including them in ui.R
      labs(y = "Sales") +
      scale_y_continuous(labels = scales::dollar)
  }
  
  if(length(years.in.data) == 5){
    #c(#ca0020, #f4a582, #f7f7f7, #92c5de, #0571b0)
    
    all.bar <- ggplot(data = agg.data, aes(y = Total.Sales, x = factor(Year), fill = factor(Year))) + 
      geom_bar(stat = "identity") + scale_y_continuous(labels = scales::dollar) + 
      scale_fill_brewer(name = "Year", palette = "RdBu") + theme_dark()+
      labs(x = "Year", y = "Sales") + theme(legend.position="none")
    
    man.pal <- c("#ca0020", "#f4a582", "#f7f7f7","#92c5de", "#0571b0") #manual palette
    year1.ts <- window(ts.data, end = c(years.in.data[2],1))
    year2.ts <- window(ts.data, start = c(years.in.data[2],1), end = c(years.in.data[3],1))
    year3.ts <- window(ts.data, start = c(years.in.data[3],1), end = c(years.in.data[4],1))
    year4.ts <- window(ts.data, start = c(years.in.data[4],1), end = c(years.in.data[5],1))
    year5.ts <- window(ts.data, start = c(years.in.data[5],1))
    
    all.line <- autoplot(ts.data) + autolayer(year1.ts, color = man.pal[1], size = 3) + 
      autolayer(year2.ts, color = man.pal[2], size = 3) + 
      autolayer(year3.ts, color = man.pal[3], size = 3) + 
      autolayer(year4.ts, color = man.pal[4], size = 3) +
      autolayer(year5.ts, color = man.pal[5], size = 3) + theme_dark()+ 
      #labs(title = "Total Sales from Selected Supplier by Month") + # removed title because including them in ui.R
      labs(y = "Sales") +
      scale_y_continuous(labels = scales::dollar)
  }
  plot.list <- list(all.bar, all.line)
}

prelim.wt.cat.lines <- function(agg.data, supplier.name){
  cat.agg <- aggregate(Total.Sales ~ Categ.By.Year + Category + Year, agg.data, sum)
  cats.line <- ggplot(cat.agg, aes(x = Year, color = Category)) +
    geom_line(aes(y = Total.Sales), size = 3) + 
    theme_dark() + labs(y = "Sales") +
    #labs(title = paste("Sales by Category for", supplier.name)) + #removed title and added in ui.R
    scale_y_continuous(labels = scales::dollar)
  cats.line
}

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

kable.cat.unit <- function(filtered.data){
  require(knitr)
  cat.unit <- aggregate(Total.Sales ~ Categ.By.Year + Category + Year, filtered.data, length)
  cat.unit <- arrange(cat.unit, desc(Year))
  cat.year.vec <- unique(cat.unit$Year)
  cat.kable.unit <- data.frame("Category" = unique(cat.unit$Category))
  
  for(i in 1:length(cat.year.vec)){
    cat.unit.i <- cat.unit %>% filter(Year == cat.year.vec[i]) %>% select(2,4)#%>% rename("2018" = Total.Sales)
    cat.unit.i[,2] <- paste0(" ", prettyNum(round(cat.unit.i[,2]), big.mark = ","))
    colnames(cat.unit.i)[2] <- cat.year.vec[i]
    cat.kable.unit <- full_join(cat.kable.unit, cat.unit.i)
    
  }
  
  #colnames(cat.kable.unit) <- c("Category", cat.year.vec)
  cat.kable.unit[is.na(cat.kable.unit)] <- 0
  #cat.kable.unit <- (cat.kable.unit)
  cat.kable.unit
}

product.facet <- function(filtered.data){
  data.dive <- filtered.data
  data.dive$Product.By.Year <- paste(data.dive$Description, data.dive$Year)
  product.agg <- aggregate(Total.Sales ~ Product.By.Year + Description + Year, data.dive, sum)
  
  cy <- year(max(filtered.data$Date.Sold))
  
  #py <- year(freemium.end.date)-1
  #product.agg.cy <- filter(product.agg, Year == cy)
  #product.cy.sum <- sum(product.agg.cy$Total.Sales)
  #product.agg.cy <- mutate(product.agg.cy, "Perc.Whole" = round((Total.Sales/product.cy.sum)*100, 2))
  #product.agg.cy <- arrange(product.agg.cy, desc(Total.Sales))
  
  #product.agg.cy <- rename(product.agg.cy, "Product" = Description)
  #product.agg.py <- filter(product.agg, Year == py)
  #product.merge.py <- product.agg.py[,c(2,4)]
  #colnames(product.merge.py) <- c("Product", "Prior Yr")
  #product.merge <- merge(product.agg.cy, product.merge.py, by.x="Product", all.x=TRUE, all.y=FALSE)
  #product.merge <- mutate(product.merge, "Growth" = Total.Sales - `Prior Yr`, "Perc.Growth" = 
  #                          paste0(round((Growth/`Prior Yr`)*100,1),"%")) %>%
  #  arrange(desc(Total.Sales)) %>%
  #  select(1,4:8)
  #colnames(product.merge)[c(2,4)] <- c("Sales Current Yr", "Sales Prior Yr")
  #product.merge[is.na(product.merge)] <- 0
  
  product.agg.cy.vec <- product.agg %>% filter(Year == cy) %>%
    arrange(desc(Total.Sales)) %>%
    select(Description, Total.Sales)
  
  how.many.in.top <- 12 #select how many should be in plot, contingent on # of years
  product.top.cy.vec <- unique(product.agg.cy.vec$Description)[1:how.many.in.top]
  product.agg.top <- product.agg[product.agg$Description %in% product.top.cy.vec,]
  
  gfacet <- ggplot(data = product.agg.top, aes(x = Year, y = Total.Sales, group = Description))+ 
    geom_line(color = "white", size = 3) + geom_point(color = "white", size = 3) +
    facet_wrap(~Description) + theme_dark()
  
  #g <- ggplot(data = product.agg.top, aes(x = Product.By.Year, fill = factor(Year))) + 
  #geom_bar(stat = "identity", aes(y = Total.Sales)) + coord_flip() +
  #scale_y_continuous(labels = scales::dollar) +
  #scale_fill_brewer(direction = 1, palette = "RdBu", name = "Year") + theme_dark() + 
  #labs(title = "Sales From Top Products By Year", y = "Total Sales", x = "Product By Year") 
  (gfacet)
}
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

#shiny app
shinyServer(
  function(input, output){
    
    filtered.reactive <- reactive({
      supplier.name <- input$id7
      supplier.data <- filter(data4years, Supplier == supplier.name)
    })
    
    two.years.reactive <- reactive({
      yy.filter.to.use <- input$id8
      two.years.data <- yy.filter(yy.filter.input = yy.filter.to.use, data.to.use = data4years)
    })

    
    ts.reactive <- reactive({
      filt.data <- filtered.reactive()
      filtered.ts <- exploratory.jh.time.series(filt.data, freq = 12)
    })
    
    output$output.table.adaptive <- renderTable({
      #filt.data <- filtered.reactive()
      #yy.filter.to.use <- input$id8
      two.years <- two.years.reactive()
      supplier.name <- input$id7
      #two.years.data <- yy.filter(yy.filter.input = yy.filter.to.use, data.to.use = data4years)
      supplier.kable.any.year(name = supplier.name, two.years)
      #supplier.data <- filter(data4years, Supplier == supplier.name)
    })
    
    output$output.bar <- renderPlot({
      filt.data <- filtered.reactive()
      filt.ts <- ts.reactive()
      gg.list <- prelim.wt.multicolor.line(filt.data, filt.ts, unique(filt.data$Year))
      gg.list[1]
    })
    
    output$output.line <- renderPlot({
      filt.data <- filtered.reactive()
      filt.ts <- ts.reactive()
      #filtered.ts <- exploratory.jh.time.series(filt.data, freq = 12)
      gg.list <- prelim.wt.multicolor.line(filt.data, filt.ts, unique(filt.data$Year))
      gg.list[2]
    })
    
    output$output.cats <- renderPlot({
      filt.data <- filtered.reactive()
      supplier.name <- input$id7
      prelim.wt.cat.lines(filt.data, supplier.name)

    })
    
    output$dollar.kable <- renderTable({
      filt.data <- filtered.reactive()
      kable.cat.dollar(filt.data)
    })
    
    output$unit.kable <- renderTable({
      filt.data <- filtered.reactive()
      kable.cat.unit(filt.data)
    })
    
    output$facet.line <- renderPlot({
      filt.data <- filtered.reactive()
      pfac <- product.facet(filt.data)
      pfac
    })
    
    output$product.table <- renderTable({
      two.years <- two.years.reactive()
      #filt.data <- filtered.reactive()
      prod.t <- product.table(two.years, num.to.include = 100)
    })
    
  }
)