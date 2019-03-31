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
datatwoyears <- filter(data4years, Year %in% c(2017, 2016))

freemium.end.date <- date("2018-12-31")

#read in functions

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

supplier.bar.yy <- function(name, hierarchy = "Supplier", data.object.mutated){
  
  require(ggplot2)
  require(dplyr)
  require(lubridate)
  require(scales)
  
  data.both <- data.object.mutated
  
  supp.cat.agg <- aggregate(Total.Sales ~ Supplier + Categ.By.Year + Category + Year, data.both, sum)
  supp.agg <- aggregate(Total.Sales ~ Supplier + Year, data.both, sum)
  supp.agg$Year <- as.factor(supp.agg$Year)
  
  one.supp.all.agg <- filter(supp.agg, Supplier == name)
  one.supp.all.bar <- ggplot(data = one.supp.all.agg, aes(Year)) +
    geom_bar(aes(fill=Year, weight=Total.Sales)) + 
    #coord_flip() + 
    theme(legend.position = "none") + 
    ylab("Total Sales in $") + xlab("Year Over Year") + ggtitle(paste("Full Year Sales Comparison for", name))+
    scale_y_continuous(labels = scales::comma) +
    scale_fill_brewer(palette = "RdBu")+
    #scale_fill_manual(values = "RdBu") +
    theme_dark() +
    theme(plot.title = element_text(size = 18, face = "bold"))
  
  print(one.supp.all.bar)
}

supplier.cat.bar.yy <- function(name, hierarchy = "Supplier", data.object.mutated){
  
  require(ggplot2)
  require(dplyr)
  require(lubridate)
  require(scales)
  require(RColorBrewer)
  
  data.both<- data.object.mutated
  supp.cat.agg <- aggregate(Total.Sales ~ Supplier + Categ.By.Year + Category + Year, data.both, sum)
  
  one.supp.agg <- supp.cat.agg %>%
    filter(Supplier == name) %>%
    arrange(desc(Total.Sales))
  top20.cats <- unique(one.supp.agg$Category)[1:20]
  
  #print(top20.cats)
  one.supp.agg <- filter(one.supp.agg, Category %in% top20.cats)
  #print(head(one.supp.agg, 3))
  #print(dim(one.supp.agg))
  #print(tail(one.supp.agg, 6))
  
  one.supp.bar <- ggplot(data = one.supp.agg, aes(Categ.By.Year)) +
    geom_bar(aes(fill=Category, weight=Total.Sales)) + 
    coord_flip() + theme(legend.position = "none") + ylab("Total Sales in $") + xlab("Category") + 
    ggtitle(paste("Categories of", name, "for Last Two Years"))+
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = colorRampPalette(brewer.pal(20, "RdBu"))(length(top20.cats)),
                      guide = guide_legend()) +
    #scale_fill_distiller(palette = "BuPu")+
    theme_dark() +
    theme(plot.title = element_text(size = 18, face = "bold"))
  
  print(one.supp.bar)
}

line.graph.wt <- function(name, hierarchy = "Supplier", data.object.mutated){
  require(dplyr)
  require(ggplot2)
  require(lubridate)
  #require(viridis)
  
  data.both <- data.object.mutated
  supplier.time.agg <- aggregate(Total.Sales ~ Year + Month + Supplier, data.both, sum)
  supplier.time.agg$Year <- as.factor(supplier.time.agg$Year)
  supplier.time.agg$Month <- as.factor(supplier.time.agg$Month)
  
  supplier.t <- supplier.time.agg[supplier.time.agg$Supplier == name,]
  supp.plot.t <- ggplot(supplier.t, aes(x=Month,y=Total.Sales, group = Year)) +
    geom_line(aes(color = Year), size = 2) + ggtitle(paste("Trend Year Over Year for", supplier.t$Supplier[1]))+
    #scale_color_viridis(discrete = TRUE)
    #scale_color_manual(values=c("blue", "red"))+
    scale_color_brewer(palette= "RdBu") +
    #scale_color_manual(values=c("#000066", "#FF0000"))
    theme_dark() +
    theme(plot.title = element_text(size = 18, face = "bold"))
  print(supp.plot.t)
}

#2019 functions to read in
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
      labs(y = "Sales", title = "Total Sales from Selected Supplier by Month") +
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
      labs(y = "Sales", title = "Total Sales from Selected Supplier by Month") +
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
      labs(y = "Sales", title = "Total Sales from Selected Supplier by Month") +
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
      labs(y = "Sales", title = "Total Sales from Selected Supplier by Month") +
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
      labs(y = "Sales", title = "Total Sales from Selected Supplier by Month") +
      scale_y_continuous(labels = scales::dollar)
  }
  plot.list <- list(all.bar, all.line)
}

prelim.wt.cat.lines <- function(agg.data, supplier.name){
  cat.agg <- aggregate(Total.Sales ~ Categ.By.Year + Category + Year, agg.data, sum)
  cats.line <- ggplot(cat.agg, aes(x = Year, color = Category)) +
    geom_line(aes(y = Total.Sales), size = 3) + 
    theme_dark() + labs(y = "Sales", title = paste("Sales by Category for", supplier.name)) +
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

product.facet <- function(filtered.data, freemium.end.date){
  data.dive <- filtered.data
  data.dive$Product.By.Year <- paste(data.dive$Description, data.dive$Year)
  product.agg <- aggregate(Total.Sales ~ Product.By.Year + Description + Year, data.dive, sum)
  
  cy <- year(freemium.end.date)
  
  product.agg.18.vec <- product.agg %>% filter(Year == year(freemium.end.date)) %>%
    arrange(desc(Total.Sales)) %>%
    select(Description, Total.Sales)
  
  how.many.in.top <- 12 #select how many should be in plot, contingent on # of years
  product.top.cy.vec <- unique(product.agg.18.vec$Description)[1:how.many.in.top]
  product.agg.top <- product.agg[product.agg$Description %in% product.top.cy.vec,]
  
  gfacet <- ggplot(data = product.agg.top, aes(x = Year, y = Total.Sales, group = Description))+ 
    geom_line(color = "white", size = 3) + geom_point(color = "white", size = 3) +
    facet_wrap(~Description) + theme_dark()

  (gfacet)
}

product.table <- function(filtered.data, freemium.end.date, num.to.include){
  data.dive <- filtered.data
  data.dive$Product.By.Year <- paste(data.dive$Description, data.dive$Year)
  product.agg <- aggregate(Total.Sales ~ Product.By.Year + Description + Year + ItemID, data.dive, sum)
  product.agg <- mutate(product.agg, "Product" = paste0(Description, " (",ItemID,")"))
  
  cy <- year(freemium.end.date)
  py <- year(freemium.end.date)-1
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
  
  product.agg.cy.vec <- product.agg %>% filter(Year == year(freemium.end.date)) %>%
    arrange(desc(Total.Sales)) %>%
    select(Description, Total.Sales)
  
  #how.many.in.top <- 12 #select how many should be in plot, contingent on # of years
  #product.top.cy.vec <- unique(product.agg.cy.vec$Description)[1:how.many.in.top]
  #product.agg.top <- product.agg[product.agg$Description %in% product.top.cy.vec,]
  
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
    
    ts.reactive <- reactive({
      filt.data <- filtered.reactive()
      filtered.ts <- exploratory.jh.time.series(filt.data, freq = 12)
      #supplier.name <- input$id7
      #supplier.data <- filter(data4years, Supplier == supplier.name)
    })

    #### use the scripts ####
    output$output.table <- renderTable({
      name2 <- input$id7
      supplier.kable(name2, data.object.mutated = datatwoyears)
    })
    
    output$output.bar <- renderPlot({
      #supplier.name <- input$id7
      filt.data <- filtered.reactive()
      filt.ts <- ts.reactive()
      #filtered.data <- filter(data4years, Supplier == supplier.name)
      #filtered.ts <- exploratory.jh.time.series(filt.data, freq = 12)
      gg.list <- prelim.wt.multicolor.line(filt.data, filt.ts, unique(filt.data$Year))
      gg.list[1]
      

    })
    
    output$output.line <- renderPlot({
    #supplier.name <- input$id7
    #filtered.data <- filter(data4years, Supplier == supplier.name)
    
      filt.data <- filtered.reactive()
      filt.ts <- ts.reactive()
      #filtered.ts <- exploratory.jh.time.series(filt.data, freq = 12)
      gg.list <- prelim.wt.multicolor.line(filt.data, filt.ts, unique(filt.data$Year))
      gg.list[2]
    })
    
    output$output.cats <- renderPlot({
      filt.data <- filtered.reactive()
      supplier.name <- input$id7
      #filtered.data <- filter(data4years, Supplier == supplier.name)
      
      prelim.wt.cat.lines(filt.data, supplier.name)

    })
    
    output$dollar.kable <- renderTable({
      filt.data <- filtered.reactive()
      kable.cat.dollar(filt.data)
      
      #name2 <- input$id7
      #supplier.kable(name2, data.object.mutated = datatwoyears)
    })
    
    output$unit.kable <- renderTable({
      filt.data <- filtered.reactive()
      kable.cat.unit(filt.data)
    })
    
    output$facet.line <- renderPlot({
      #supplier.name <- input$id7
      #filtered.data <- filter(data4years, Supplier == supplier.name)
      
      filt.data <- filtered.reactive()
      #filt.ts <- ts.reactive()
      pfac <- product.facet(filt.data, freemium.end.date = freemium.end.date)
      #filtered.ts <- exploratory.jh.time.series(filt.data, freq = 12)
      #gg.list <- prelim.wt.multicolor.line(filt.data, filt.ts, unique(filt.data$Year))
      #gg.list[2]
      pfac
    })
    
    output$product.table <- renderTable({
      filt.data <- filtered.reactive()
      prod.t <- product.table(filt.data, freemium.end.date = freemium.end.date, num.to.include = 100)
    })
    
    #output$outputagg.all <- renderPlot({
    #  name2 <- input$id7
    #  supplier.bar.yy(name2, data.object.mutated = data4years)
    #})
    
    #output$outputagg.cat <- renderPlot({
    #  name2 <- input$id7
    #  supplier.cat.bar.yy(name2, data.object.mutated = datatwoyears)
    #})
    
    #output$outputplot1 <- renderPlot({
    #  name2 <- input$id7
    #  line.graph.wt(name2, data.object.mutated = data4years)
    #})
    
    #output$outputplot3 <- renderPlot({
    #  name2 <- input$id7
    #  supplier.prophet(name2, data.object.mutated = data4years)
    #})
    
  }
)