library(shiny)
library(lubridate)
library(dplyr)

#read in data

#setwd("D://Users/SPritchard/Music/Documents/R/allocate/whaletale/")
#data4years <- read.csv("mutated.data.14.17decpart.csv", stringsAsFactors = FALSE)
#data4years <- read.csv("fake.wt.fixed.csv", stringsAsFactors = FALSE)  # attempting to replace with fake data file
data4years <- read.csv("fake.compressed.csv", stringsAsFactors = FALSE)  # attempting to replace with fake data file

#data4years <- read.csv("/srv/shiny-server/ab-trail/mutated.data1417.csv", stringsAsFactors = FALSE)
datatwoyears <- filter(data4years, Year %in% c(2017, 2016))

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
  #print(head(data.date.agg))
  prophet.data <- filter(data.date.agg, Supplier == supplier.name)
  #print(head(prophet.data, 10))
  prophet.data <- select(prophet.data, ds, y)
  model <- prophet(prophet.data, yearly.seasonality = TRUE)
  future <- make_future_dataframe(model, periods = 365)
  forecast <- predict(model, future)
  prophet.plot.obj <- prophet_plot_components(model, forecast)
  #print(prophet.plot.obj)
}

#shiny app
shinyServer(
  function(input, output){

    
    output$oid1 <- renderPrint({input$id1})
    output$oid2 <- renderPrint({input$id2})
    output$oid3 <- renderPrint({input$id3})
    output$oid4 <- renderPrint({
      if(input$id3 == TRUE){
        (input$id1)*(input$id2)*100
      } else{
        (input$id1)*(input$id2)
      }
    })
    #### use the scripts ####
    output$output.table <- renderTable({
      name2 <- input$id7
      supplier.kable(name2, data.object.mutated = datatwoyears)
    })
    
    
    output$outputagg.all <- renderPlot({
      name2 <- input$id7
      supplier.bar.yy(name2, data.object.mutated = data4years)
    })
    
    output$outputagg.cat <- renderPlot({
      name2 <- input$id7
      supplier.cat.bar.yy(name2, data.object.mutated = datatwoyears)
    })
    
    output$outputplot1 <- renderPlot({
      name2 <- input$id7
      line.graph.wt(name2, data.object.mutated = data4years)
    })
    
    output$outputplot3 <- renderPlot({
      name2 <- input$id7
      supplier.prophet(name2, data.object.mutated = data4years)
    })
    
  }
)