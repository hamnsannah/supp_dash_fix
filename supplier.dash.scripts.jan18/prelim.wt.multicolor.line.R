
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