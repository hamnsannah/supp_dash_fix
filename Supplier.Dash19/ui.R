#ui.R for supplier dash shiny project
library(shiny)


#data17 <- read.csv("Detailed Sales Report 17.csv", stringsAsFactors = FALSE)
#data16 <- read.csv("Detailed Sales Report 16.csv", stringsAsFactors = FALSE)
#supplier.vector <- sort(unique(rbind(data17, data16)$Supplier))

## Not sure if this one was ever correct without a ton of subsetting
#supplier.vector <- read.csv("/srv/shiny-server/ab-trail/mutated.data1417.csv") # www5 server

## use this one to run on server##

#supplier.vector <- read.csv("/srv/shiny-server/supplier-dash/supplier.vec.csv", stringsAsFactors = FALSE)

## this data for R studio desktop##
#setwd("D://Users/SPritchard/Music/Documents/R/allocate/whaletale/")
#supplier.vector <- read.csv("supplier.vec.csv", stringsAsFactors = FALSE)

## fake data for sample version ##
#setwd("D://Users/SPritchard/Music/Documents/R/supp_dash_fix/")
#supplier.vector <- read.csv("fake.supplier.vec.csv", stringsAsFactors = FALSE)
supplier.vector <- read.csv("wt.supplier.vec.csv", stringsAsFactors = FALSE)

supplier.vector 

print(head(supplier.vector))
shinyUI(pageWithSidebar(
  headerPanel(" "),
  sidebarPanel(
    #h3('Enter Market and Test Date Here'),
    selectInput('id7', 'Supplier Name', choices = supplier.vector, selected = "ACOMO JEWELRY"),
    selectInput('id8','Select How Years Are Compared', choices = c("Year-To-Date vs. Full Prior Year", 
                                                                "Year-To-Date vs. Year-To-Date Prior Year", 
                                                                "Last Full Year vs. Prior Year")),

h5('This dashboard provides multiple views of the sales of the selected supplier.  
       It shows whether a supplier is outperforming or underperforming prior years and which categories and products are driving that trend.')
    
  ),
  mainPanel(
    h3('Analysis of Selected Supplier'),
    tableOutput("output.table.adaptive"),
    h4('Sales from Selected Supplier by Year'),
    plotOutput("output.bar", height = 500, width = 600),
    h4('Sales from Selected Supplier by Month'),
    plotOutput("output.line",height = 500, width = 600),
    h4('Sales from Selected Supplier by Category'),
    plotOutput("output.cats",height = 500, width = 600),
    h4('Sales Dollars by Category'),
    tableOutput("dollar.kable"),
    h4('Units Sold by Category'),
    tableOutput("unit.kable"),
    h4('Trend of Top Products in Current Year'),
    plotOutput("facet.line", height = 500, width = 600),
    h4('Performance of Top Products Year Over Year'),
    tableOutput("product.table")
    #plotOutput("outputagg.all", height = 500, width = 600),
    #plotOutput("outputagg.cat", height = 500, width = 600),
    #h3('Overall Shape of Sales Each Year'),
    #plotOutput("outputplot1", height = 500, width = 600),
    #h4('Seasonality Trends for Supplier')
    #plotOutput("outputplot3", height = 800, width = 600)
    #h4('Comparison of Sales By Category 17 vs. 16'),
    #h4('Comparison of Sales For Supplier Top 25 Items')
  )
))