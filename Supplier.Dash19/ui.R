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

h5('This dashboard first has a summary table at the top followed by multiple views of the sales of the selected supplier.  
       As occasions arise to evaluate or re-evaluate suppliers, this can inform decisions about when to give more or less prominence to a supplier or some of its product categories.')
    
  ),
  mainPanel(
    h3('Analysis of Selected Supplier'),
    tableOutput("output.table"),
    tableOutput("output.table.adaptive"),
    plotOutput("output.bar", height = 500, width = 600),
    plotOutput("output.line",height = 500, width = 600),
    plotOutput("output.cats",height = 500, width = 600),
    tableOutput("dollar.kable"),
    tableOutput("unit.kable"),
    plotOutput("facet.line", height = 500, width = 600),
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