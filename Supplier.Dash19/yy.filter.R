# this function takes input8 from ui.R and implements it by filtering the data set to two comparative years as requested (ytd or full years)

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