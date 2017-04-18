####################
## Purpose: To parse Weather information from www.apixu.com and output current and 
##          forecast weather data to csv files
## Author: Adam Sampson
## Organization: Bellarmine University, Masters of Analytics Program
## Date: April 2017
## Function: forecast(zip,days)
####################

#test values for running through the code without running full function
#zip='40202'
#days=7

#declare function forecast and set defaults for zip and days
forecast <- function(zip = '40202',days = 7) {
  
  #load libraries if needed
  require(stringr)
  require(jsonlite)
  require(XML)
  require(dplyr)
  
  #report errors if unacceptable inputs are passed
  if ((days > 10)|(days<3)) stop("Days of forecast data requested must be between 3 and 10")
  if ((str_detect(zip,"\\D")|(str_length(zip)!=5))) stop("Zip code must be a 5 digit number")
  
  #put together the urls for current and forecast (made for easy changing later)
  base_url <- "http://api.apixu.com/v1"
  base_curr_url <- "/current.xml"
  base_fore_url <- "/forecast.json"
  api_key <- "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" #intentionally masked...get your own key...
  curr_url <- paste0(base_url,base_curr_url,"?key=",api_key,"&q=",zip)
  fore_url <- paste0(base_url,base_fore_url,"?key=",api_key,"&q=",zip,"&days=",as.character(days))
  
  #####
  ##get current weather in XML format
  #At work I have proxy issues and have to use the following for troubleshooting
  #current <- xmlParse("current.xml")
  current <- xmlParse(curr_url,isURL = TRUE)
  current <- xmlRoot(current)
  
  #fix the issue with multiple rows by putting <location> inside <current>
  xmlParent(xmlChildren(current[["current"]])) <- current[["location"]]
  
  #convert current info to dataframe
  current <- xmlToDataFrame(current[["current"]])
  
  #####
  ##get forecast in JSON format and convert to data frame
  forecast_df <- as.data.frame(fromJSON(url(fore_url),flatten = TRUE))
  
  #####
  ##re-arrange, filter, and clean-up dataframes to match desired output
  current <- transmute(current,
                          zip = zip,
                          name = current$'name',
                          region = current$'region',
                          country = current$'country',
                          last_updated = current$'last_updated',
                          temp_f = current$'temp_f',
                          wind_mph = current$'wind_mph',
                          wind_dir = current$'wind_dir',
                          precip_in = current$'precip_in',
                          humidity = current$'humidity',
                          cloud = current$'cloud',
                          vis_miles = current$'vis_miles'
                          )
  
  forecast_df <- transmute(forecast_df,
                        zip = zip,
                        name = forecast_df$'location.name',
                        region = forecast_df$'location.region',
                        country = forecast_df$'location.country',
                        last_updated = forecast_df$'current.last_updated',
                        forecast.date = forecast_df$'forecast.forecastday.date',
                        forecast.maxtemp_f = forecast_df$'forecast.forecastday.day.maxtemp_f',
                        forecast.mintemp_f = forecast_df$'forecast.forecastday.day.mintemp_f',
                        forecast.avgtemp_f = forecast_df$'forecast.forecastday.day.avgtemp_f',
                        forecast.maxwind_mph = forecast_df$'forecast.forecastday.day.maxwind_mph'
                        )
  
  #####
  ##create CSV files in the current working directory
  current_filename <- paste0("Current Weather for ",zip," on ",
                             unlist(strsplit(as.character(current$last_updated[1])," "))[1],".csv")
  forecast_filename <- paste0(days," Day Weather Forecast for ",zip," as of ",
                              unlist(strsplit(as.character(forecast_df$last_updated[1])," "))[1],".csv")
  write.csv(current,file = current_filename,row.names = FALSE)
  write.csv(forecast_df, file = forecast_filename,row.names = FALSE)
}
