##    Programme:  R
##
##    Objective: Map reported crime in Wellington City.
##
##    Plan of  : 
##    Attack   :  
##
##               1. Load in data.
##               2. Tidy up.
##               3. Visualisation.
##
##    Author   :  Bradd Forster
##
##  Clear the decks and load up some functionality
    ##
    rm(list=ls(all=TRUE))

##  Core libraries
    ##
    library(dplyr)
    library(tidyr)
    library(readxl)
    library(writexl)
    library(stringr)
    library(lubridate)
    library(ggplot2)

##  Optional libraries
    ##
    library(plotly)
    
##  Set up paths for working directories
    ##
    userid <- ""
    r <- paste0("C:/Users/", userid, "")
    data <- paste0("C:/Users/", userid, "")
    
##  Setting the working directory
    ##
    setwd(data)

################################################################################
## 1.                             Load in data                                ##
################################################################################
    
raw <- read.csv("Demand_Activity_Public_Tab_Migrated Data.csv")


################################################################################
## 2.                                Tidy up                                  ##
################################################################################

df <- raw[raw$Occurrence.Type.Category == "Offence", ]  # keeping the offences

df$Year <- as.numeric(str_sub(df$Year.Month, start = str_length(df$Year.Month)-3, end = str_length(df$Year.Month)))  # creating a Year variable 

df$Month <- str_sub(df$Year.Month, start = 1, end = str_locate(df$Year.Month, "[0-9]")[1]-1)

df$Date <- as.Date(paste0(df$Year, "-", df$Month, "-1"), "%Y-%b-%d")  # create a date variable

df$Ta <- str_remove(df$Territorial.Authority, "\\.")  # remove full stop in TA variable

total <- aggregate(Events.Occurrences ~ Occurrence.Division + Date + Ta, data = df, sum)  # total by crime (division), year, month, and teritorial authority

colnames(total) <- c("Crime", "Date", "Ta", "Number")

################################################################################
## 3.                            Visualisation                                ##
################################################################################
place <- "Wellington City"


ggplotly(ggplot(data = total[total$Ta == place, ]) +
  geom_line(aes(x = Date, y = Number, color = Crime)) +
    labs(y = "Reported Crime Count", x = NULL, title = paste0(place)) +
    theme(
      plot.background = element_rect(fill = "#132E35", color = NA),
      panel.background = element_rect(fill = "#132E35", color = NA),
      legend.background = element_rect(fill = "#132E35", color = NA),
      legend.text = element_text(color = "#AFB3B7"),
      legend.title = element_text(color = "#AFB3B7"),
      axis.title = element_text(color = "#AFB3B7"),
      axis.text = element_text(color = "#AFB3B7"),
      title = element_text(color = "#AFB3B7")
    ))