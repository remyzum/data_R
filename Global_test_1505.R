library(ggplot2)
library(questionr)
library(dplyr)
library(ggmap)
library(readr)
library(lubridate)
library(magrittr)
library(shiny)
library(leaflet)
library(rCharts)
library(rjson)

# Set working directory
getwd()
setwd("/Users/admin/Desktop/Master_DEsiGEO/EcoleDesPonts/TP_Data/SanFransisco_data/Shiny")

# Import csv et Separation année mois jour heure 
coltypes <-
  list(Dates = col_datetime("%Y-%m-%d %H:%M:%S"))

train <-
  read_csv(file="/Users/admin/Desktop/Master_DEsiGEO/EcoleDesPonts/TP_Data/SanFransisco_data/train.csv",
           col_types=coltypes)
train <-
  train %>% mutate(Year  = factor(year(Dates), levels=2003:2015), Month = factor(month(Dates), levels=1:12),
                    Month = month.abb[Month],
                   Day  = day(Dates),
                   Hour  = factor(hour(Dates), levels=0:23),
                   dayDate = as.POSIXct(round(Dates, units = "days")),
                   DayOfWeek = factor(DayOfWeek, levels=c("Monday",
                                                "Tuesday",
                                                "Wednesday",
                                                "Thursday",
                                                "Friday",
                                                "Saturday",
                                                "Sunday"))
          )

# Classer par catégorie et compter les nombres de délits par catégorie
counts <- summarise(group_by(train, Category), Counts=length(Category))

# Compter le nombre de délit par catégorie du + au -
counts <- counts[order(-counts$Counts),]

# Ne garde que les 10 premières catégories et retire "Other Offenses" categorie avec (c(1,3:13))
top10 <- train[train$Category %in% counts$Category[c(1,3:12,17,20,31)],]
#category_10 <- unique(c(as.character(top10$Category))) ## Categorie de crimes

# Ajouter colonne avec intervalle de temps
top10$FourHours <- top10$Hour
levels(top10$FourHours) <- c(levels(top10$FourHours), "00H-04H")
top10[top10$Hour %in% c("0", "1", "2", "3"), "FourHours"] <- "00H-04H"
levels(top10$FourHours) <- c(levels(top10$FourHours), "04H-08H")
top10[top10$Hour %in% c("4", "5", "6", "7"), "FourHours"] <- "04H-08H"
levels(top10$FourHours) <- c(levels(top10$FourHours), "08H-12H")
top10[top10$Hour %in% c("8", "9", "10", "11"), "FourHours"] <- "08H-12H"
levels(top10$FourHours) <- c(levels(top10$FourHours), "12H-16H")
top10[top10$Hour %in% c("12", "13", "14", "15"), "FourHours"] <- "12H-16H"
levels(top10$FourHours) <- c(levels(top10$FourHours), "16H-20H")
top10[top10$Hour %in% c("16", "17", "18", "19"), "FourHours"] <- "16H-20H"
levels(top10$FourHours) <- c(levels(top10$FourHours), "20H-00H")
top10[top10$Hour %in% c("20", "21", "22", "23"), "FourHours"] <- "20H-00H"

data3 <- top10[,c(2,8,9,4,12,11,10,13,14,15)]
names(data3)[1]<-"Name"
names(data3)[2]<-"Long"
names(data3)[3]<-"Lat"

setwd("/Users/admin/Desktop/Master_DEsiGEO/EcoleDesPonts/TP_Data/SanFransisco_data/Shiny")
write.csv2(data3, file ="data3.csv", row.names=FALSE)

typeof(data3$FourHours)

counts
