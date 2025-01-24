library(tidyverse)


library(lubridate)
library(openintro)
library(maps)
library(ggmap)
library(ggthemes)

mapdata <- map_data("world")
view(mapdata)

HR_df <- read_csv("IHME_GBD_2019_HRH_1990_2019_DATA_Y2022M06D03 2.CSV")

#Focus on Data of Physicians in Pak and USA
HR_PaknUSA_df <- HR_df %>% filter(location_name == "Pakistan" | location_name == "United States of America", cadre == 'Physicians' )
View(HR_PaknUSA_df)

#HR_df <- HR_df %>% rename(location_name = Country)

colnames(HR_PaknUSA_df)[colnames(HR_PaknUSA_df)=="location_name"] <- "Country"

graph <- HR_PaknUSA_df %>%  ggplot(aes(x = year_id, y = mean, color = Country)) +
         geom_point() +
        geom_smooth() +
        theme(legend.position = "bottom")

graph + 
  labs(title = "Average # of Physicians per 10,000 from 1990-2019 in Pakistan and USA", x = "Year" , y = "average # of Physicians per 10,000")


clinic_Loc <- read_csv("pakistan.csv")

#Latitude,Longitude
colnames(clinic_Loc)[colnames(clinic_Loc)=="X"] <- "Longitude"
colnames(clinic_Loc)[colnames(clinic_Loc)=="Y"] <- "Latitude"


#https://www.youtube.com/watch?v=2k8O-Y_uiRU
#https://www.openstreetmap.org/export#map=7/30.600/73.367&layers=C
#http://maps.stamen.com/#watercolor/12/37.7706/-122.3782

clinic_Loc <- clinic_Loc[!grepl("dentist", clinic_Loc$amenity),] %>% drop_na('amenity')
View(clinic_Loc)

Pak_Map <- get_stamenmap(
        bbox = c(left = 60.546 , bottom = 23.604, right = 78.299, top = 37.440),
        maptype = "toner-hybrid",
        zoom = 6
)
#find way to remove all rows with dentists in column amenities


ggmap(Pak_Map) +
    geom_point(data = clinic_Loc,
               aes(x = Longitude, y = Latitude, color = amenity),
               size = 1) +
              theme_map() +
              theme(legend.background = element_blank())
              


View(clinic_Loc)


