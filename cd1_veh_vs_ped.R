library(data.table)
library(tidyverse)
library(janitor)
library(sf)
library(ggmap)
library(lubridate)
library(here)
library(ggthemes)

sf::sf_use_s2(FALSE)
ncs <- here("Neighborhood_Councils_(Certified)/Neighborhood_Councils_(Certified).shp") %>% st_read() %>%
st_transform(4326)


cds <- here("LA_City_Council_Districts_(Adopted_2021)/LA_City_Council_Districts_(Adopted_2021).shp") %>% st_read() %>%
st_transform(4326)

##limit to central and veh. vs. ped or bike
data <- fread(list.files(pattern="Traffic")) %>% clean_names() %>% 
  mutate(ped=ifelse(grepl("3003", mo_codes), 1,0),
  bike=ifelse(grepl("3008|3016", mo_codes),1,0),
  fatal=ifelse(grepl("3027", mo_codes),1,0),
  severe_inj=ifelse(grepl("3024", mo_codes),1,0))


ped <-  data %>% filter(ped==1 | bike==1) %>%
  mutate(location=gsub("\\(|\\)","",location)) %>%
  mutate(lat=gsub(",.*$", "", location) %>% as.numeric(),
  long=gsub(".*\\, ", "", location)%>% as.numeric(),
  date_occurred = mdy(date_occurred),
  year_occurred = year(date_occurred))

###Geocode to CD boundaries
ped_sf <- ped %>% sf::st_as_sf(coords=c("long","lat"), crs=4326)

ped_cd <- st_join(ped_sf, cds, join=st_within) %>%
  filter(!is.na(NAME))

ped_cd1 <- ped_cd %>% filter(District==1)

intersection_by_year <- ped_cd1 %>% st_drop_geometry() %>% mutate(intersection = paste0(gsub("\\s+"," ",address), "+",gsub("\\s+"," ",cross_street))) %>%
  group_by(year_occurred, intersection) %>%
  summarise(sum_ped = sum(ped),
  sum_bike = sum(bike),
  sum_fatal=sum(fatal),
  sum_severe_inj = sum(severe_inj))


write_csv(intersection_by_year,"cd1_veh_vs_ped.csv")
