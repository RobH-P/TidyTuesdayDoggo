#Load in my libraries

library(tidyverse)
library(ggplot2)
library(janitor)
library(githubinstall)
library(readr)
library(openintro)

#read in the data

dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

names(dog_descriptions)

#merge the dog_moves and dog_travel data sets
doggo <- dog_descriptions %>% 
  inner_join(dog_travel, by = "id")
  
#clean rows without contact state 
#I need to find a better way to do multiple gsubs.

dog_descriptions$contact_state <- gsub("12220","NY", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("12477","NY", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("17325","PA", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("19053","PA", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("19063","PA", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("20136","VA", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("20136","VA", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("23112","VA", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("24588","VA", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("20905","MD", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("37189","TN", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("38506","TN", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("45061","OH", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("45249","OH", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("98106","WA", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("46158","IN", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("47131","IN", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("47454","IN", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("61944","IL", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("70601","CA", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("85249","AZ", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("87108","NM", dog_descriptions$contact_state, ignore.case = T) 
dog_descriptions$contact_state <- gsub("89146","NV", dog_descriptions$contact_state, ignore.case = T) 

#which states have the most housetrained dogs?



good_doggo <- dog_descriptions %>% 
  select(c(id, breed_primary, breed_secondary, breed_mixed, house_trained, contact_state)) %>% 
  filter(house_trained == "TRUE") %>% 
  group_by(contact_state)
  
  
good_doggo2 <- good_doggo %>% 
  group_by(contact_state) %>% 
  summarise(Frequency=sum(house_trained)) 

colnames(good_doggo2)[1] <- "state_abb"

full_state <- tibble(
  state_abb = state.abb,
  state_name = state.name
)


good_doggo3 <- right_join(good_doggo2, full_state, by="state_abb")

good_doggo3 <- good_doggo3[, c(1,3,2)]
colnames(good_doggo3)[2] <- "State"
colnames(good_doggo3)[3] <- "number_trained"

# mapping these on a hex map
#load additional libraries 
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(mapproj)
library(broom)
library(viridis)

# create a hex map 
# Load in shape file - sourced from the R Graphs Gallery site https://www.r-graph-gallery.com/
usa <- geojson_read("/Users/robhal1/Downloads/us_states_hexgrid.geojson",  what = "sp")

usa@data = usa@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

# plot state names onto the map

usa@data = usa@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
usa_fortified <- tidy(usa, region = "google_name")


centers <- cbind.data.frame(data.frame(gCentroid(usa, byid=TRUE), id=usa@data$iso3166_2))

ggplot() +
  geom_polygon(data = usa_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()


good_doggo3 %>% 
  ggplot( aes(x=number_trained)) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white') + 
  scale_x_continuous(breaks = seq(1,30))

# Merge geospatial and numerical information
doggo_map <- usa_fortified %>%
  left_join(. , good_doggo3, by=c("id"="State")) 

# Create the basic Chloropeth map
ggplot() +
  geom_polygon(doggo_map, mapping=aes(x = long, y = lat, group = group, fill = number_trained)) +
  scale_fill_gradient(trans = "log") +
  theme_void() +
  coord_map()



names(doggo_map)


# plot
doggo_map %>% 
ggplot() +
  geom_polygon(aes(fill = number_trained, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  geom_text(data=centers, aes(x=x, y=y, label=id), color="white") +
   scale_fill_gradient(low = "gray", high = "black", 
   name="House trained dogs up for adoption", 
    guide = guide_legend( keyheight = unit(3, units = "mm"), 
                          keywidth=unit(12, units = "mm"), 
                          label.position = "bottom", 
                          title.position = 'top', 
                          nrow=1)) + 
  theme_void() +
  coord_map() +
  labs(title = "Good Doggo: House trained hounds by state",
       caption = "Data: Petfinder.com/#Tidy Tuesday") +
  theme(legend.position = c(0.5, 0.9),
        plot.title = element_text(size= 22, hjust=0.5, 
                                  color = "red", 
                                  margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm"))
  )  
  
  
  

 