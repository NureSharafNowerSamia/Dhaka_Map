###Code for Static Map showing sample collection site ####
library(sf)
library(tmap)
library(bangladesh)
library(RColorBrewer) 

soil_samples <- data.frame(
  Name = c("E-01: Farm Soil", "E-02: Farm Soil", "E-03: Farm Soil", "E-04: Farm Soil",
           "E-05: Farm Soil", "E-06: Farm Soil", "E-07: Farm Soil", "E-08: Farm Soil",
           "E-10: Local Soil"),
  Type = c("Farm", "Farm", "Farm", "Farm", "Farm", "Farm", "Farm", "Farm", "Local"),
  Location = c("Mohammadpur, Shonir bil", "Kaundia, Savar", "Diabari, Mirpur 1",
               "Sher e Bangla, Bottola", "Darussalam, Shahid Minar", "Amin Bazar",
               "Hemayatpur", "Gabtoli", "Shahidullah Hall, DU"),
  Longitude = c(90.3569, 90.2667, 90.3594, 90.3632, 90.3946, 90.3324, 90.2085, 90.3492, 90.3964),
  Latitude = c(23.7643, 23.8554, 23.8235, 23.7805, 23.7278, 23.8021, 23.8907, 23.7936, 23.7333))
soil_samples_sf <- st_as_sf(soil_samples, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(Type = factor(Type, levels = c("Farm", "Local")),
         Location = factor(Location))
districts <- get_map("district") 
dhaka_districts <- districts[districts$Division == "Dhaka", ]
location_pal <- brewer.pal(9, "Set1") 
tmap_mode("plot")
tm <- tm_shape(dhaka_districts) +
  	tm_polygons(col = "District", palette = "Pastel1", legend.show = FALSE) +
  	#tm_polygons(alpha = 0.1, col = "gray95", border.col = "gray50") +
  	tm_shape(soil_samples_sf) +
 	 tm_symbols(
    	size = 0.7,
   	 shape = "Type", # Shapes show Farm/Local
    	shapes = c(16, 17),               
   	 col = "Location",                  
    	palette = location_pal,
    	border.lwd = 1.5,
    	title.shape = "Soil Type",
    	title.col = "Sampling Locations") +
 	 tm_layout(
   	 main.title = "Soil Sample Locations in Dhaka Division",
    	main.title.size = 1.5,
   	 legend.position = c("right", "bottom"),
   	 #legend.bg.color = "white",
   	 legend.frame = TRUE,
   	 legend.outside = FALSE ) +
 	 tm_legend(
   	 legend.format = list(text.align = "right"),
   	 legend.stack = "vertical")

tmap_save(tm, "dhaka_soil_map.png", 
          	width = 12,  # Inches
         	height = 10, 
          	dpi = 600, 
          	units = "in")
tmap_save(tm, "dhaka_soil_map.svg", 
          	width = 297,  
          	height = 210, 
          	units = "mm",
          	device = svg)

#### Code for Map interactive Map showing the actual Map #####
library(htmlwidgets)
library(leaflet)
library(dplyr)

pal <- colorFactor(palette = c("blue", "red"), domain = locations$Type)

map <- leaflet(soil_samples) %>%
 	 addTiles() %>%  
  	addCircleMarkers(~Longitude, ~Latitude, color = ~pal(Type), stroke = FALSE, fillOpacity = 0.8, popup = ~paste(Name, "<br>", Location) ) %>%
  addLegend("bottomright", pal = pal, values = ~Type,
            title = "Type of Soil", opacity = 1)

saveWidget(map, "Soil_interactive_map.html")

