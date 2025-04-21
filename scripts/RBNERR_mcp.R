library(geodata)
library(tidyverse)
library(rnaturalearth)
library(gganimate)
library(raster)
library(ggmap)
library(ggplot2)
library(sf)
library(dplyr)
df<-read.csv("./data/telem.total.csv")

#set API
ggmap::register_google(key = "AIzaSyBeZDy6_OZBaN3ewQZxDmKgh8Xcq519IKQ")

#create basemap
base<-get_map(c(left=min(df$Long-0.1),bottom=min(df$Lat),right=max(df$Long),top=max(df$Lat)), source="google",maptype="satellite",zoom=11)

#convert to gg object
base<-ggmap(base)

base

#filter to remove rogue snakes
df<-df %>% filter(df$Status !="rogue")

#remove snake 118 (only one location)
df<-df %>% filter(df$Snake_ID!="PYBI_118")

#fixing Jesse's coordinate
df$UTM_N<-ifelse(df$UTM_N == 1, 2881527, df$UTM_N)

# #change unknown times to 1200
# df$Time<-ifelse(is.na(df$Time),1200,df$Time)

#format date column
df$Date<-as.Date(df$Date, format = "%Y-%m-%d")

#adding leading zeroes to time column
df$Time<-as.integer(df$Time)
df<-df %>% 
  mutate(Time=sprintf("%04d", df$Time))


## read in and plot picayune boundary
kml_boundary_rough <- st_read("./shapefiles/removal_boundaryKML.kml")
#kml_boundary <- sf::read_sf(".\\shapefiles\\Parks_Boundary.shp")

#merge geometries into a single layer
kml_boundary_rough <- kml_boundary_rough %>%
  group_by(Name) %>%
  summarize(geometry = st_union(geometry))

#change crs
kml_boundary_rough<-st_transform(kml_boundary_rough, crs = 4269)

#offsetting RBNERR boundary to align with new base map
kml_boundary_rough$geometry <-  kml_boundary_rough |> st_geometry() + c(.00185, -.0018)

#redefine crs
st_crs(kml_boundary_rough) = "EPSG:4269"

#create df of coord data
xy <-data.frame(df$UTM_E,df$UTM_N)

#creating a copy of df to turn into an spdf dataframe
spdf<-df

#converting point data to spatial points dataframe
coordinates(spdf) <- ~Long+Lat
proj4string(spdf) <- CRS("+proj=longlat +datum=NAD83")
spdf <- st_as_sf(spdf,coords = 24:25)
spdf <- spdf %>% 
  st_transform(crs = 4269)

#select only points that occur within the RBNERR boundary
RBNERR_p<-spdf[which(st_intersects(kml_boundary_rough, spdf, sparse = FALSE)), ]

names<-unique(RBNERR_p$Snake_Name)
df.RBNERR<-df %>% filter(df$Snake_Name %in% names)
df.RBNERR<-df.RBNERR %>% filter(Snake_Name!="Severus")
df.RBNERR<-df.RBNERR %>% filter(Snake_Name!="Marshall")
df.RBNERR<-df.RBNERR %>% filter(Snake_Name!="Joker")

#plot data
base+
  geom_sf(data = kml_boundary_rough, aes(fill = Name),inherit.aes=FALSE,show.legend = FALSE)+
  geom_sf(data = RBNERR_p, aes(fill=Snake_ID), show.legend = FALSE, size=2,  inherit.aes = FALSE)
  #geom_point(data=df.RBNERR, aes(x=Long,y=Lat))

#calculating the number of scout snakes within the rookery bay area and the MCP range coverage 
summ<-RBNERR_p %>% group_by(Snake_Name) %>% summarise(n=n())


# Make an amt `track` object with our sample data set
library(adehabitatHR)
library(amt)

#telemetry.mcp<-telemetry.mcp %>% group_by(id_season) %>% mutate (n=n()) %>% filter(n>=3) #Maybe Not Necessary????
#isolate relevant rows
telemetry.mcp<-as.data.frame(RBNERR_p)
telemetry.mcp <- telemetry.mcp[, c("Snake_ID", "UTM_E", "UTM_N","Date","Time")] 

# Define long/lat columns
coordinates(telemetry.mcp) <- c("UTM_E", "UTM_N")

# Set the coordinate reference system (CRS)
proj4string(telemetry.mcp) <- CRS("+init=epsg:26917")

# Project data to utms
telemetry.mcp <- spTransform(telemetry.mcp, 
                             CRS("+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs"))

#convert to df
telemetry.mcp<-data.frame(telemetry.mcp)

telemetry.mcp<-ungroup(telemetry.mcp)

telemetry.mcp$Snake_ID<-"01"
telemetry.mcp<-telemetry.mcp %>%
  mutate(ts = as.POSIXct(paste(Date, Time, sep = " ")))

track_data<-make_track(telemetry.mcp, coords.x1, coords.x2, ts,
                       crs = "+init=epsg:26917")

hr_mcp_result <- hr_mcp(track_data, level = 1)

hr_area<-hr_mcp_result[[1]][[3]]* 0.0000003861 #convert to sq mi
hr_area

sf_object <- st_as_sf(hr_mcp_result$mcp, coords = c("x_", "y_"))
crs(sf_object)

sf_object<-sf_object %>% st_transform(crs = 4269)


base +
  geom_sf(data = sf_object, fill = "lightblue", color = "black",alpha=.5,inherit.aes=FALSE,show.legend = FALSE) +
  geom_sf(data = RBNERR_p, aes(fill=Snake_ID), show.legend = FALSE, size=2,  inherit.aes = FALSE)

ggsave(filename="total.mcp.png",width = 180, height = 180, dpi=300, units = "mm",path= "./images")













