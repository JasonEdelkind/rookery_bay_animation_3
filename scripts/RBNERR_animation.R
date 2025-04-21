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

#filter to remove rogue snakes
df<-df %>% filter(df$Status !="rogue")

#remove snake 118 (only one location)
df<-df %>% filter(df$Snake_ID!="PYBI_118")

#fixing Jesse's coordinate
df$UTM_N<-ifelse(df$UTM_N == 1, 2881527, df$UTM_N)

#remove rogue 1, rogue 2, and caesar
df<-df %>% filter(df$Snake_Name !="Rogue 1" & df$Snake_Name != "Rogue 2" & df$Snake_Name != "Caesar")

# #change unknown times to 1200
# df$Time<-ifelse(is.na(df$Time),1200,df$Time)
 
#format date column
df$Date<-as.Date(df$Date, format = "%Y-%m-%d")

#adding leading zeroes to time column
df$Time<-as.integer(df$Time)
df<-df %>% 
  mutate(Time=sprintf("%04d", df$Time))

#create timestamp column
# df<-df %>% mutate(Timestamp=paste(Date,Time))
# df$Timestamp<-as.POSIXct(df$Timestamp, format = "%Y-%m-%d %H%M")

#save dataframe
write.csv(df,"./data/telemetry_cleaned.csv", row.names = FALSE)

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

#set API
ggmap::register_google(key = "AIzaSyBeZDy6_OZBaN3ewQZxDmKgh8Xcq519IKQ")

#create basemap
base<-get_map(c(left=min(df$Long-0.1),bottom=min(df$Lat),right=max(df$Long),top=max(df$Lat)), source="google",maptype="satellite",zoom=11)

#convert to gg object
base<-ggmap(base)

base

#plot data
base+
  geom_sf(data = kml_boundary_rough, aes(fill = Name),inherit.aes=FALSE,show.legend = FALSE)+
  geom_sf(data = spdf, aes(fill=Snake_ID), show.legend = FALSE, size=2,  inherit.aes = FALSE)

#select only points that occur within the RBNERR boundary
RBNERR_p<-spdf[which(st_intersects(kml_boundary_rough, spdf, sparse = FALSE)), ]

RBNERR_p<-RBNERR_p %>% arrange(Snake_Name)
#identify snakes that occur within RBNERR
names<-unique(RBNERR_p$Snake_Name)

#plot data
base+
  geom_sf(data = kml_boundary_rough, aes(fill = Name),inherit.aes=FALSE,show.legend = FALSE)+
  geom_sf(data = RBNERR_p, aes(fill=Snake_ID), show.legend = FALSE, size=2,  inherit.aes = FALSE)

#creating an id-timestamp column to filter data
df<-df %>% mutate(id.timestamp=paste(Snake_ID,".",timestamp,sep=""))
RBNERR_p<-RBNERR_p %>% mutate(id.timestamp=paste(Snake_ID,".",timestamp,sep=""))

#filtering telemetry data to only include individuals within the RBNERR
df<-df %>% filter(df$Snake_Name %in% names)

base+
  geom_sf(data = kml_boundary_rough, aes(fill = Name),inherit.aes=FALSE,show.legend = FALSE)+
  geom_point(data=df, aes(x=Long,y=Lat))

#identifying duplicate records
duplicates.df<-df[duplicated(paste(df$id,df$Timestamp))|duplicated(paste(df$id,df$Timestamp), fromLast=TRUE),] #identify duplicates

#removing duplicate records
# df<-df %>% filter(!(df$Snake_Name=="Bobby Rubino" & is.na(df$Location)))
# df<-df %>% filter(!(df$Snake_Name=="Ender" & df$Date == as.Date("2023-09-13") & is.na(df$Observers)))
# df<-df %>% filter(!(df$Snake_Name=="Rogue 1" & df$Date == as.Date("2021-11-01") & is.na(df$Observers)))
# df<-df %>% filter(!(df$Snake_Name=="Harriet" & df$Date == as.Date("2021-05-06") & is.na(df$Burrow_Type)))
# df<-df %>% filter(!(df$Snake_Name=="Shrek" & df$Date == as.Date("2021-03-29") & is.na(df$Location)))
# df<-df %>% filter(!(df$Snake_Name=="Darwin" & df$Date == as.Date("2021-02-23") & df$Location=="A")) # low confidence aerial on same day as triangulation

#changing incorrect dates
# df$Date[df$Snake_Name=="Bobby Rubino" & df$Date == as.Date("2021-12-01") & df$Location == "R"] <-as.Date("2021-12-03",format="%Y-%m-%d")

#recreating the timestamp and id.timestamp columns
df<-df %>% mutate(timestamp=paste(Date,Time))
df$timestamp<-as.POSIXct(df$timestamp, format = "%Y-%m-%d %H%M")
df<-df %>% mutate(id.timestamp=paste(Snake_ID,".",timestamp,sep=""))

#check to confirm no more duplicate records
duplicates.df<-df[duplicated(paste(df$id,df$timestamp))|duplicated(paste(df$id,df$timestamp), fromLast=TRUE),] #identify duplicates

#isolate relevant rows
df<-df[,c(2,3,4,45)]

# assign convenient column names
colnames(df) = c('lon', 'lat','id','Timestamp')

#add date column
df$date<-as.Date(df$Timestamp)

#set relevant columns to factors and time
df$id<-as.factor(df$id)
df$Timestamp<-as.POSIXct(df$Timestamp, format = "%Y-%m-%d %H%M")

#visualize filtered data
base+
  geom_sf(data = kml_boundary_rough, aes(fill = Name),inherit.aes=FALSE,show.legend = FALSE)+
  geom_point(data=df, aes(x=lon,y=lat))+
  scale_fill_viridis_d(option="turbo")+                                #specify viridis turbo palette
  theme(legend.position="none")

## Animation

library(devtools)
library(pkgbuild)
library(ggspatial)
library(gginnards)
install_url('http://cran.r-project.org/src/contrib/Archive/moveVis/moveVis_0.10.5.tar.gz') #moveVis
# install_url('http://cran.r-project.org/src/contrib/Archive/maptools/maptools_1.1-8.tar.gz') #maptools
# install_url('http://cran.r-project.org/src/contrib/Archive/ggsn/ggsn_0.5.0.tar.gz') #ggsn
# library(maptools)
# library(ggsn)
library(moveVis)
library(move)

#creating the anchor
lon<-runif(n=10,min=-81.447560,max=-81.428541)
lat<-runif(n=10,min=25.975607,max=26.005258)
id<-c("PYBI_007","PYBI_007","PYBI_007","PYBI_007","PYBI_007","PYBI_007","PYBI_007","PYBI_007","PYBI_007","PYBI_007")
Timestamp<-c(as.POSIXct("2024-08-01 12:00:00", format="%Y-%m-%d %H:%M:%S"),
            as.POSIXct("2024-09-01 12:00:00", format="%Y-%m-%d %H:%M:%S"),
            as.POSIXct("2024-10-01 12:00:00", format="%Y-%m-%d %H:%M:%S"),
            as.POSIXct("2024-11-01 12:00:00", format="%Y-%m-%d %H:%M:%S"),
            as.POSIXct("2024-12-01 12:00:00", format="%Y-%m-%d %H:%M:%S"),
            as.POSIXct("2025-01-01 12:00:00", format="%Y-%m-%d %H:%M:%S"),
            as.POSIXct("2025-02-01 12:00:00", format="%Y-%m-%d %H:%M:%S"),
            as.POSIXct("2025-03-01 12:00:00", format="%Y-%m-%d %H:%M:%S"),
            as.POSIXct("2025-04-01 12:00:00", format="%Y-%m-%d %H:%M:%S"),
            as.POSIXct("2025-05-01 12:00:00", format="%Y-%m-%d %H:%M:%S"))
date<-c(as.Date("2024-08-01", format = "%Y-%m-%d"),as.Date("2024-09-01", format = "%Y-%m-%d"),
       as.Date("2024-10-01", format = "%Y-%m-%d"),as.Date("2024-11-01", format = "%Y-%m-%d"),
       as.Date("2024-12-01", format = "%Y-%m-%d"),as.Date("2025-01-01", format = "%Y-%m-%d"),
       as.Date("2025-02-01", format = "%Y-%m-%d"),as.Date("2025-03-01", format = "%Y-%m-%d"),
       as.Date("2025-04-01", format = "%Y-%m-%d"),as.Date("2025-05-01", format = "%Y-%m-%d"))
anchor<-data.frame(lon,lat,id,Timestamp,date)
df<-rbind(df,anchor)

#create move object
df.mov<-df2move(df,proj="+proj=longlat +datum=NAD83",x="lon",y="lat",time="Timestamp",track_id="id")

# align df.mov to a uniform time scale
m <- align_move(df.mov, res = 1, unit = "days")

## coding symbols by season

timestamps <- as.data.frame(m$time)
names(timestamps) <- "timestamp"
timestamps$colour <- "orange" #base colour

#setting colours by season
# timestamps[timestamps$timestamp >=as.POSIXct("2018-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# timestamps[timestamps$timestamp >=as.POSIXct("2018-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding
# 
# timestamps[timestamps$timestamp >=as.POSIXct("2019-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# timestamps[timestamps$timestamp >=as.POSIXct("2019-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding
# 
# timestamps[timestamps$timestamp >=as.POSIXct("2020-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# timestamps[timestamps$timestamp >=as.POSIXct("2020-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding
# 
# timestamps[timestamps$timestamp >=as.POSIXct("2021-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# timestamps[timestamps$timestamp >=as.POSIXct("2021-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding
# 
# timestamps[timestamps$timestamp >=as.POSIXct("2022-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# timestamps[timestamps$timestamp >=as.POSIXct("2022-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding
# 
# timestamps[timestamps$timestamp >=as.POSIXct("2023-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# timestamps[timestamps$timestamp >=as.POSIXct("2023-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding
# 
# timestamps[timestamps$timestamp >=as.POSIXct("2024-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# #timestamps[timestamps$timestamp >=as.POSIXct("2024-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding

# timestamps[timestamps$timestamp >=as.POSIXct("2025-04-16 00:00:00", tz = "UTC"),]$colour <- "blue" #feeding
# #timestamps[timestamps$timestamp >=as.POSIXct("2025-10-15 00:00:00", tz = "UTC"),]$colour <- "orange"  #breeding

cols <- timestamps$colour # pull out the colors into a list

m$colour <- cols # now add the "cols" to the "colours" column


#define extent
ext <- extent(-81.8, -81.54, 25.96, 26.13)

# create spatial frames 
frames<- frames_spatial(m,
                        map_service = "mapbox", map_type = "satellite", 
                        map_token = "pk.eyJ1IjoiamFzb25lZGVsa2luZCIsImEiOiJjbTMzOTlzcnAxbWJ4MmpwdWkyYnpmODIwIn0.J242Igdiqc-VQupNItls9w",
                        map_res = 1, ext = ext, equidistant = FALSE) %>% 
  add_labels(x = "Longitude", y = "Latitude")# %>% # add some customizations, such as axis labels
# moveVis::add_northarrow(position="bottomright") %>% 
# moveVis::add_scalebar(height=0.02,colour="white") #%>% 
#add_timestamps(size = 5,type = "label") %>% 
#add_progress()  

#remove legend and axes elements
frames = add_gg(frames, gg = expr(theme(legend.position = "none"))) # remove legend
frames = add_gg(frames, gg = expr(theme(axis.text.x=element_blank()))) #remove text labels
frames = add_gg(frames, gg = expr(theme(axis.ticks.x=element_blank()))) #remove ticks
frames = add_gg(frames, gg = expr(theme(axis.text.y=element_blank()))) #remove text labels
frames = add_gg(frames, gg = expr(theme(axis.ticks.y=element_blank()))) #remove ticks
frames = add_gg(frames, gg = expr(theme(axis.title.x = element_blank()))) #remove titles
frames = add_gg(frames, gg = expr(theme(axis.title.y = element_blank()))) #remove titles

# #set extent
# frames = add_gg(frames, gg = expr(coord_sf(xlim = c(-81.8, -81.565), ylim = c(25.93, 26.1),expand=FALSE)))
# 
# #specify crs
# frames = add_gg(frames, gg = expr(coord_sf(crs = 4269)))

#add scalebar
frames = add_gg(frames, gg = expr(ggspatial::annotation_scale(location="bl",line_width=.4,text_col="white",style="bar",pad_x=unit(0,"cm"), pad_y=unit(2.5,"cm"),text_cex=.75,width_hint=.18,height=unit(0.3, "cm"))))

#add north arrow
frames = add_gg(frames, gg = expr( ggspatial::annotation_north_arrow(pad_x=unit(0.15, "cm"),pad_y=unit(2.9, "cm"),location = "bl", which_north = "true", style = north_arrow_nautical(text_size = 15, text_col = "white"))))

#add  title
frames<-add_labels(frames, title = "Rookery Bay National Estuarine Research Reserve \n Python Tracking and Removal 2013 to 2025")

#remove old base layer
frames<-lapply(frames,delete_layers, match_type="GeomRaster")

#set API
ggmap::register_google(key = "AIzaSyBeZDy6_OZBaN3ewQZxDmKgh8Xcq519IKQ")

#create new base layer
base<-get_map(c(left = ext[1]-.08, bottom = ext[3]-.03, right = ext[2]+.08, top = ext[4]+.03),
              source="google",maptype="satellite")
base<-ggmap(base) #convert ggmap to gg object
base

#add new base layer
frames<-lapply(frames,append_layers, object=base$layers[[2]],position="bottom")

#add RBNERR boundary line
kml_boundary <- sf::read_sf(".\\shapefiles\\RB_BoundaryKML.kml")

#merge geometries into a single layer
kml_boundary <- kml_boundary %>%
  group_by(Name) %>%
  summarize(geometry = st_union(geometry))

#change crs
kml_boundary<-st_transform(kml_boundary, crs = 4269)

#offsetting RBNERR boundary to align with new base map
kml_boundary$geometry <-  kml_boundary |> st_geometry() + c(.00185, -.0018)

#redefine crs
st_crs(kml_boundary) = "EPSG:4269"

frames = add_gg(frames, gg = expr(geom_sf(data = kml_boundary,color=NA, aes(fill=NA,linewidth=0.25),inherit.aes=FALSE,show.legend = FALSE))) #i have no frickin clue why I need to add an initial boundary before i can edit the thickness of a secondary boundary, but if this jenga tower gets built who am I to criticize the one piece in the middle holding this wobbly mess up?
frames = add_gg(frames, gg = expr(geom_sf(data = kml_boundary,color="springgreen", aes(fill=NA,linewidth=0.13),inherit.aes=FALSE,show.legend = FALSE))) #add PSSF boundary
frames = add_gg(frames, gg = expr(coord_sf(xlim = c(-81.8, -81.565), ylim = c(25.93, 26.1),expand=FALSE)))

#add background rectangle for dates and removal tally
frames = add_gg(frames, gg = expr(geom_rect(aes(xmin = -81.605, xmax =-81.565 , ymin =  26.0745, ymax = 26.087), 
                                            fill = "white"))) # add background rectangle for date
frames = add_gg(frames, gg = expr(geom_rect(aes(xmin = -81.635, xmax =-81.565 , ymin = 26.0875  , ymax = 26.1), 
                                            fill = "white"))) # add background rectangle for removal tally

  
#create inset map
inset<-get_map(c(left=-88,bottom=24.5,right=-80,top=31), source="google",maptype="satellite",zoom=6) #option 1 for all of FL
#inset<-get_map(c(left=-84,bottom=24.5,right=-79,top=28.5), source="google",maptype="satellite",zoom=8) #option 2 for zoom in of peninsula
inset<-ggmap(inset)
inset

#create Florida border layer
map <- map_data('state')
florida <- subset(map, region %in% "florida")
florida$lat2<-florida$lat-0.2
florida$long2<-florida$long+0.02

inset_final<-inset+ 
  #geom_sf(data = kml_boundary,fill="red",color="red",linewidth=1,inherit.aes=FALSE,show.legend = FALSE)+
  #geom_point(aes(x=26.080015715876005, y=-81.56204701438494), fill="red",size=3)+
  geom_polygon(data = florida, aes(x=long2, y = lat2, group = group), fill=NA,colour="black", linewidth=4)+ #option 1
  annotate("point", y = 26.032690, x = -81.700743, colour = "red",size=12)+
  #geom_polygon(data = florida, aes(x=long2, y = lat2, group = group), fill=NA,colour="black", linewidth=0.5)+ #option 2
  xlim(-87.75,-80)+  ylim(24,31)+ #option 1
  #xlim(-83,-80)+  ylim(25.2,27.75)+ #option 2
  theme_void()+
  theme(
    axis.line=element_blank(),
    axis.text = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=0.5))

inset_final
ggsave(filename="inset.png",width = 180, height = 180, dpi=300, units = "mm",path= "./images")

#creating a list of all days in movestack
dates<-data.frame(seq(as.Date(min(timestamps(m))), as.Date(max(timestamps(m))), by="days"))
colnames(dates)[1]<-"Date"

#reformat date and change to list format
dates$Date<-format(as.Date(dates$Date), "%d-%b-%y")
dates<-as.list(dates)

#add custom timestamps to frames
for(i in 1:length(frames)){
  frames[[i]]<-frames[[i]]+annotate(geom="text", x=-81.5845, y=26.0815, label=dates[[1]][i],size=4.5,
                                    color="black")
}


#read in removal data
removal<-read.csv(".\\data\\removal.scout.csv")
removal<-removal %>% filter(Method=="Scout")

#fixing blank scout names in removal database
removal$Sentinel<-ifelse(removal$Additional_ID == "PYBI-114", "KJ", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP01-06MAR24", "Vlad", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP02-06MAR24", "Vlad", removal$Sentinel)
removal$ID<-ifelse(removal$ID == "BP01- 08MAR24", "BP01-08MAR24", removal$ID)
removal$Sentinel<-ifelse(removal$ID == "BP01-08MAR24", "Eddie", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP01-11MAR24", "Abe", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP01-12MAR24", "Ronin", removal$Sentinel)
removal$Sentinel<-ifelse(removal$ID == "BP01-02JAN25", "Pacino", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Bobby Rubino", "Bobby Jo", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Joey", "Bobby Jo", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Harriett", "Harriet", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Darryl McLovin", "McLovin", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Timmy", "Timmy T", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Cash", "Ca$h", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Jaegar", "Jaeger", removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Dos Equis", "XX", removal$Sentinel)

#changing bobby rubino to bobby jo
removal$Sentinel<-ifelse(removal$Sentinel == "Bobby Rubino","Bobby Jo",removal$Sentinel)
removal$Sentinel<-ifelse(removal$Sentinel == "Joey","Bobby Jo",removal$Sentinel)

RBNERR_p<-spdf[which(st_intersects(kml_boundary_rough, spdf, sparse = FALSE)), ]

RBNERR_p<-RBNERR_p %>% arrange(Snake_Name)
#identify snakes that occur within RBNERR
names<-unique(RBNERR_p$Snake_Name)

#filtering removal to only include RBNERR scouts
removal<-removal %>% filter(removal$Sentinel %in% names)

#removing removal points north of tamiami
removal<-removal %>% filter(removal$Designation!="Six L's Agricultural Buffer")
removal<-removal %>% filter(removal$ID!="BP01-20DEC22")
removal<-removal %>% filter(removal$ID!="BP03-14FEB23")


#renaming columns
colnames(removal)[2] <- 'x'
colnames(removal)[3] <- 'y'

#filtering removal to only include points within the RBNERR boundary
removal_sf<-st_as_sf(removal, coords = c("x", "y"), crs = 4269,remove=FALSE) #convert removal df to sf object
removal_sf<-st_filter(removal_sf, kml_boundary_rough) #filter removal sf to only include points within RBNERR boundary sf
removal_RBNERR<-st_drop_geometry(removal_sf) #reconvert removal sf to df while creating a separate removal object to only count removals within PSSF for dynamic tally

removal_RBNERR<-removal_RBNERR %>% dplyr::arrange(removal_RBNERR$Sentinel)
names<- unique(removal_RBNERR$Sentinel)

#converting date to date format
removal$Date<-as.Date(removal$Date, format="%Y-%m-%d")

#filter data by date
#removal<-removal %>% filter(removal$Date > as.Date("11/01/2018",format="%m/%d/%Y"))

#rename coords
names(removal)[names(removal) == 'x'] <- 'longitude'
names(removal)[names(removal) == 'y'] <- 'latitude'

#creating a list of all days in movestack
dates<-data.frame(seq(as.Date(min(timestamps(m))), as.Date(max(timestamps(m))), by="days"))
colnames(dates)[1]<-"Date"

# calculate cumulative removals by date
removal2<-removal_RBNERR %>% group_by(Date) %>% summarise(n=n()) #summarizing removals by date
removal2$total.removal<-cumsum(removal2$n) #calculate cumulative removals over time
cum.removal<-merge(dates,removal2,by='Date',all.x=TRUE) #merge date and removal dataframes
library(zoo)
cum.removal<-fill(cum.removal, total.removal, .direction = 'down') #carry cumulative removals down the column rows
cum.removal$total.removal<-ifelse(is.na(cum.removal$total.removal),0,cum.removal$total.removal) #replace NAs with zero
cum.removal$n<-ifelse(is.na(cum.removal$n),0,cum.removal$n) #replace NAs with zero

#add custom removal tally to frames
frames = add_gg(frames, gg = expr(annotate(geom="text", x=-81.601, y=26.094355, label=paste("Python Removals: ",sep=""),size=4.5,
                                           color="black"))) # add background rectangle for date
# for(i in 1:length(frames)){
#   frames[[i]]<-frames[[i]]+annotate(geom="text", x=-81.601, y=26.094355, label=paste("Python Removals: ",sep=""),size=4.5,
#                                     color="black")
# }
# add bolded python  removal numbers to tally
for(i in 1:length(frames)){
  frames[[i]]<-frames[[i]]+annotate(geom="text", x=-81.571, y=26.094355, label=cum.removal$total.removal[i],size=4.5,
                                    color="black",fontface = "bold")
  
}
  

#convert dates to a data table and create a column to define the season
dates.table<-as.data.frame(dates)
dates.table$Date<-as.Date(dates.table$Date, format = "%d-%b-%y")
dates.table$active.season<-NA
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2012",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2019",format="%m/%d/%Y"),1,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2013",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2019",format="%m/%d/%Y"),2,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2014",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2019",format="%m/%d/%Y"),3,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2015",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2019",format="%m/%d/%Y"),4,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2016",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2019",format="%m/%d/%Y"),5,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2017",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2019",format="%m/%d/%Y"),6,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2018",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2019",format="%m/%d/%Y"),7,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2019",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2020",format="%m/%d/%Y"),8,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2020",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2021",format="%m/%d/%Y"),9,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2021",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2022",format="%m/%d/%Y"),10,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2022",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2023",format="%m/%d/%Y"),11,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2023",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2024",format="%m/%d/%Y"),12,dates.table$active.season)
dates.table$active.season<-ifelse(dates.table$Date>=as.Date("10/15/2024",format="%m/%d/%Y") & dates.table$Date<=as.Date("10/14/2025",format="%m/%d/%Y"),13,dates.table$active.season)

###########################ADDING CONSTANT REMOVAL POINTS#####################################################
#run a loop to add removal points to each frame with constant shapes and colors
for (i in 1:as.integer(length(frames))) {
  points<-removal_RBNERR %>% filter(removal_RBNERR$Date<=dates.table[[1]][i])   #filter removals to only include records from the targeted date 
  frames[i] = add_gg(frames[i], gg = expr(geom_point(data=points, aes(x=x, y=y),size=3,color="red"))) # add the filtered removal points to the map
  
}

##add a final frame to show just removal points
#select just the frames up to march 20 2025
frames<-frames[1:4443]
frames[[4444]]<-frames[[4443]] #create final frame
frames[[4444]]$layers[[2]]<- NULL #remove scout tracklines from final frame
frames[[4444]]

## adding inset to frames
library(png)
library(grid)

# read in inset image
inset.img <- readPNG("./images/inset.png")
legend.img <- readPNG("./images/legend_cropped.png")

g1 <- rasterGrob(inset.img, interpolate=TRUE)
g2 <- rasterGrob(legend.img, interpolate=TRUE)

#add the inset image to all frames
frames = add_gg(frames, gg = expr(annotation_custom(g1, xmin=-81.616, xmax=-81.553, ymin=25.93, ymax=25.965)))
#add the legend image to all frames
frames = add_gg(frames, gg = expr(annotation_custom(g2, xmin=-81.8, xmax =-81.734375 , ymin =  25.895, ymax =  25.9943125)))
#add the legend components to all frames
# frames = add_gg(frames, gg = expr(geom_rect(aes(xmin = -81.8, xmax =-81.725 , ymin =  25.996, ymax =  26.0335), 
#                                             fill = "white")))#add legend background rectangle
# frames = add_gg(frames, gg = expr(annotate(geom="text", x=-81.762, y=26.028, label=paste("Legend",sep=""),size=4.5, color="black")))#add legend title
# frames = add_gg(frames, gg = expr(annotate(geom="text", x=-81.777, y=26.018, label=paste("Scout Python",sep=""),size=3.5, color="black")))#add subtitle
# frames = add_gg(frames, gg = expr(annotate(geom="text", x=-81.7735, y=26.01084, label=paste("Python Removal",sep=""),size=3.5, color="black"))) #add subtitle
# frames = add_gg(frames, gg = expr(annotate(geom="text", x=-81.774, y=26.0035, label=paste("PSSF Boundary",sep=""),size=3.5, color="black"))) #add subtitle
# frames = add_gg(frames, gg = expr(annotate("point", x = -81.735,y = 26.008, colour = "orange",size=4))) #add legend symbols
# frames = add_gg(frames, gg = expr(annotate("point", x = -81.735,y = 26.00084, colour = "red",size=4))) #add legend symbols
# frames = add_gg(frames, gg = expr(annotate("segment", x = -81.7381,xend=-81.7319 ,y = 26.0035, colour = "springgreen",size=1))) #add legend symbols

#animate and save frames
animate_frames(frames, fps = 15, out_file = "./animations/final_v2.15fps.mp4", end_pause = 2, overwrite=TRUE)

##################################################################################
#summarizing PSRP removal data
removal.RBNERR.summ<-removal %>% summarise(n=n(),
                                              avg.lbs=round(mean(Weight_lbs,na.rm=T),digits=2),
                                              avg.lbs.female=round(mean(Weight_lbs[Sex == "Female"]),digits=2),
                                              avg.lbs.male=round(mean(Weight_lbs[Sex == "Male"],na.rm=T),digits=2),
                                              total.lbs=round(sum(Weight_lbs,na.rm=T),digits=2),
                                              total.lbs.female=round(sum(Weight_lbs[Sex == "Female"]),digits=2),
                                              total.lbs.male=round(sum(Weight_lbs[Sex == "Male"],na.rm=T),digits=2),
                                              
                                              total.length.cm=round(sum(TotL_cm,na.rm=T),digits=2),
                                              total.length.cm.female=round(sum(TotL_cm[Sex == "Female"],na.rm=T),digits=2),
                                              total.length.cm.male=round(sum(TotL_cm[Sex == "Male"],na.rm=T),digits=2),
                                              avg.length.cm=round(mean(TotL_cm,na.rm=T),digits=2),
                                              avg.length.cm.female=round(mean(TotL_cm[Sex == "Female"],na.rm=T),digits=2),
                                              avg.length.cm.male=round(mean(TotL_cm[Sex == "Male"],na.rm=T),digits=2),
                                              
                                              total.svl.cm=round(sum(SVL_cm,na.rm=T),digits=2),
                                              total.svl.cm.female=round(sum(SVL_cm[Sex == "Female"],na.rm=T),digits=2),
                                              total.svl.cm.male=round(sum(SVL_cm[Sex == "Male"],na.rm=T),digits=2),
                                              avg.svl.cm=round(mean(SVL_cm,na.rm=T),digits=2),
                                              avg.svl.cm.female=round(mean(SVL_cm[Sex == "Female"],na.rm=T),digits=2),
                                              avg.svl.cm.male=round(mean(SVL_cm[Sex == "Male"],na.rm=T),digits=2)
)



############################################################################
# Save a single object to a file
saveRDS(test, "test.rds")
# Restore it under a different name
test <- readRDS("test.rds")

#add the legend components to one frame
test<-frames[[1000]]
test

test+
geom_rect(aes(xmin = -81.8, xmax =-81.725 , ymin =  25.996, ymax =  26.0335), 
                                            fill = "white")+#add legend background rectangle
annotate(geom="text", x=-81.762, y=26.028, label=paste("Legend",sep=""),size=4.5, color="black")+#add legend title
annotate(geom="text", x=-81.7805, y=26.018, label=paste("Scout Python",sep=""),size=3.5, color="black")+#add subtitle
annotate(geom="text", x=-81.7774, y=26.01084, label=paste("Python Removal",sep=""),size=3.5, color="black")+ #add subtitle
annotate(geom="text", x=-81.774, y=26.0035, label=paste("RBNERR Boundary",sep=""),size=3.5, color="black")+ #add subtitle
annotate("point", x = -81.735,y = 26.018, colour = "orange",size=4)+ #add legend symbols
annotate("point", x = -81.735,y = 26.01084, colour = "red",size=4)+ #add legend symbols
annotate("segment", x = -81.7381,xend=-81.7319 ,y = 26.0035, colour = "springgreen",size=1) #add legend symbols

ggsave(filename="test.png",width = 180, height = 180, dpi=300, units = "mm",path= "./images") #crop image afterwards to just legend


########################################################################################################

#figuring out scalebar and north arrow
test+
  coord_sf(crs = 4269)+
  coord_sf(xlim = c(-81.8, -81.565), ylim = c(25.93, 26.1),expand=FALSE)+
  ggspatial::annotation_scale(location="bl",line_width=.4,text_col="white",style="bar",pad_x=unit(0,"cm"), pad_y=unit(2.5,"cm"),text_cex=.75,width_hint=.18,height=unit(0.3, "cm"))+
  ggspatial::annotation_north_arrow(pad_x=unit(0.15, "cm"),pad_y=unit(2.9, "cm"),location = "bl", which_north = "true", style = north_arrow_nautical(text_size = 15, text_col = "white"))

#extent = (-81.8, -81.565), ylim = c(25.93, 26.1)

#figuring out removal tally and timestamp rectangles
test+
geom_rect(aes(xmin = -81.605, xmax =-81.565 , ymin =  26.0745, ymax = 26.087), 
                                            fill = "white")+ # add background rectangle for date
geom_rect(aes(xmin = -81.635, xmax =-81.565 , ymin = 26.0875  , ymax = 26.1), 
                                            fill = "white")+ # add background rectangle for removal tally
  

#figuring out timestamp
test+
  annotate(geom="text", x=-81.5845, y=26.0815, label=dates[[1]][1000],size=4.5,
                                    color="black")
#figuring out removal tally
test+
  annotate(geom="text", x=-81.601, y=26.094355, label=paste("Python Removals: ",sep=""),size=4.5,
           color="black")+
  annotate(geom="text", x=-81.57, y=26.094355, label=cum.removal$total.removal[1000],size=4.5,
           color="black",fontface = "bold")


#adding inset and legend images
#read in inset image
inset.img <- readPNG("./images/inset.png")
legend.img <- readPNG("./images/legend_cropped.png")

g1 <- rasterGrob(inset.img, interpolate=TRUE)
g2 <- rasterGrob(legend.img, interpolate=TRUE)

test<-test+
annotation_custom(g1, xmin=-81.616, xmax=-81.553, ymin=25.93, ymax=25.965)

test<-frames[[1000]]


test<-test+
annotation_custom(g2, xmin=-81.8, xmax =-81.734375 , ymin =  25.895, ymax =  25.9943125)
ggsave(filename="test.png",width = 180, height = 180, dpi=300, units = "mm",path= "./images") #crop image afterwards to just legend

#annotation_custom(g2, xmin=-81.8, xmax =-81.7475 , ymin =  25.902, ymax =  25.98145)


