

library(tidyverse)
library(lubridate)
library(sf)
library(rpostgis)
library(RPostgreSQL)
library(RPostgres)
library(shiny)
library(highcharter)
library(zoo)
library(plyr)
library(deldir)


# Data Import 
# I am imported the data into a postgres database, and used important the data from there. 
# But, all you need is the data from ACLED, the administrative boundaries, for which is included
# in the github files. 
# ------------------------------------------------------------------------------------------------------------------------

# Connecting Database 
con <- dbConnect(
  drv = RPostgres::Postgres(),
  dbname = 'UkraineAnalysis',
  host = 'localhost',
  port = '5432',
  user = 'postgres',
  password = '********'
)

# List tables in my database 
dbListTables(con)
# Getting data via database
# Going to pick one country, Ukraine, because more than one country seems too ambitious
# and its hot topic at the moment. 

data.acled <- dbGetQuery(con, "SELECT * FROM acled_data")
ukraine.acled <- dbGetQuery(con, "SELECT * FROM acled_data WHERE country = 'Ukraine'")



# Getting administrative boundary data 
ukr.admin.level.0.boundaries <- rpostgis::pgGetGeom(con,
                                                    name = c('public', 'ukr_admbnda_adm0_sspe_20230201'),
                                                    geom = 'geom',
                                                    gid = 'gid')
ukr.admin.level.1.boundaries <- rpostgis::pgGetGeom(con, 
                                                    name = c('public', 'ukr_admbnda_adm1_sspe_20230201'),
                                                    geom = 'geom',
                                                    gid = 'gid')

# ukr.admin.level.2.boundaries <-rpostgis::pgGetGeom(con,
#                                                   name = c('public', 'ukr_admbnda_adm2_sspe_20230201'),
#                                                   geom = 'geom',
#                                                   gid = NULL)

# ------------------------------------------------------------------------------------------------------------------------

# Data cleaning/getting stuff ready for the "Data Visualization" part of the shiny app
# ------------------------------------------------------------------------------------------------------------------------

# Removing 
# There are a few observations, which don't lie on a particular administrative boundary in the ACLED dataset,
# these observations lie in the Black Sea and aren't events that occur on land, so I'm going to remove them
dim(ukraine.acled)
ukraine.acled[which(is.na(ukraine.acled$admin1)),]
ukraine.acled <- ukraine.acled[-which(is.na(ukraine.acled$admin1)),]
dim(ukraine.acled)

# These come in as spatial data format, in a SpatialPolygonsDataFrame (namely from the package sp)
class(ukr.admin.level.1.boundaries) 

# Turning this data set into a class sf
ukr.admin.level.0.boundaries <- sf::st_as_sf(ukr.admin.level.0.boundaries)
ukr.admin.level.1.boundaries <- sf::st_as_sf(ukr.admin.level.1.boundaries)
# ukr.admin.level.2.boundaries <- sf::st_as_sf(ukr.admin.level.2.boundaries)

# Setting the coordinate reference system 
# I don't know the official CRS that was used in the source, or even if there is one
# from looking at their website

st_crs(ukr.admin.level.0.boundaries) <- 4326
st_crs(ukr.admin.level.1.boundaries) <- 4326
# st_crs(ukr.admin.level.2.boundaries) <- 4326
# The only rationale for choosing 4326 as the EPSG code is that ACLED uses latitude and longitude 
# geo-coordinates based on EPSG:4326 (source: https://acleddata.com/resources/quick-guide-to-acled-data/
# under 'What Coordinate Reference System (CRS) does ACLED use?')


# Are the names of the administrative boundaries the equal from both datasets? 
# Do they have the same 
unique(sort(ukraine.acled$admin1))
unique(ukraine.acled$admin1)
unique(sort(ukr.admin.level.1.boundaries$adm1_en))
# Sevastopol, is a 1st level administrative region in the Crimean Peninsula, for which ACLED does not include
# (the reason for the inconsistency in the length output of both datasets (in the above code)),
# rather I think it just includes it in the 'Autonomous Republic of Crimea". 
# The code below combines the Autonomous Republic of Crimea and Sevastopol. 


ukr.admin.level.1.boundaries <- ukr.admin.level.1.boundaries %>%
  mutate(admin1 = case_when(
    adm1_en %in% c("Autonomous Republic of Crimea", "Sevastopol") ~ "Crimea",
    adm1_en %in% c("Cherkaska") ~ "Cherkasy",
    adm1_en %in% c("Chernihivska") ~ "Chernihiv",
    adm1_en %in% c("Chernivetska") ~ "Chernivtsi", 
    adm1_en %in% c("Dnipropetrovska") ~ "Dnipropetrovsk",
    adm1_en %in% c("Donetska") ~ "Donetsk",
    adm1_en %in% c("Ivano-Frankivska") ~ "Ivano-Frankivsk",
    adm1_en %in% c("Kharkivska") ~ "Kharkiv",
    adm1_en %in% c("Khersonska") ~ "Kherson",
    adm1_en %in% c("Khmelnytska") ~ "Khmelnytskyi",
    adm1_en %in% c("Kirovohradska") ~ "Kirovograd",
    adm1_en %in% c("Kyiv") ~ "Kyiv City",
    adm1_en %in% c("Kyivska") ~ "Kyiv",
    adm1_en %in% c("Luhanska") ~ "Luhansk",
    adm1_en %in% c("Lvivska") ~ "Lviv",
    adm1_en %in% c("Mykolaivska") ~ "Mykolaiv",
    adm1_en %in% c("Odeska") ~ "Odesa",
    adm1_en %in% c("Poltavska") ~ "Poltava",
    adm1_en %in% c("Rivnenska") ~ "Rivne",
    adm1_en %in% c("Sumska") ~ "Sumy",
    adm1_en %in% c("Ternopilska") ~ "Ternopil",
    adm1_en %in% c("Vinnytska") ~ "Vinnytsia",
    adm1_en %in% c("Volynska") ~ "Volyn",
    adm1_en %in% c("Zakarpatska") ~ "Zakarpattia",
    adm1_en %in% c("Zaporizka") ~ "Zaporizhia",
    adm1_en %in% c("Zhytomyrska") ~ "Zhytomyr"
  )) %>% select(-adm1_en)


combined <- st_union(
  ukr.admin.level.1.boundaries[which(ukr.admin.level.1.boundaries$admin1 == "Crimea"),]
)

combined <- st_sf(combined,
                  admin1 = 'Crimea')
colnames(combined) <- c("admin1", "geometry")
st_geometry(combined) <- "geometry"
st_crs(combined) <- 4326

ukr.admin.level.1.boundaries <- ukr.admin.level.1.boundaries %>% 
  filter(admin1 != 'Crimea') %>% select(admin1, geometry)


# The below code combines the ACLED with the geometries. 
ukr.admin.level.1.boundaries <- rbind(ukr.admin.level.1.boundaries, combined)

data <- full_join(ukraine.acled, ukr.admin.level.1.boundaries, by = c("admin1"))
data$event_type <- as.factor(data$event_type)
data$sub_event_type <- as.factor(data$sub_event_type)
ukr.admin.level.1.boundaries.selected <- ukr.admin.level.1.boundaries
ukr.admin.level.1.boundaries.selected$ID <- c(1:26)

# Now we are going to filter the data to only a select columns 
data = data %>% select(event_date, time_precision,
                       event_type, sub_event_type,
                       actor1, assoc_actor_1, inter1,
                       actor2, assoc_actor_2, inter2,
                       interaction, admin1, admin2,
                       location_variable, latitude, longitude,
                       geo_precision, source_variable,
                       notes, geometry)


# Arranging data and grouping by administrative boundary level 1 and event date
data = data %>% 
  group_by(event_date, admin1) %>% 
  arrange(event_date, admin1) %>%
  ungroup()

# Creating unique event identifier and sf version and color thing
data$recordID <- 1:55330

data.sf <- st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)
st_crs(data.sf)

# Changing the coordinate system to a projected one
data.sf = st_transform(data.sf, crs = "EPSG:6381")
st_crs(data.sf)
data.projected = data %>% select(-longitude,-latitude) 
data.projected$X <- st_coordinates(data.sf)[,1]
data.projected$Y <- st_coordinates(data.sf)[,2]


# Changing projection

# Removing obsolete data
rm(combined); rm(ukraine.acled); rm(data.acled); 



data.count.day <- data %>% 
  group_by(event_date, admin1, sub_event_type) %>% 
  dplyr::count(event_date, admin1, sub_event_type)
data.count.day.sf = st_sf(inner_join(data.count.day, ukr.admin.level.1.boundaries, by = c("admin1" = "admin1")),
                          sfc_last = T)
data.count.day$sub_event_type <- as.factor(data.count.day$sub_event_type)


data.count.month <- data %>%
  mutate(Month = as.yearmon(event_date, "%Y-%m")) %>%
  dplyr::count(Month, admin1, sub_event_type)
data.count.month$Month <- as_date(data.count.month$Month)
colnames(data.count.month) <- c("event_date", "admin1", "sub_event_type", "n")

data.count.month.sf <- st_sf(inner_join(data.count.month, ukr.admin.level.1.boundaries, by = c("admin1" = "admin1")),
                             sfc_last = T)




# Removing obsolete data
rm(data.count.month.sf); rm(data.count.day.sf); rm(data.sf)



# ------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------


library(spatstat)
library(terra)


# Since the plan is reactive data analysis/exploration, the goals of the plan are outlined hierarchically here
# render data analysis/exploration things output <- reactive models things output in R <- reactive prerequisites for
# model things output <- inputs 

data.points <- data.projected %>%
  filter() %>%
  dplyr::group_by(sub_event_type, X, Y, admin1) %>% 
  dplyr::count(sub_event_type, X, Y)


# Projecting sf objects to be used for the creation of observation windows
ukr.admin.level.0.boundaries <- st_transform(ukr.admin.level.0.boundaries, crs = "EPSG:6381")
ukr.admin.level.1.boundaries <- st_transform(ukr.admin.level.1.boundaries, crs = "EPSG:6381")
st_crs(ukr.admin.level.1.boundaries)
st_crs(ukr.admin.level.0.boundaries)

# Starting observation window creation process
vect.ukr.admin.level.0.boundary <- vect(ukr.admin.level.0.boundaries)
vect.ukr.admin.level.1.boundaries <- vect(ukr.admin.level.1.boundaries)
st_area((ukr.admin.level.1.boundaries %>% filter(admin1 == "Kyiv")))
st_area((ukr.admin.level.1.boundaries %>% filter(admin1 == "Kyiv City")))

ukr.admin.level.1.boundaries.list <- plyr::dlply(ukr.admin.level.1.boundaries, .variables = "admin1", st_sf)


vect.ukr.admin.level.1.boundaries.list <- lapply(ukr.admin.level.1.boundaries.list, terra::vect)
# I need to buffer some of the individual polygons because otherwise, Idk, during the rasterization process
# it losses area, and anyways, what happens is that points are rejected as lying outside their observation
# windows when they are not. So if you don't do this buffering, you'll find that for some administrative regions
# there are points that are rejected when you try to turn it into a ppp, because they lie outside the observation
# window. In any case, I figured out how much to buffer those particular layers by trial and error 

vect.ukr.admin.level.1.boundaries.list[[13]] <- terra::buffer(vect.ukr.admin.level.1.boundaries.list[[13]], width = 10000)
vect.ukr.admin.level.1.boundaries.list[[6]] <- terra::buffer(vect.ukr.admin.level.1.boundaries.list[[6]], width = 20000)
vect.ukr.admin.level.1.boundaries.list[[17]] <- terra::buffer(vect.ukr.admin.level.1.boundaries.list[[17]], width = 21000)
vect.ukr.admin.level.1.boundaries.list[[16]] <- terra::buffer(vect.ukr.admin.level.1.boundaries.list[[16]], width = 22000)
vect.ukr.admin.level.1.boundaries.list[[4]] <- terra::buffer(vect.ukr.admin.level.1.boundaries.list[[4]], width = 26000)
vect.ukr.admin.level.1.boundaries.list[[12]] <- terra::buffer(vect.ukr.admin.level.1.boundaries.list[[12]], width = 4500)



rast.ukr.admin.level.0.boundary <- terra::rasterize(vect.ukr.admin.level.0.boundary,
                                                    rast(
                                                      ncol = 400, nrow = 400,
                                                      crs = "epsg:4326",
                                                      xmin = ext(vect.ukr.admin.level.0.boundary)[1],
                                                      xmax = ext(vect.ukr.admin.level.0.boundary)[2],
                                                      ymin = ext(vect.ukr.admin.level.0.boundary)[3],
                                                      ymax = ext(vect.ukr.admin.level.0.boundary)[4]
                                                    ),
                                                    touches = T)

rast.ukr.admin.level.1.boundaries.list <- list()
for (i in 1:26) {
  rast.ukr.admin.level.1.boundaries.list[[i]] <- terra::rasterize(vect.ukr.admin.level.1.boundaries.list[[i]],
                                                                  rast(
                                                                    ncol = 400, nrow = 400,
                                                                    crs = "epsg:4326",
                                                                    xmin = ext(vect.ukr.admin.level.1.boundaries.list[[i]])[1],
                                                                    xmax = ext(vect.ukr.admin.level.1.boundaries.list[[i]])[2],
                                                                    ymin = ext(vect.ukr.admin.level.1.boundaries.list[[i]])[3],
                                                                    ymax = ext(vect.ukr.admin.level.1.boundaries.list[[i]])[4]
                                                                  ),
                                                                  touches = T)
}

matrices.ukr.admin.level.1.boundaries.list <- lapply(rast.ukr.admin.level.1.boundaries.list, terra::as.matrix, wide = T)
matrices.ukr.admin.level.1.boundaries.list = lapply(matrices.ukr.admin.level.1.boundaries.list, Thermimage::flip.matrix)
matrix.ukr.admin.level.0.boundary <- as.matrix(rast.ukr.admin.level.0.boundary, wide = T)
matrix.ukr.admin.level.0.boundary = Thermimage::flip.matrix(matrix.ukr.admin.level.0.boundary)

# Create images 
images.ukr.admin.level.1.boundaries.list <- list()
for (i in 1:26) {
  images.ukr.admin.level.1.boundaries.list[[i]] <- spatstat.geom::im(matrices.ukr.admin.level.1.boundaries.list[[i]],
                                                                     xcol = seq(from = ext(vect.ukr.admin.level.1.boundaries.list[[i]])[1],
                                                                                to = ext(vect.ukr.admin.level.1.boundaries.list[[i]])[2],
                                                                                length.out = 400),
                                                                     yrow = seq(from = ext(vect.ukr.admin.level.1.boundaries.list[[i]])[3],
                                                                                to = ext(vect.ukr.admin.level.1.boundaries.list[[i]])[4],
                                                                                length.out = 400)
  )
}

image.ukr.admin.level.0.boundary <- spatstat.geom::im(matrix.ukr.admin.level.0.boundary,
                                                      xcol = seq(from = ext(vect.ukr.admin.level.0.boundary)[1],
                                                                 to = ext(vect.ukr.admin.level.0.boundary)[2],
                                                                 length.out = 400),
                                                      yrow = seq(from = ext(vect.ukr.admin.level.0.boundary)[3],
                                                                 to = ext(vect.ukr.admin.level.0.boundary)[4],
                                                                 length.out = 400))


# Converting the names of the images to their original names
names(images.ukr.admin.level.1.boundaries.list) <- names(ukr.admin.level.1.boundaries.list) 

# Turning the images into observation windows
owin.ukr.admin.level.1.boundaries.list <- lapply(images.ukr.admin.level.1.boundaries.list, as.owin.im)
owin.ukr.admin.level.1.boundaries.list <- as.solist(owin.ukr.admin.level.1.boundaries.list)
owin.ukr.admin.level.0.boundary <- spatstat.geom::as.owin.im(image.ukr.admin.level.0.boundary)

# Removing unnecessary data
rm(vect.ukr.admin.level.0.boundary); rm(vect.ukr.admin.level.1.boundaries); rm(vect.ukr.admin.level.1.boundaries.list)
rm(matrices.ukr.admin.level.1.boundaries.list); rm(images.ukr.admin.level.1.boundaries.list); rm(rast.ukr.admin.level.1.boundaries.list)
rm(i); rm(ukr.admin.level.1.boundaries.list); rm(ukr.admin.level.0.boundaries); rm(ukr.admin.level.1.boundaries)
rm(image.ukr.admin.level.0.boundary); rm(matrix.ukr.admin.level.0.boundary); rm(rast.ukr.admin.level.0.boundary)


# # Construction point pattern
data.ppp <- ppp(x = data.points$X,
    y = data.points$Y,
    window = union.owin(owin.ukr.admin.level.1.boundaries.list),
    marks = data.points[,c("sub_event_type", "n")])


owin.ukr.admin.level.0.boundary.scaled <- rescale.im(X = owin.ukr.admin.level.0.boundary, s = 1609.344,
                                                     unitname = "miles")

rm(owin.ukr.admin.level.0.boundary)


