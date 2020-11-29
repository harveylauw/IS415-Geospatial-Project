rm(list=ls()) # remove all variables in global environment
gc()          # force R to release memory

library('leaflet')
library('rstudioapi')
library('rgdal')
library('tidyverse')
library('ggridges')
library('spatstat')
library('raster')
library('maptools')
library('gstat')
library('sp')
library('ggthemes')
library('sf')
library('rgeos')
library('shinydashboard')
library('dplyr')
library('lubridate')
library('tmap')
library('gstat')
library('here')
library('ggnewscale')
library('scico')
library('ggrepel')
library('nngeo')
library('kader')
library('gvlma')
library('spgwr')
library('spdep')
library('shiny')
library('plotly')
library('shinyjs')
library('ggpubr')
library('stpp')
library('png')
library('randomcoloR')
library('grid')
library('gridExtra')
library('shinycssloaders')

dengue <- read_csv("data/dengue outbreak_Singapore_2020.csv")

mpsz <- readOGR(dsn = "data", layer="MP14_SUBZONE_WEB_PL")
mpsz_sp <- as(mpsz, "SpatialPolygons")
mpsz_sp <- spTransform(mpsz_sp, "+init=epsg:3414")
mpsz_owin <- as(mpsz_sp, "owin")

for(i in 1:nrow(dengue)){
    dengue$date[i] <- toString(dengue$date[i])
}
# Assign eweek number to each case 
unique_dates <- unique(dengue$date)
for(i in 1:nrow(dengue)){
    for(j in 1:length(unique_dates)){
        if(dengue$date[i] == unique_dates[j]){
            eweek <- paste0(j)
            dengue$eweek[i] <- eweek
        }
    }
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------
#By eweek - barchart data processing 
dengue_weekly <- dengue %>%
    select ("case ID", "date") %>%
    group_by(date) %>%
    dplyr::summarise(weeklycases = n())

for(i in 1:nrow(dengue_weekly)){
    x <- ymd(paste0("20", dengue_weekly$date[i]))
    dengue_weekly$month[i] <- format(x,"%B")
    month <- dengue_weekly$month[i]
    dengue_weekly$month_num[i] <- match(month, month.name)
}

for(i in 1:nrow(dengue_weekly)){
    eweek <- paste0(i)
    dengue_weekly$eweek[i] <- eweek
}
dengue_weekly$eweek <- factor(dengue_weekly$eweek, levels = dengue_weekly$eweek)
weeks <- seq(1,21,by=1)
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#By month - barchart data processing 
# need to add month column to dengue df (add month then change to month name)
for(i in 1:nrow(dengue)){
    x <- ymd(paste0("20", dengue$date[i]))
    dengue$month[i] <- format(x,"%B")
    month <- dengue$month[i]
    dengue$month_num[i] <- match(month, month.name)
}
dengue_monthly <- dengue_weekly %>%
    select ("weeklycases", "month") %>%
    group_by(month) %>%
    dplyr::summarise(monthlycases = sum(weeklycases)) %>%
    mutate(month = factor(month, levels = month.name)) %>% 
    arrange(month)

#----------------------------------------------------------------------------------
#2nd order point pattern analysis
# Import URA Master Plan 2014 Planning Subzone Dataset
mpsz <- st_read(dsn = "data", 
                layer = "MP14_SUBZONE_WEB_PL")
mpsz <- st_set_crs(mpsz, 3414)
mpsz <- st_make_valid(mpsz)

dengue_sf <- st_as_sf(dengue, coords = c("longitude", "latitude"), crs=4326) %>%
    st_transform(crs=3414)

dengue_sf_filtered <- NULL
makeReactiveBinding("dengue_sf_filtered")

## Create Owin
#From sf data.frame to spdf
spdf_mpsz <- as_Spatial(mpsz)

dengue_selected_ppp <- NULL
makeReactiveBinding("dengue_selected_ppp")

numberOfDengueCases <- 0
makeReactiveBinding("numberOfDengueCases")

# Function to plot L function CSR test
plotLFunctionCSRFn <- function(inputTitle, input_csr, input_function_type){
    
    input_csr_df <- as.data.frame(input_csr)
    colour=c("#0D657D","#ee770d","#D3D3D3")  
    
    Lplot <- ggplot(input_csr_df, aes(r, obs-r)) +
        # plot observed value
        geom_line(colour=c("#4d4d4d"))+
        geom_line(aes(r,theo-r), colour="red", linetype = "dashed")+
        # plot simulation envelopes
        geom_ribbon(aes(ymin=lo-r,ymax=hi-r),alpha=0.1, colour=c("#91bfdb")) +
        # SVY 21 unit is in m
        xlab("Distance d (m)") +
        ylab(paste(input_function_type,"(r)-r",sep="")) +
        geom_rug(data=input_csr_df[input_csr_df$obs > input_csr_df$hi,], sides="b", colour=colour[1])  +
        geom_rug(data=input_csr_df[input_csr_df$obs < input_csr_df$lo,], sides="b", colour=colour[2]) +
        geom_rug(data=input_csr_df[input_csr_df$obs >= input_csr_df$lo & input_csr_df$obs <= input_csr_df$hi,], sides="b", color=colour[3]) +
        theme_tufte() +
        ggtitle(inputTitle)
    
    text1<-"Significant clustering"
    text2<-"Significant dispersion"
    text3<-"No significant clustering/dispersion"
    
    # conditional statement is required to ensure that the labels (text1/2/3) are assigned to the correct traces
    if (nrow(input_csr_df[input_csr_df$obs > input_csr_df$hi,])==0){ 
        if (nrow(input_csr_df[input_csr_df$obs < input_csr_df$lo,])==0){ 
            ggplotly(Lplot, dynamicTicks=T) %>%
                style(text = text3, traces = 4) %>%
                rangeslider() 
        }else if (nrow(input_csr_df[input_csr_df$obs >= input_csr_df$lo & input_csr_df$obs <= input_csr_df$hi,])==0){ 
            ggplotly(Lplot, dynamicTicks=T) %>%
                style(text = text2, traces = 4) %>%
                rangeslider() 
        }else {
            ggplotly(Lplot, dynamicTicks=T) %>%
                style(text = text2, traces = 4) %>%
                style(text = text3, traces = 5) %>%
                rangeslider() 
        }
    } else if (nrow(input_csr_df[input_csr_df$obs < input_csr_df$lo,])==0){
        if (nrow(input_csr_df[input_csr_df$obs >= input_csr_df$lo & input_csr_df$obs <= input_csr_df$hi,])==0){
            ggplotly(Lplot, dynamicTicks=T) %>%
                style(text = text1, traces = 4) %>%
                rangeslider() 
        } else{
            ggplotly(Lplot, dynamicTicks=T) %>%
                style(text = text1, traces = 4) %>%
                style(text = text3, traces = 5) %>%
                rangeslider()
        }
    } else{
        ggplotly(Lplot, dynamicTicks=T) %>%
            style(text = text1, traces = 4) %>%
            style(text = text2, traces = 5) %>%
            style(text = text3, traces = 6) %>%
            rangeslider()
    }
    
}

PA_list <- unique(sort(mpsz$PLN_AREA_N))

#----------------------------------------------------------------------------------
#Data processing
master_list <- read_csv("data/temp_and_rainfall.csv"
                        , col_types = cols(.default = 'd', Station = "c"))
station_records <- read_csv("data/Station_Records.csv")
master_list <- master_list %>%
    mutate(Date = as.Date(paste(Year, Month, Day, sep='-'))
           , Month_abbreviation = case_when(Month == 1 ~ 'January',
                                            Month == 2 ~ 'February',
                                            Month == 3 ~ 'March',
                                            Month == 4 ~ 'April',
                                            Month == 5 ~ 'May',
                                            Month == 6 ~ 'June',
                                            Month == 7 ~ 'July',
                                            Month == 8 ~ 'August',
                                            Month == 9 ~ 'September',
                                            Month == 10 ~ 'October',
                                            Month == 11 ~ 'November',
                                            Month == 12 ~ 'December'))
final_data <- left_join(master_list[,c('Station','Date','Year','Month', 'Day','Month_abbreviation','Daily Rainfall Total','Highest 30 Min Rainfall','Highest 60 Min Rainfall','Highest 120 Min Rainfall','Mean Temperature', 'Maximum Temperature', 'Minimum Temperature')],
                        station_records[,c('Station','Longitude', 'Latitude')],
                        by = c("Station" = "Station"))
names(final_data)<-str_replace_all(names(final_data),
                                   c(" " = "_"))
final_data$Month_abbreviation <- factor(final_data$Month_abbreviation,
                                        levels=c("January","February","March","April","May", "June", "July", "August","September", "October", "November", "December"))

final_data <- final_data %>%
    subset(Year == 2020)

summary(final_data)

#Replace NA values with median
list_na <- colnames(final_data)[ apply(final_data, 2, anyNA) ]
median_missing <- apply(final_data[,colnames(final_data) %in% list_na],
                        2,
                        median,
                        na.rm =  TRUE)
final_data <- final_data %>%
    mutate(Average_daily_rainfall  = ifelse(is.na(Daily_Rainfall_Total),
                                            median_missing[1], Daily_Rainfall_Total),
           Average_daily_temperature  = ifelse(is.na(Mean_Temperature),
                                               median_missing[5], Mean_Temperature))
final_GIS_data <- final_data %>%
    group_by(Station) %>%
    summarise(Average_daily_rainfall = mean(Average_daily_rainfall, na.rm=TRUE)
              ,Average_daily_temperature = mean(Average_daily_temperature, na.rm=TRUE)
              ,Longitude = mean(Longitude, na.rm=TRUE)
              ,Latitude = mean(Latitude, na.rm=TRUE)) %>%
    ungroup()

#Create Climate dataframe
coords <- SpatialPoints(final_GIS_data[,c("Longitude", 
                                          "Latitude")])
Climate_data_df <- SpatialPointsDataFrame(coords,final_GIS_data)

#Create Boundary dataframe
Singapore_boundary_df <- readOGR(dsn = "data", layer = "SingaporeBoundary65")

Climate_data_df@bbox <- Singapore_boundary_df@bbox

rainfall_banner <- final_data %>%
    arrange(-Average_daily_rainfall)

temperature_banner <- final_data %>%
    arrange(-Average_daily_temperature)

#----------------------------------------------------------------------------------

# Generate Thiessen polygons
th <-  as(dirichlet(as.ppp(Climate_data_df)), "SpatialPolygons")
proj4string(th) <- proj4string(Climate_data_df)
th.z <- over(th, Climate_data_df)

th.spdf <-  SpatialPolygonsDataFrame(th, th.z)
proj4string(th.spdf) <- proj4string(Singapore_boundary_df)
#Clip Thiessen polygon together with Singapore's boundary layer
th.clp <- raster::intersect(Singapore_boundary_df ,th.spdf)
#----------------------------------------------------------------------------------

# Generate Raster grids
grid <- as.data.frame(spsample(Climate_data_df, "regular", n=100000))
names(grid) <- c("X", "Y")
coordinates(grid) <- c("X", "Y")
gridded(grid) <- TRUE  # Create SpatialPixel object
fullgrid(grid) <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
proj4string(grid) <- proj4string(Climate_data_df)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
Climate_data_df.idw <- gstat::idw(Average_daily_rainfall ~ 1, Climate_data_df, newdata=grid, idp=2.0)
#----------------------------------------------------------------------------------

#Generate interpolated Rainfall Map
#1st order polynomial
f.1 <- as.formula(Average_daily_rainfall ~ X + Y) 

# Add X and Y to P
Climate_data_df$X <- coordinates(Climate_data_df)[,1]
Climate_data_df$Y <- coordinates(Climate_data_df)[,2]

# Compute the sample variogram; note that the f.1 trend model is one of the
# parameters passed to variogram(). This tells the function to create the variogram on the de-trended data.
var.smpl <- variogram(f.1, Climate_data_df, cloud = FALSE, cutoff=1000000, width=89900)

# Compute the variogram model by passing the nugget, sill and range values to fit.variogram() via the vgm() function.
dat.fit  <- fit.variogram(var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                          vgm(psill=14, model="Sph", range=590000, nugget=0))

# Perform the krige interpolation (note the use of the variogram model created in the earlier step)
dat.krg <- krige( f.1, Climate_data_df, grid, dat.fit)

dat.krg@proj4string <- Climate_data_df@proj4string

# Convert kriged surface to a raster object for clipping
r <- raster(dat.krg)
rainfall_r_m <- mask(r, Singapore_boundary_df)
proj4string(rainfall_r_m) <- proj4string(Singapore_boundary_df)
proj4string(Climate_data_df) <- proj4string(rainfall_r_m)

#plot
rainfall_raster <- tm_shape(rainfall_r_m) + 
    tm_raster(n=10, palette="Blues", stretch.palette=TRUE, 
              title="Rainfall map \n(mm)", alpha =0.7, midpoint = NA) +
    tm_shape(Climate_data_df) + tm_dots(size=0.05) +
    tm_legend(legend.outside=TRUE)
#----------------------------------------------------------------------------------

# Extract raster information to polygon df
Singapore_subzone <- readOGR(dsn = "data", layer = "MP14_SUBZONE_WEB_PL")

Singapore_subzone@bbox <- Singapore_boundary_df@bbox
Singapore_subzone <- st_as_sf(Singapore_subzone, crs=4326)

raster_extract = function(x, y, fun = NULL, na.rm = FALSE) {
    x = as(x, "Raster")
    y = as(y, "Spatial")
    raster::extract(x = x, y = y, fun = fun, na.rm = na.rm)
}

Singapore_subzone <-
    Singapore_subzone %>% mutate(
        Rainfall_Mean = raster_extract(rainfall_r_m, Singapore_subzone, fun = mean, na.rm = FALSE),
        Rainfall_Max = raster_extract(rainfall_r_m, Singapore_subzone, fun = max, na.rm = FALSE),
        Rainfall_Min = raster_extract(rainfall_r_m, Singapore_subzone, fun = min, na.rm = FALSE)
    )

Singapore_subzone %>%
    st_set_geometry(NULL) %>%
    knitr::kable()

temperature_final_GIS_data <- final_data %>%
    group_by(Station) %>%
    summarise(Average_daily_rainfall = mean(Average_daily_rainfall, na.rm=TRUE)
              ,Average_daily_temperature = mean(Average_daily_temperature, na.rm=TRUE)
              ,Longitude = mean(Longitude, na.rm=TRUE)
              ,Latitude = mean(Latitude, na.rm=TRUE)) %>%
    ungroup()

temperature_coords <- SpatialPoints(temperature_final_GIS_data[,c("Longitude", 
                                                                  "Latitude")])
temperature_Climate_data_df <- SpatialPointsDataFrame(temperature_coords,temperature_final_GIS_data)

temperature_Singapore_boundary_df <- readOGR(dsn = "data", layer = "SingaporeBoundary65")

temperature_Climate_data_df@bbox <- temperature_Singapore_boundary_df@bbox
temperature_th <-  as(dirichlet(as.ppp(temperature_Climate_data_df)), "SpatialPolygons")
proj4string(temperature_th) <- proj4string(temperature_Climate_data_df)
temperature_th.z <- over(temperature_th, temperature_Climate_data_df)

temperature_th.spdf <-  SpatialPolygonsDataFrame(temperature_th, temperature_th.z)
proj4string(temperature_th.spdf) <- proj4string(temperature_Singapore_boundary_df)
temperature_th.clp <- raster::intersect(temperature_Singapore_boundary_df ,temperature_th.spdf)
temperature_grid <- as.data.frame(spsample(temperature_Climate_data_df, "regular", n=100000))
names(temperature_grid) <- c("X", "Y")
coordinates(temperature_grid) <- c("X", "Y")
gridded(temperature_grid) <- TRUE  # Create SpatialPixel object
fullgrid(temperature_grid) <- TRUE  # Create SpatialGrid object

proj4string(temperature_grid) <- proj4string(temperature_Climate_data_df)

temperature_Climate_data_df.idw <- gstat::idw(Average_daily_temperature ~ 1,
                                              temperature_Climate_data_df,
                                              newdata=temperature_grid,
                                              idp=2.0)

temperature_f.1 <- as.formula(Average_daily_temperature ~ X + Y) 

temperature_Climate_data_df$X <- coordinates(temperature_Climate_data_df)[,1]
temperature_Climate_data_df$Y <- coordinates(temperature_Climate_data_df)[,2]

temperature_var.smpl <- variogram(temperature_f.1,
                                  temperature_Climate_data_df,
                                  cloud = FALSE,
                                  cutoff=1000000,
                                  width=89900)

temperature_dat.fit  <- fit.variogram(temperature_var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                                      vgm(psill=14, model="Sph", range=590000, nugget=0))

temperature_dat.krg <- krige( temperature_f.1, temperature_Climate_data_df, temperature_grid, temperature_dat.fit)

temperature_r <- raster(temperature_dat.krg)
temperature_r_m <- mask(temperature_r, temperature_Singapore_boundary_df)
proj4string(temperature_r_m) <- proj4string(temperature_Singapore_boundary_df)
proj4string(temperature_Climate_data_df) <- proj4string(temperature_r_m)


#plot
temperature_raster <- tm_shape(temperature_r_m) + 
    tm_raster(n=10, palette="Reds", stretch.palette=TRUE, 
              title="Temperature map \n(degrees celsius)", alpha =0.7, midpoint = NA) +
    tm_shape(temperature_Climate_data_df) + tm_dots(size=0.05) +
    tm_legend(legend.outside=TRUE)

Singapore_subzone <-
    Singapore_subzone %>% mutate(
        Temperature_Mean = raster_extract(temperature_r_m, Singapore_subzone, fun = mean, na.rm = FALSE),
        Temperature_Max = raster_extract(temperature_r_m, Singapore_subzone, fun = max, na.rm = FALSE),
        Temperature_Min = raster_extract(temperature_r_m, Singapore_subzone, fun = min, na.rm = FALSE)
    )

Singapore_subzone %>%
    st_set_geometry(NULL) %>%
    knitr::kable()

# Replace NA values
Singapore_subzone$Rainfall_Mean[is.na(Singapore_subzone$Rainfall_Mean)] <- mean(Singapore_subzone$Rainfall_Mean, na.rm=T)

Singapore_subzone$Rainfall_Max[is.na(Singapore_subzone$Rainfall_Max)] <- mean(Singapore_subzone$Rainfall_Max, na.rm=T)

Singapore_subzone$Rainfall_Min[is.na(Singapore_subzone$Rainfall_Min)] <- mean(Singapore_subzone$Rainfall_Min, na.rm=T)

Singapore_subzone$Temperature_Mean[is.na(Singapore_subzone$Temperature_Mean)] <- mean(Singapore_subzone$Temperature_Mean, na.rm=T)

Singapore_subzone$Temperature_Max[is.na(Singapore_subzone$Temperature_Max)] <- mean(Singapore_subzone$Temperature_Max, na.rm=T)

Singapore_subzone$Temperature_Min[is.na(Singapore_subzone$Temperature_Min)] <- mean(Singapore_subzone$Temperature_Min, na.rm=T)
#----------------------------------------------------------------------------------

#Importing Residents data from 2011 to 2019, csv file, into r environment
residents <- read_csv("data/planning-area-subzone-age-group-sex-and-type-of-dwelling-june-2011-2019.csv")

residents_2019_dwelling <- residents %>%
    filter(year == 2019) %>%
    group_by(planning_area,subzone,type_of_dwelling) %>%
    summarise('resident_count' = sum(resident_count)) %>%
    ungroup %>%
    spread(type_of_dwelling, resident_count) %>%
    mutate('TOTAL' = rowSums(.[3:10]))

residents_2019_gender <- residents %>%
    filter(year == 2019) %>%
    group_by(planning_area,subzone,sex) %>%
    summarise('resident_count' = sum(resident_count)) %>%
    ungroup %>%
    spread(sex, resident_count)

residents_2019_dwelling_gender <- inner_join(residents_2019_dwelling,residents_2019_gender, by = "subzone")

residents_2019_dwelling_gender$subzone= toupper(residents_2019_dwelling_gender$subzone)

names(residents_2019_dwelling_gender)[names(residents_2019_dwelling_gender) == 'planning_area.x'] <- 'planning_area'

df2 = data.frame(Singapore_subzone)

Singapore_subzone_2020 <- inner_join(df2, residents_2019_dwelling_gender, by = c("SUBZONE_N" = "subzone")) %>%
    dplyr::select('PLN_AREA_N','SUBZONE_N','Condominiums and Other Apartments','HDB 1- and 2-Room Flats','HDB 3-Room Flats','HDB 4-Room Flats','HDB 5-Room and Executive Flats','HUDC Flats (excluding those privatised)','Landed Properties','Others','Females','Males','TOTAL','Rainfall_Mean','Temperature_Mean','SHAPE_Area','geometry')

Singapore_subzone_2020 <- st_as_sf(Singapore_subzone_2020) 

#Importing Dengue cases data from 2020, csv file, into r environment
dengue_cases <- read_csv("data/dengue outbreak_Singapore_2020.csv")
dengue_cases.sf <- st_as_sf(dengue_cases,
                            coords = c("longitude", "latitude"),crs=4326)

tmap_mode("view")
#plot
dengue_cases_plot <- tm_shape(Singapore_subzone_2020)+
    tm_polygons(alpha = 0.4) +
    tm_shape(dengue_cases.sf) +  
    tm_dots(col = "case ID",
            alpha = 0.6,
            style="quantile") +
    tm_view(set.zoom.limits = c(11,14))

dengue_cases.sf <- dengue_cases.sf %>% st_transform(st_crs(Singapore_subzone_2020))
Singapore_subzone_2020$"No. of Cases" <- lengths(st_intersects(Singapore_subzone_2020, dengue_cases.sf))

names(Singapore_subzone_2020)[names(Singapore_subzone_2020) == 'Rainfall_Mean'] <- 'Rainfall'
names(Singapore_subzone_2020)[names(Singapore_subzone_2020) == 'Temperature_Mean'] <- 'Temperature'

#Convert Matrix type columns to numeric 
Singapore_subzone_2020$Rainfall <- as.numeric(Singapore_subzone_2020$Rainfall)
Singapore_subzone_2020$Temperature <- as.numeric(Singapore_subzone_2020$Temperature)

# Remove non-residential areas & transform variables to remove left-skewness
placeholder_cuberoot <- Singapore_subzone_2020 %>%
    mutate(
        `Condominiums and Other Apartments` = `Condominiums and Other Apartments`**(1/3),
        `HDB 1- and 2-Room Flats` = `HDB 1- and 2-Room Flats`**(1/3),
        `HDB 3-Room Flats` = `HDB 3-Room Flats`**(1/3),
        `HDB 4-Room Flats` = `HDB 4-Room Flats`**(1/3),
        `HDB 5-Room and Executive Flats` = `HDB 5-Room and Executive Flats`**(1/3),
        `Landed Properties` = `Landed Properties`**(1/3),
        `Others` = `Others`**(1/3),
        `Females` = `Females`**(1/3),
        `Males` = `Males`**(1/3),
        `Temperature` = `Temperature`**(1/3),
        `SHAPE_Area` = `SHAPE_Area`**(1/3)
    ) %>%
    dplyr::select(-`HUDC Flats (excluding those privatised)`,-`TOTAL`)

# Min-max Standardization
placeholder_cuberoot <- placeholder_cuberoot %>%
    st_set_geometry(NULL)
placeholder_cuberoot[,3:14] <- scale(placeholder_cuberoot[,3:14])

Regression_ready <- inner_join(Singapore_subzone,placeholder_cuberoot, by = "SUBZONE_N") %>%
    dplyr::select('PLN_AREA_N.x','SUBZONE_N','Condominiums and Other Apartments','HDB 1- and 2-Room Flats','HDB 3-Room Flats','HDB 4-Room Flats','HDB 5-Room and Executive Flats','Landed Properties','Others','Females','Males','Rainfall','Temperature','SHAPE_Area.x','No. of Cases','geometry') 

names(Regression_ready)[names(Regression_ready) == 'PLN_AREA_N.x'] <- 'Planning Area'
names(Regression_ready)[names(Regression_ready) == 'SUBZONE_N.x'] <- 'Subzone'
names(Regression_ready)[names(Regression_ready) == 'SHAPE_Area.x'] <- 'Subzone Area'

Regression_ready <- st_as_sf(Regression_ready, crs=4326)

mlr2 <- lm(formula = `No. of Cases` ~ `HDB 3-Room Flats`+`HDB 5-Room and Executive Flats`+`Landed Properties`+`Others`+`Females`+`Males`+`Rainfall`+`Temperature`, data=Regression_ready)

Regression_ready.res.sf <- cbind(Regression_ready, 
                                 mlr2$residuals) %>%
    rename(`MLR_RES` = `mlr2.residuals`)

Regression_ready.sp <- as_Spatial(Regression_ready.res.sf)

knb <- knn2nb(knearneigh(coordinates(Regression_ready.sp), k=8, longlat = FALSE), row.names=row.names(Regression_ready.sp$No..of.Cases))

nb_lw <- nb2listw(knb, style = 'W')

lm.morantest(mlr2, nb_lw)

# Select Fixed bandwidth
bwG <- gwr.sel(No..of.Cases ~ HDB.3.Room.Flats + HDB.5.Room.and.Executive.Flats + Landed.Properties + Others + Females + Males + Rainfall + Temperature + Subzone.Area, data=Regression_ready.sp, gweight = gwr.Gauss, verbose = FALSE)

# Regress with Fixed bandwith above
gwrG <- gwr(No..of.Cases ~ HDB.3.Room.Flats + HDB.5.Room.and.Executive.Flats + Landed.Properties + Others + Females + Males + Rainfall + Temperature + Subzone.Area, data=Regression_ready.sp, bandwidth = bwG, gweight = gwr.Gauss, hatmatrix = TRUE)

dengue_cases.sf.fixed <- st_as_sf(gwrG$SDF) %>%
    st_transform(crs=3414)

dengue_cases.sf.fixed.svy21 <- st_transform(dengue_cases.sf.fixed, 3414)

#plot
localr2 <- tm_shape(dengue_cases.sf.fixed.svy21) +
    tm_borders(lwd=1) +
    tm_fill("localR2", 
            style = "jenks", 
            palette = "Blues",
            alpha = 0.7)

#plot
pred <- tm_shape(dengue_cases.sf.fixed.svy21) +
    tm_borders(lwd=1) +
    tm_fill("pred", 
            style = "jenks", 
            palette = "Reds",
            alpha = 0.7)

#plot
pred.se <- tm_shape(dengue_cases.sf.fixed.svy21) +
    tm_borders(lwd=1) +
    tm_fill("pred.se", 
            style = "jenks", 
            palette = "Blues",
            alpha = 0.7)

#Spatio-temporal----------------------------------------------------------------------------------------------------------------------------------------
dengue_sf_temporal <- st_as_sf(dengue, coords = c("longitude", "latitude"), crs=4326) %>%
    st_transform(crs=3414)

SG_main_sf <-st_read(dsn = "data", layer = "SG_main") %>% 
    st_set_crs(3414)

SG_planning_area_sf <- st_read(dsn = "data/planning_area_mp2014", layer="MP14_PLNG_AREA_WEB_PL") %>% 
    st_set_crs(3414)%>% 
    filter(!PLN_AREA_N %in% c("NORTH-EASTERN ISLANDS", "SOUTHERN ISLANDS", "WESTERN ISLANDS", "BUKIT MERAH", "JURONG EAST"))

Spatio_temporal_PA_list <- unique(sort(SG_planning_area_sf$PLN_AREA_N))

dengue_sf_temporal_jitter <- NULL
makeReactiveBinding("dengue_sf_temporal_jitter")

dengue_sf_temporal_full <- NULL
makeReactiveBinding("dengue_sf_temporal_full")

SG_main_xy <- NULL
makeReactiveBinding("SG_main_xy")

numberOfSpatialTemporalEDADengueCase <- NULL
makeReactiveBinding("numberOfSpatialTemporalEDADengueCase")

numberOfSpatialTemporalAnalysisDengueCase <- NULL
makeReactiveBinding("numberOfSpatialTemporalAnalysisDengueCase")

SG_planning_area_sf_filtered <- NULL
makeReactiveBinding("SG_planning_area_sf_filtered")
#-----------------------------------------------------------------------------------------------------------------------------------------------------
#UI Code Chunk
header <- dashboardHeader(
    title = "Dengue Pals",
    titleWidth = 200)

sidebar <- dashboardSidebar(
    width=205,
    
    sidebarMenu(
        menuItem(icon = icon("home"),"Introduction",tabName = "tab1"),
        menuItem(icon = icon("chart-line"),"Geo Weighted Regression",tabName = "tab5"),
        menuItem(icon = icon("cloudversify"),"Weather Study",tabName = "tab4"),
        menuItem(icon = icon("map-marked"),"1st Order Spatial Analysis",tabName = "tab2"),
        menuItem(icon = icon("map-marked-alt"),"2nd Order Spatial Analysis",tabName = "tab2_5"),
        menuItem(icon = icon("clock"),"Spatio Temporal EDA",tabName = "tab3"),
        menuItem(icon = icon("clock"),"Spatio Temporal Analysis",tabName = "tab3_5")
    )
)

body <- dashboardBody(
    # Create a tabBox
    tabItems(
        tabItem(
            tabName = "tab1",
            box(
                h2("Dengue Cases Research during COVID-19 Circuit breaker period"),
                h3(uiOutput("text1_1")),
                uiOutput("text1_2"),
                uiOutput("text1_3"),
                uiOutput("text1_4"),
                uiOutput("text1_5"),
                h3(uiOutput("text2_1")),
                uiOutput("text2_2"),
                h3(uiOutput("text3_1")),
                uiOutput("text3_2"),
                uiOutput("text3_3"),
                uiOutput("text3_4"),
                h3(uiOutput("text4_1")),
                uiOutput("text4_2"),
                height = 800,
                width = 24
            )
        ),
        
        tabItem(
            tabName = "tab2",
            box(
                width = 24,
                title = "1st Order Spatial Point Patterns Analysis",
                
                #1st part
                fluidRow(column(6,selectInput("eweek_num", "Eweek Number:",weeks,selected=21))),
                
                fluidRow(
                    column(6, wellPanel(
                        plotOutput(outputId = "eweek_kde") %>% withSpinner(color="#0dc5c1"))),
                    column(6,wellPanel(
                        plotlyOutput(outputId = "eweek_bar") %>% withSpinner(color="#0dc5c1")))
                ),
                
                h5("Confidence level: 0.05"),
                h5("H0 = The distribution of Weekly dengue cases are randomly distributed."),
                h5("H1 = The distribution of Weekly dengue cases are not randomly distributed."),
                
                fluidRow(
                    column(6,h4("Quadrat Analysis by Week")),
                    column(6,h4("Nearest Neighbour Analysis by Week")),
                ),
                
                fluidRow(
                    column(6,wellPanel(verbatimTextOutput(outputId = "eweek_quad") %>% withSpinner(color="#0dc5c1"))),
                    column(6,wellPanel(verbatimTextOutput(outputId = "eweek_neighbour") %>% withSpinner(color="#0dc5c1"))),
                ),
                
                #2nd part
                fluidRow(column(6,selectInput("month_chosen", "Month:",
                                              c("February", "March", "April","May","June","July"), selected = "February")
                )),
                
                fluidRow(
                    column(6, wellPanel(
                        plotOutput(outputId = "month_kde") %>% withSpinner(color="#0dc5c1"))
                    ),
                    column(6,wellPanel(
                        plotlyOutput(outputId = "month_bar") %>% withSpinner(color="#0dc5c1")
                    ))
                ),
                
                h5("Confidence level: 0.05"),
                h5("H0 = The distribution of Monthly dengue cases are randomly distributed."),
                h5("H1 = The distribution of Monthly dengue cases are not randomly distributed."),
                
                fluidRow(
                    column(6,h4("Quadrat Analysis by Month")),
                    column(6,h4("Nearest Neighbour Analysis by Month"))
                ),
                
                fluidRow(
                    column(6,wellPanel(verbatimTextOutput(outputId = "month_quad") %>% withSpinner(color="#0dc5c1"))),
                    column(6,wellPanel(verbatimTextOutput(outputId = "month_neighbour") %>% withSpinner(color="#0dc5c1")))
                ),
                
                #3rd part
                fluidRow(column(6,selectInput("planning_area_chosen", "Planning Area:",
                                              PA_list, selected = "ANG MO KIO"))),
                
                fluidRow(
                    column(6, wellPanel(
                        plotOutput(outputId = "subzone_kde") %>% withSpinner(color="#0dc5c1")
                    )
                    )
                )
            )
        ),
        
        tabItem(
            tabName = "tab2_5",
            box(
                # height = 1500,
                width = 24,
                title = "2nd Order Spatial Point Patterns Analysis",
                
                #Dropdowns
                fluidRow(column(4,selectInput("inputPlanningArea", "Planning Area:",
                                              PA_list, selected = "ANG MO KIO")
                ),
                column(4,selectInput("inputWeek", "EWeek Number:",
                                     seq(1,21,by=1), selected = 21)
                ),
                column(4,selectInput("inputFunction", "Second-Order Analysis Method:",
                                     c("L function", "K function", "G function", "F function"))
                )
                ),
                
                fluidRow(column(4,selectInput("inputCorrectionMethod", "Edge Correction Method:", 
                                              c("Ripley", "none", "border", "bord.modif", "isotropic", "translate", "translation",
                                                "rigid", "good", "best", "all"))
                ),
                column(4,sliderInput("inputConfidenceInterval", "Confidence Interval (%):",
                                     min = 90, max = 99.9, value =95, step = 0.1)
                ),
                column(2.5,offset = 4,conditionalPanel(condition = "(input.inputFunction == 'L function' || input.inputFunction == 'K function')",
                                                       checkboxInput("displayInteractivePlot", "Display Interactive Plot", value = TRUE))
                ),
                column(1.5,offset = 3,actionButton("applyButton", "Apply"))
                ),
                
                tags$h3(textOutput("insufficientPointsMessageOutput")),
                
                fluidRow(column(6,plotlyOutput("distPlot") %>% withSpinner(color="#0dc5c1")),
                         column(6,plotOutput("studyAreaPlot") %>% withSpinner(color="#0dc5c1"))
                ),
                
                htmlOutput("hypothesisMessageOutput"),
                
                #         plotOutput("functionPlot"),
                plotlyOutput("functionCSRInteractivePlot") %>% withSpinner(color="#0dc5c1"),
                plotOutput("functionCSRPlot") %>% withSpinner(color="#0dc5c1")
                
            )
        ),
        
        tabItem(
            tabName = "tab3",
            box(
                width = 24,
                title = "Spatio Temporal EDA",
                
                # h4(uiOutput("spatio_text1_1")),
                # uiOutput("spatio_text1_2"),
                
                fluidRow(
                    column(4,selectInput("inputSpatioTemporalEDAPlanningArea", "Planning Area:",
                                         Spatio_temporal_PA_list, selected="ANG MO KIO")),
                    column(4,sliderInput("inputRange", "EWeek Number:",
                                         min = 1, max = 21,
                                         value = c(1,2))),
                    column(4,selectInput("inputEDAPlotType", "EDA Plot Type:",
                                         c("projection", "mark", "scatter")))
                ),

                actionButton("spatioTemporalApplyButton", "Apply"),

                tags$h3(textOutput("insufficientPointsSpatioTemporalEDAMessageOutput")),

                fluidRow(column(12,plotOutput("spatioTemporalEDAPlot") %>% withSpinner(color="#0dc5c1"))),
            ) 
        ),
        tabItem(
            tabName = "tab3_5",
            box(
                width = 24,
                title = "Spatio Temporal Analysis",
                
                # h4(uiOutput("spatio_text2_1")),
                # uiOutput("spatio_text2_2"),

                fluidRow(
                    column(6,selectInput("inputSpatioTemporalAnalysisPlanningArea", "Planning Area:",
                                         Spatio_temporal_PA_list, selected="ANG MO KIO")),
                    column(6,sliderInput("inputSpatioTemporalRange", "EWeek Number:",
                                         min = 1, max = 21,
                                         value = c(1,2)))
                    ),

                actionButton("spatioTemporalAnalysisApplyButton", "Apply"),

                fluidRow(
                    column(12,h4("Note: Due to a bug in the stpp:plotK() function, please hide the Navigation Bar for the STIKhat Visualizations to be displayed. Additionally, a larger range of eweeks will require a longer waiting time for the STIKhat results to be generated.")),
                ),

                tags$h3(textOutput("insufficientPointsSpatioTemporalAnalysisMessageOutput")),

                fluidRow(
                    column(6,plotOutput("spatioTemporalAnalysisAreaPlot")%>% withSpinner(color="#0dc5c1")),
                    column(6,plotOutput("contourPlot")%>% withSpinner(color="#0dc5c1"))
                ),
                fluidRow(
                    column(6,plotOutput("perspPlot")%>% withSpinner(color="#0dc5c1")),
                    column(6,plotOutput("imagePlot")%>% withSpinner(color="#0dc5c1"))
                ),
                
            ) 
        ), 
        tabItem(
            tabName = "tab4",
            box(
                width = 24,
                title = "Weather Information in Singapore",
                
                #Banners for Weather study page
                fluidRow(valueBoxOutput("weather_banner1"),
                         valueBoxOutput("weather_banner2")
                ),
                fluidRow(valueBoxOutput("weather_banner3"),
                         valueBoxOutput("weather_banner4")
                ),      
                
                #Dropdown of Geospatial Interpolation
                fluidRow(column(6,selectInput("type_tab4","Select Type of layer",
                                              choices = c("Main","Variance","Confidence Interval"),
                                              selected = "Main", multiple = FALSE))),
                
                fluidRow(column(6,h3('Rainfal Interpolation:')),
                         column(6,h3('Temperature Interpolation:'))),
                
                fluidRow(column(6,leafletOutput('rainfallGIS') %>% withSpinner(color="#0dc5c1")),
                         column(6,leafletOutput('temperatureGIS') %>% withSpinner(color="#0dc5c1"))
                )
            )
        ),
        tabItem(
            tabName = "tab5",
            box(
                width = 24,
                title = "Geographically Weighted Regression",
                
                fluidRow(column(6,selectInput("type_tab5","Select Type of layer",
                                              choices = c("Prediction","Local R2","Prediction Standard error"),
                                              selected = "Prediction", multiple = FALSE))
                ),
                
                h5(uiOutput("text5_1")),
                
                fluidRow(column(6,h3('Predicted Number of Dengue Cases:')),
                         column(6,h3('Breakdown of Regression Outcome by Subzone:'))),
                
                fluidRow(column(6,leafletOutput('dengueGWR') %>% withSpinner(color="#0dc5c1")),
                         column(6,leafletOutput("dengueCases") %>% withSpinner(color="#0dc5c1"))),
                
                fluidRow(column(6,selectInput("variable_tab5","Select Variable used in GWR:",
                                              choices = c("Condominiums and Other Apartments","HDB 1- and 2-Room Flats",
                                                          "HDB 3-Room Flats","HDB 4-Room Flats",
                                                          "HDB 5-Room and Executive Flats","Landed Properties",
                                                          "Others","Females",
                                                          "Males","Rainfall",
                                                          "Temperature","Subzone Area Size"),
                                              selected = "Condominiums and Other Apartments", multiple = FALSE))
                ),
                
                h5(uiOutput("text5_2")),
                
                fluidRow(column(6,plotlyOutput('GWR_variable')),
                         column(6,plotlyOutput("GWR_variable_cuberoot"))
                ),
            )
        )
    )
)

class(body) <- class(header)

ui <- dashboardPage(header, sidebar, body, skin = "green", useShinyjs())

server <- function(session, input, output) {
    
    url1 <- a("Click Here", href="https://denguepals.netlify.app/")
    
    url2 <- a("Harvey Kristanto Lauw", href="https://www.linkedin.com/in/harvey-lauw-462479157/")
    
    url3 <- a("Sin Myeong Eun", href="https://www.linkedin.com/in/myeong-eun/")
    
    url4 <- a("Loh Yuan Jian Keith", href="https://www.linkedin.com/in/keith-loh/")
    
    output$text1_1 <- renderUI({"This tool aids in:"})
    output$text1_2 <- renderUI({"- Study the dengue clusters across Singapore from February 2020 to July 2020 inclusive of the circuit breaker period"})
    output$text1_3 <- renderUI({"- Allow users to interact within the application to analyse the different geospatial aspects of how dengue spread in Singapore"})
    output$text1_4 <- renderUI({"- Provide insightful results of dengue clusters, analyse how weather changes & Singapore Population demographics have contributed to the trend in dengue cases"})
    output$text1_5 <- renderUI({"- Study temporal changes over the past few months by implementing appropriate geospatial analysis methods"})
    
    output$text2_1 <- renderUI({"Project Write up:"})
    output$text2_2 <- renderUI({tagList(url1)})
    
    output$text3_1 <- renderUI({"Team members:"})
    output$text3_2 <- renderUI({tagList(url2)})
    output$text3_3 <- renderUI({tagList(url3)})
    output$text3_4 <- renderUI({tagList(url4)})
    
    output$text4_1 <- renderUI({"Last Updated by:"})
    output$text4_2 <- renderUI({"29 November 2020"})
    
    inputPlanningArea <- reactiveVal()
    inputWeek <- reactiveVal()
    numberOfDengueCasesVal <- reactiveVal()
    inputFunction <- reactiveVal()
    inputCorrectionMethod <- reactiveVal()
    inputConfidenceInterval <- reactiveVal()
    displayInteractivePlot <- reactiveVal()
    
    displayInteractivePlot(TRUE)
    shinyjs::show("functionCSRInteractivePlot")
    shinyjs::hide("functionCSRPlot")
    
    # Change edge correction method based on selected function
    observeEvent(input$inputFunction, {
        if(input$inputFunction == 'L function' || input$inputFunction == 'K function'){
            updateSelectInput(session,
                              inputId="inputCorrectionMethod",
                              choices=c("Ripley", "none", "border", "bord.modif", "isotropic", "translate", "translation",
                                        "rigid", "good", "best", "all"))
        }
        else if(input$inputFunction == 'G function'){
            updateSelectInput(session,
                              inputId="inputCorrectionMethod",
                              choices=c("none", "rs", "km", "Hanisch", "best", "all"))
        }
        else if(input$inputFunction == 'F function'){
            updateSelectInput(session,
                              inputId="inputCorrectionMethod",
                              choices=c("none", "rs", "km", "cs", "best", "all"))
        }
    })
    
    # Main logic
    observeEvent(input$applyButton, {
        inputPlanningArea(input$inputPlanningArea)
        inputWeek(input$inputWeek)
        inputFunction(input$inputFunction)
        inputCorrectionMethod(input$inputCorrectionMethod)
        inputConfidenceInterval(round(2/(1-(input$inputConfidenceInterval/100)), digits=0))
        if(input$inputFunction == 'L function' || input$inputFunction == 'K function'){
            displayInteractivePlot(input$displayInteractivePlot)
            if(displayInteractivePlot()){
                shinyjs::show("functionCSRInteractivePlot")
                shinyjs::hide("functionCSRPlot")
            }
            else{
                shinyjs::hide("functionCSRInteractivePlot")
                shinyjs::show("functionCSRPlot")
            }
        }
        else{
            displayInteractivePlot(FALSE)
            shinyjs::hide("functionCSRInteractivePlot")
            shinyjs::show("functionCSRPlot")
        }
        
        print(inputPlanningArea())
        print(inputWeek())
        print(inputFunction())
        print(numberOfDengueCasesVal())
        print(displayInteractivePlot())
        print(inputCorrectionMethod())
        print(inputConfidenceInterval())
        
    }, ignoreNULL = F)
    
    
    output$insufficientPointsMessageOutput <- renderText({ 
        if(numberOfDengueCasesVal()){
            if(inputPlanningArea() == "ALL"){
                paste("There are ", numberOfDengueCases, " dengue cases at all Planning Areas in eweek ", inputWeek(), ".", sep="")
            }
            else{
                paste("There are ", numberOfDengueCases, " dengue cases at ", inputPlanningArea(), " in eweek ", inputWeek(), ".", sep="")
            }
        }
        else{
            paste("There are insufficient Dengue Cases for Point Patterns Analysis at ", inputPlanningArea(), " in eweek ", inputWeek() , ". Please select other options.", sep="")
        }
    })
    
    output$distPlot <- renderPlotly({
        
        selected_mpsz_freq <- mpsz[mpsz$PLN_AREA_N == inputPlanningArea(), ]
        freq_intersection <- st_intersection(selected_mpsz_freq, dengue_sf)
        dengue_sf$eweek <- as.numeric(dengue_sf$eweek)
        if(nrow(freq_intersection) >0){
            freq_intersection <- freq_intersection %>% 
                group_by(eweek) %>% 
                dplyr::summarize(count = n())
            temp_weeks <- data.frame(eweek = seq(1,21,by=1))
            temp_merged <- merge(temp_weeks, freq_intersection, all = TRUE)
            temp_merged[is.na(temp_merged)] <- 0
            
            ggplotly(ggplot(temp_merged) +
                         geom_bar(aes(x=eweek, y=count), fill = "deepskyblue4", stat="identity") +
                         theme_classic() +
                         scale_x_continuous(breaks = c(range(dengue_sf$eweek)[1]:range(dengue_sf$eweek)[2])) +
                         labs(title=paste(inputPlanningArea(), " Dengue Cases per Week", sep = ""), x="eweek", y="Number of Cases"))
        }
        
    })
    
    output$studyAreaPlot <- renderPlot({
        
        dengue_sf_filtered <<- filter(dengue_sf, eweek == inputWeek())
        # 2nd Order Spatial Point Patterns Analysis
        ## Convert from sf data.frame to spatstat’s ppp format
        ### From sf data.frame to spdf
        spdf_dengue <- as_Spatial(dengue_sf_filtered)
        ### From spdf to SpatialPoints
        sp_dengue <- as(spdf_dengue, "SpatialPoints")
        ### From SpatialPoints to spatstat’s ppp format
        ppp_dengue <- as(sp_dengue, "ppp")
        
        ## Handle duplicated points
        ppp_dengue_jit <- rjitter(ppp_dengue, retry=TRUE, nsim=1, drop=TRUE)
        
        spdf_selected <- spdf_mpsz[spdf_mpsz@data$PLN_AREA_N == inputPlanningArea(), ]
        
        # From spdf to SpatialPolygons
        sp_selected <- as(spdf_selected, "SpatialPolygons")
        # From SpatialPolygons to owin object
        owin_selected <- as(sp_selected, "owin")
        
        # Combine dengue points with study area
        dengue_selected_ppp = ppp_dengue[owin_selected]
        dengue_selected_ppp <<- dengue_selected_ppp
        
        numberOfDengueCases <<- npoints(dengue_selected_ppp)
        print(numberOfDengueCases)
        if(numberOfDengueCases <10){
            
            numberOfDengueCasesVal(FALSE)
        }
        else{
            numberOfDengueCasesVal(TRUE)
            plot(dengue_selected_ppp, paste("Dengue Event Points - ", inputPlanningArea() , ", eweek: ", inputWeek(), sep=""))
        }
        
        
    })
    
    output$functionPlot <- renderPlot({
        if(numberOfDengueCasesVal()){
            if(inputFunction() == "L function"){
                function_selected = Lest(dengue_selected_ppp, correction = inputCorrectionMethod())
                plot(function_selected, . -r ~ r, ylab= "L(d)-r", xlab = "d(m)", main = paste("L_", inputPlanningArea(), sep=""))
            }
            else if(inputFunction() == "K function"){
                function_selected = Kest(dengue_selected_ppp, correction = inputCorrectionMethod())
                plot(function_selected, . -r ~ r, ylab= "K(d)-r", xlab = "d(m)", main = paste("K_", inputPlanningArea(), sep=""))
            }
            else if(inputFunction() == "G function"){
                function_selected = Gest(dengue_selected_ppp, correction = inputCorrectionMethod())
                plot(function_selected, main = paste("G_", inputPlanningArea(), sep=""))
            }
            else{
                function_selected = Fest(dengue_selected_ppp, correction = inputCorrectionMethod())
                plot(function_selected, main = paste("F_", inputPlanningArea(), sep=""))
            }
        }
    })
    
    output$hypothesisMessageOutput <- renderUI({ 
        if(numberOfDengueCasesVal()){
            hypothesis_title <- "<b><u>Complete Spatial Randomness Test:</u></b>"
            hypothesis_part1 <- paste("H0 = The distribution of dengue cases at ", inputPlanningArea(), " in eweek ", inputWeek(), " are randomly distributed.", sep="")
            hypothesis_part2 <- paste("H1 = The distribution of dengue cases at ", inputPlanningArea(), " in eweek ", inputWeek(), " are not randomly distributed.", sep="")
            hypothesis_final <- paste("<i>The null hypothesis will be rejected if p-value is smaller than alpha value of ", 1-(input$inputConfidenceInterval/100), " (", input$inputConfidenceInterval , "% Confidence Interval).</i>", sep="")
            HTML(paste(hypothesis_title, hypothesis_part1, hypothesis_part2, hypothesis_final, "<br>", sep="<br>"))
        }
    })
    
    
    output$functionCSRInteractivePlot <- renderPlotly({
        print("DEFAULT INTERACTIVE OPTION:")
        print(displayInteractivePlot())
        
        if(numberOfDengueCasesVal() & displayInteractivePlot()){
            set.seed(50)
            if(inputFunction() == "L function"){
                function_selected_csr <- envelope(dengue_selected_ppp, Lest, correction = inputCorrectionMethod(), nsim = inputConfidenceInterval()-1, rank = 1, glocal=TRUE)
                plotLFunctionCSRFn(paste("L function - ", inputPlanningArea(), sep=""), function_selected_csr, "L")
            }
            else if(inputFunction() == "K function"){
                function_selected_csr <- envelope(dengue_selected_ppp, Kest, correction = inputCorrectionMethod(), nsim = inputConfidenceInterval()-1, rank = 1, glocal=TRUE)
                plotLFunctionCSRFn(paste("K function - ", inputPlanningArea(), sep=""), function_selected_csr, "K")
            }
        }
        else{
            return(NULL)
        }
    })
    
    output$functionCSRPlot <- renderPlot({
        
        if(numberOfDengueCasesVal() & displayInteractivePlot() == FALSE){
            set.seed(50)
            if(inputFunction() == "L function"){
                function_selected_csr <- envelope(dengue_selected_ppp, Lest, correction = inputCorrectionMethod(), nsim = inputConfidenceInterval()-1, rank = 1, glocal=TRUE)
                plot(function_selected_csr, . - r ~ r, xlab="d", ylab="L(d)-r", main = "L function CSR")
            }
            else if(inputFunction() == "K function"){
                function_selected_csr <- envelope(dengue_selected_ppp, Kest, correction = inputCorrectionMethod(), nsim = inputConfidenceInterval()-1, rank = 1, glocal=TRUE)
                plot(function_selected_csr, . - r ~ r, xlab="d", ylab="K(d)-r", main = "K function CSR")
            }
            else if(inputFunction() == "G function"){
                function_selected_csr <- envelope(dengue_selected_ppp, Gest, correction = inputCorrectionMethod(), nsim = inputConfidenceInterval()-1, rank = 1, glocal=TRUE)
                plot(function_selected_csr, main = "G function CSR")
            }
            else{
                function_selected_csr <- envelope(dengue_selected_ppp, Fest, correction = inputCorrectionMethod(), nsim = inputConfidenceInterval()-1, rank = 1, glocal=TRUE)
                plot(function_selected_csr, main = "F function CSR")
            }
        }
        else{
            return(NULL)
        }
    })
    
    output$weather_banner1 <- renderValueBox({
        valueBox(rainfall_banner$Station[1],"Station with Highest Rainfall",icon = icon("cloud"),
                 color="blue")
    })
    
    output$weather_banner2 <- renderValueBox({
        valueBox(rainfall_banner$Average_daily_rainfall[1],"Highest Rainfall in mm",icon = icon("cloud"),
                 color="blue")
    })
    
    output$weather_banner3 <- renderValueBox({
        valueBox(temperature_banner$Station[1],"Station with Highest Temperature",icon = icon("thermometer-full"),
                 color="yellow")
    })
    
    output$weather_banner4 <- renderValueBox({
        valueBox(temperature_banner$Average_daily_temperature[1],"Highest Temperature in Degrees Celsius",icon = icon("thermometer-full"),
                 color="yellow")
    })
    
    output$rainfallGIS <- renderLeaflet({
        
        rainfall_final_GIS_data <- final_data %>%
            group_by(Station) %>%
            summarise(Average_daily_rainfall = mean(Average_daily_rainfall, na.rm=TRUE)
                      ,Average_daily_temperature = mean(Average_daily_temperature, na.rm=TRUE)
                      ,Longitude = mean(Longitude, na.rm=TRUE)
                      ,Latitude = mean(Latitude, na.rm=TRUE)) %>%
            ungroup()
        
        rainfall_coords <- SpatialPoints(rainfall_final_GIS_data[,c("Longitude", 
                                                                    "Latitude")])
        rainfall_Climate_data_df <- SpatialPointsDataFrame(rainfall_coords,rainfall_final_GIS_data)
        
        rainfall_Singapore_boundary_df <- readOGR(dsn = "data", layer = "SingaporeBoundary65")
        
        rainfall_Climate_data_df@bbox <- rainfall_Singapore_boundary_df@bbox
        tmap_mode("view")
        rainfall_th <-  as(dirichlet(as.ppp(rainfall_Climate_data_df)), "SpatialPolygons")
        proj4string(rainfall_th) <- proj4string(rainfall_Climate_data_df)
        rainfall_th.z <- over(rainfall_th, rainfall_Climate_data_df)
        
        rainfall_th.spdf <-  SpatialPolygonsDataFrame(rainfall_th, rainfall_th.z)
        proj4string(rainfall_th.spdf) <- proj4string(rainfall_Singapore_boundary_df)
        rainfall_th.clp <- raster::intersect(rainfall_Singapore_boundary_df ,rainfall_th.spdf)
        rainfall_grid <- as.data.frame(spsample(rainfall_Climate_data_df, "regular", n=100000))
        names(rainfall_grid) <- c("X", "Y")
        coordinates(rainfall_grid) <- c("X", "Y")
        gridded(rainfall_grid) <- TRUE  # Create SpatialPixel object
        fullgrid(rainfall_grid) <- TRUE  # Create SpatialGrid object
        
        proj4string(rainfall_grid) <- proj4string(rainfall_Climate_data_df)
        
        rainfall_Climate_data_df.idw <- gstat::idw(Average_daily_rainfall ~ 1,
                                                   rainfall_Climate_data_df,
                                                   newdata=rainfall_grid,
                                                   idp=2.0)
        
        rainfall_f.1 <- as.formula(Average_daily_rainfall ~ X + Y) 
        
        rainfall_Climate_data_df$X <- coordinates(rainfall_Climate_data_df)[,1]
        rainfall_Climate_data_df$Y <- coordinates(rainfall_Climate_data_df)[,2]
        
        rainfall_var.smpl <- variogram(rainfall_f.1,
                                       rainfall_Climate_data_df,
                                       cloud = FALSE,
                                       cutoff=1000000,
                                       width=89900)
        
        rainfall_dat.fit  <- fit.variogram(rainfall_var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                                           vgm(psill=14, model="Sph", range=590000, nugget=0))
        
        rainfall_dat.krg <- krige(rainfall_f.1, rainfall_Climate_data_df, rainfall_grid, rainfall_dat.fit)
        
        if (input$type_tab4 == "Main"){
            
            r <- raster(rainfall_dat.krg)
            r_m <- mask(r, rainfall_Singapore_boundary_df)
            proj4string(r_m) <- proj4string(rainfall_Singapore_boundary_df)
            proj4string(rainfall_Climate_data_df) <- proj4string(r_m)
            
            rainfall_pal <- colorNumeric(c("GnBu"),
                                         values(r_m),
                                         na.color = "transparent")
            
            leaflet(data=rainfall_Climate_data_df,options = leafletOptions( minZoom = 10.5,
                                                                            dragging = TRUE)) %>%
                addCircles(lat = ~Latitude, lng = ~Longitude, radius = 4, color = "black", opacity = 0.3,
                           popup = paste("Station:", rainfall_Climate_data_df$Station, "<br>",
                                         "Average daily rainfall: ", rainfall_Climate_data_df$Average_daily_rainfall, "<br>",
                                         "Longitude: ", rainfall_Climate_data_df$Longitude, "<br>",
                                         "Latitude: ", rainfall_Climate_data_df$Latitude),
                           label = ~as.character(Station)) %>%
                addTiles() %>%
                #Provider Tiles can be in "Esri" or "CartoDB.PositronNoLabels"
                addProviderTiles(provider = "CartoDB") %>%
                setView(lng = 103.814564, lat = 1.361085, zoom = 10.5) %>%
                setMaxBounds(lng1 = 103.6,
                             lat1 = 1.16,
                             lng2 = 104.1,
                             lat2 = 1.48) %>%
                addRasterImage(r_m , colors = rainfall_pal, opacity = 0.5) %>%
                addLegend(position = "bottomright",
                          pal = rainfall_pal, values = values(r_m),
                          title = paste("Rainfall (mm)"))
            
        } else if (input$type_tab4  == "Variance"){
            
            r   <- raster(rainfall_dat.krg, layer="var1.var")
            r_m <- mask(r, rainfall_Singapore_boundary_df)
            proj4string(r_m) <- proj4string(rainfall_Singapore_boundary_df)
            proj4string(rainfall_Climate_data_df) <- proj4string(r_m)
            
            variance_pal <- colorBin(c("Reds"),
                                     values(r_m),
                                     na.color = "transparent",
                                     bins = c(0.0000002,0.0000004, 0.0000006, .0000008, .000001))
            
            leaflet(data=rainfall_Climate_data_df,options = leafletOptions( minZoom = 10.5,
                                                                            dragging = TRUE)) %>%
                addCircles(lat = ~Latitude, lng = ~Longitude, radius = 4, color = "black", opacity = 0.3,
                           popup = paste("Station:", rainfall_Climate_data_df$Station, "<br>",
                                         "Average daily rainfall: ", rainfall_Climate_data_df$Average_daily_rainfall, "<br>",
                                         "Longitude: ", rainfall_Climate_data_df$Longitude, "<br>",
                                         "Latitude: ", rainfall_Climate_data_df$Latitude),
                           label = ~as.character(Station)) %>%
                addTiles() %>%
                #Provider Tiles can be in "Esri" or "CartoDB.PositronNoLabels"
                addProviderTiles(provider = "CartoDB") %>%
                setView(lng = 103.814564, lat = 1.361085, zoom = 10.5) %>%
                setMaxBounds(lng1 = 103.6,
                             lat1 = 1.16,
                             lng2 = 104.1,
                             lat2 = 1.48) %>%
                addRasterImage(r_m , colors = variance_pal, opacity = 0.5) %>%
                addLegend(position = "bottomright",
                          pal = variance_pal, values = values(r_m),
                          title = paste("Variance"),
                          labFormat = labelFormat(digits = 9))
            
        } else if (input$type_tab4 == "Confidence Interval"){
            
            r   <- sqrt(raster(rainfall_dat.krg, layer="var1.var")) * 1.96
            r_m <- mask(r, rainfall_Singapore_boundary_df)
            proj4string(r_m) <- proj4string(rainfall_Singapore_boundary_df)
            proj4string(rainfall_Climate_data_df) <- proj4string(r_m)
            
            confidence_interval_pal <- colorBin(c("GnBu"),
                                                values(r_m),
                                                na.color = "transparent", 
                                                bins = c(0.0005,0.001, 0.002, 0.003, 0.004, 0.005))
            
            leaflet(data=rainfall_Climate_data_df,options = leafletOptions( minZoom = 10.5,
                                                                            dragging = TRUE)) %>%
                addCircles(lat = ~Latitude, lng = ~Longitude, radius = 4, color = "black", opacity = 0.3,
                           popup = paste("Station:", rainfall_Climate_data_df$Station, "<br>",
                                         "Average daily rainfall: ", rainfall_Climate_data_df$Average_daily_rainfall, "<br>",
                                         "Longitude: ", rainfall_Climate_data_df$Longitude, "<br>",
                                         "Latitude: ", rainfall_Climate_data_df$Latitude),
                           label = ~as.character(Station)) %>%
                addTiles() %>%
                #Provider Tiles can be in "Esri" or "CartoDB.PositronNoLabels"
                addProviderTiles(provider = "CartoDB") %>%
                setView(lng = 103.814564, lat = 1.361085, zoom = 10.5) %>%
                setMaxBounds(lng1 = 103.6,
                             lat1 = 1.16,
                             lng2 = 104.1,
                             lat2 = 1.48) %>%
                addRasterImage(r_m , colors = confidence_interval_pal, opacity = 0.5) %>%
                addLegend(position = "bottomright",
                          pal = confidence_interval_pal, values = values(r_m),
                          title = paste("Confidence","<br>","Interval 95%"))
        }
    })
    
    output$temperatureGIS <- renderLeaflet({
        
        temperature_final_GIS_data <- final_data %>%
            group_by(Station) %>%
            summarise(Average_daily_rainfall = mean(Average_daily_rainfall, na.rm=TRUE)
                      ,Average_daily_temperature = mean(Average_daily_temperature, na.rm=TRUE)
                      ,Longitude = mean(Longitude, na.rm=TRUE)
                      ,Latitude = mean(Latitude, na.rm=TRUE)) %>%
            ungroup()
        
        temperature_coords <- SpatialPoints(temperature_final_GIS_data[,c("Longitude", 
                                                                          "Latitude")])
        temperature_Climate_data_df <- SpatialPointsDataFrame(temperature_coords,temperature_final_GIS_data)
        
        temperature_Singapore_boundary_df <- readOGR(dsn = "data", layer = "SingaporeBoundary65")
        
        temperature_Climate_data_df@bbox <- temperature_Singapore_boundary_df@bbox
        
        tmap_mode("view")
        
        temperature_th <-  as(dirichlet(as.ppp(temperature_Climate_data_df)), "SpatialPolygons")
        proj4string(temperature_th) <- proj4string(temperature_Climate_data_df)
        temperature_th.z <- over(temperature_th, temperature_Climate_data_df)
        temperature_th.spdf <-  SpatialPolygonsDataFrame(temperature_th, temperature_th.z)
        proj4string(temperature_th.spdf) <- proj4string(temperature_Singapore_boundary_df)
        temperature_th.clp <- raster::intersect(temperature_Singapore_boundary_df ,temperature_th.spdf)
        
        temperature_grid <- as.data.frame(spsample(temperature_Climate_data_df, "regular", n=100000))
        names(temperature_grid) <- c("X", "Y")
        coordinates(temperature_grid) <- c("X", "Y")
        gridded(temperature_grid) <- TRUE  # Create SpatialPixel object
        fullgrid(temperature_grid) <- TRUE  # Create SpatialGrid object
        
        proj4string(temperature_grid) <- proj4string(temperature_Climate_data_df)
        
        temperature_Climate_data_df.idw <- gstat::idw(Average_daily_temperature ~ 1,
                                                      temperature_Climate_data_df,
                                                      newdata=temperature_grid,
                                                      idp=1.0)
        
        temperature_f.1 <- as.formula(Average_daily_temperature ~ X + Y) 
        
        temperature_Climate_data_df$X <- coordinates(temperature_Climate_data_df)[,1]
        temperature_Climate_data_df$Y <- coordinates(temperature_Climate_data_df)[,2]
        
        temperature_var.smpl <- variogram(temperature_f.1,
                                          temperature_Climate_data_df,
                                          cloud = FALSE,
                                          cutoff=1000000,
                                          width=89900)
        
        temperature_dat.fit  <- fit.variogram(temperature_var.smpl, fit.ranges = FALSE, fit.sills = FALSE,
                                              vgm(psill=14, model="Sph", range=590000, nugget=0))
        
        temperature_dat.krg <- krige( temperature_f.1, temperature_Climate_data_df, temperature_grid, temperature_dat.fit)
        
        if (input$type_tab4 == "Main"){
            
            r <- raster(temperature_dat.krg)
            r_m <- mask(r, temperature_Singapore_boundary_df)
            proj4string(r_m) <- proj4string(temperature_Singapore_boundary_df)
            proj4string(temperature_Climate_data_df) <- proj4string(r_m)
            
            temperature_pal <- colorNumeric(c("YlOrRd"),
                                            values(r_m),
                                            na.color = "transparent")
            
            leaflet(data=temperature_Climate_data_df,options = leafletOptions( minZoom = 10.5,
                                                                               dragging = TRUE)) %>%
                addCircles(lat = ~Latitude, lng = ~Longitude, radius = 4, color = "black", opacity = 0.3,
                           popup = paste("Station:", temperature_Climate_data_df$Station, "<br>",
                                         "Average daily temperature: ", temperature_Climate_data_df$Average_daily_temperature, "<br>",
                                         "Longitude: ", temperature_Climate_data_df$Longitude, "<br>",
                                         "Latitude: ", temperature_Climate_data_df$Latitude),
                           label = ~as.character(Station)) %>%
                addTiles() %>%
                #Provider Tiles can be in "Esri" or "CartoDB.PositronNoLabels"
                addProviderTiles(provider = "CartoDB") %>%
                setView(lng = 103.814564, lat = 1.361085, zoom = 10.5) %>%
                setMaxBounds(lng1 = 103.6,
                             lat1 = 1.16,
                             lng2 = 104.1,
                             lat2 = 1.48) %>%
                addRasterImage(r_m , colors = temperature_pal, opacity = 0.5) %>%
                addLegend(position = "bottomright",
                          pal = temperature_pal, values = values(r_m),
                          title = paste("Temperature", "<br>", "(Degrees Celsius)")
                )
            
        } else if (input$type_tab4  == "Variance"){
            
            r   <- raster(temperature_dat.krg, layer="var1.var")
            r_m <- mask(r, temperature_Singapore_boundary_df)
            proj4string(r_m) <- proj4string(temperature_Singapore_boundary_df)
            proj4string(temperature_Climate_data_df) <- proj4string(r_m)
            
            variance_pal <- colorBin(c("Reds"),
                                     values(r_m),
                                     na.color = "transparent",
                                     bins = c(0.0000002,0.0000004, 0.0000006, .0000008, .000001))
            
            leaflet(data=temperature_Climate_data_df,options = leafletOptions( minZoom = 10.5,
                                                                               dragging = TRUE)) %>%
                addCircles(lat = ~Latitude, lng = ~Longitude, radius = 4, color = "black", opacity = 0.3,
                           popup = paste("Station:", temperature_Climate_data_df$Station, "<br>",
                                         "Average daily temperature: ", temperature_Climate_data_df$Average_daily_temperature, "<br>",
                                         "Longitude: ", temperature_Climate_data_df$Longitude, "<br>",
                                         "Latitude: ", temperature_Climate_data_df$Latitude),
                           label = ~as.character(Station)) %>%
                addTiles() %>%
                #Provider Tiles can be in "Esri" or "CartoDB.PositronNoLabels"
                addProviderTiles(provider = "CartoDB") %>%
                setView(lng = 103.814564, lat = 1.361085, zoom = 10.5) %>%
                setMaxBounds(lng1 = 103.6,
                             lat1 = 1.16,
                             lng2 = 104.1,
                             lat2 = 1.48) %>%
                addRasterImage(r_m , colors = variance_pal, opacity = 0.5) %>%
                addLegend(position = "bottomright",
                          pal = variance_pal, values = values(r_m),
                          title = paste("Variance"),
                          labFormat = labelFormat(digits = 6))
            
        } else if (input$type_tab4 == "Confidence Interval"){
            
            r   <- sqrt(raster(temperature_dat.krg, layer="var1.var")) * 1.96
            r_m <- mask(r, temperature_Singapore_boundary_df)
            proj4string(r_m) <- proj4string(temperature_Singapore_boundary_df)
            proj4string(temperature_Climate_data_df) <- proj4string(r_m)
            
            confidence_interval_pal <- colorBin(c("GnBu"),
                                                values(r_m),
                                                na.color = "transparent", 
                                                bins = c(0.0005,0.001, 0.002, 0.003, 0.004, 0.005))
            
            leaflet(data=temperature_Climate_data_df,options = leafletOptions( minZoom = 10.5,
                                                                               dragging = TRUE)) %>%
                addCircles(lat = ~Latitude, lng = ~Longitude, radius = 4, color = "black", opacity = 0.3,
                           popup = paste("Station:", temperature_Climate_data_df$Station, "<br>",
                                         "Average daily temperature: ", temperature_Climate_data_df$Average_daily_temperature, "<br>",
                                         "Longitude: ", temperature_Climate_data_df$Longitude, "<br>",
                                         "Latitude: ", temperature_Climate_data_df$Latitude),
                           label = ~as.character(Station)) %>%
                addTiles() %>%
                #Provider Tiles can be in "Esri" or "CartoDB.PositronNoLabels"
                addProviderTiles(provider = "CartoDB") %>%
                setView(lng = 103.814564, lat = 1.361085, zoom = 10.5) %>%
                setMaxBounds(lng1 = 103.6,
                             lat1 = 1.16,
                             lng2 = 104.1,
                             lat2 = 1.48) %>%
                addRasterImage(r_m , colors = confidence_interval_pal, opacity = 0.5) %>%
                addLegend(position = "bottomright",
                          pal = confidence_interval_pal, values = values(r_m),
                          title = paste("Confidence","<br>","Interval 95%"))
        }
    })
    
    output$text5_1 <- renderUI({"Note: The higher the Case ID, the more recent this dengue case has occurred."})
    
    output$dengueGWR <- renderLeaflet({
        
        if (input$type_tab5 == "Prediction"){
            tmap_leaflet(pred) %>%
                setView(lng = 103.814564, lat = 1.361085, zoom = 10.5) %>%
                setMaxBounds(lng1 = 103.6,
                             lat1 = 1.16,
                             lng2 = 104.1,
                             lat2 = 1.48) 
            
        } else if (input$type_tab5 == "Prediction Standard error"){
            tmap_leaflet(pred.se) %>%
                setView(lng = 103.814564, lat = 1.361085, zoom = 10.5) %>%
                setMaxBounds(lng1 = 103.6,
                             lat1 = 1.16,
                             lng2 = 104.1,
                             lat2 = 1.48)
            
        } else if (input$type_tab5 == "Local R2"){
            tmap_leaflet(localr2) %>%
                setView(lng = 103.814564, lat = 1.361085, zoom = 10.5) %>%
                setMaxBounds(lng1 = 103.6,
                             lat1 = 1.16,
                             lng2 = 104.1,
                             lat2 = 1.48)
        }
    })
    
    output$dengueCases <- renderLeaflet({
        tmap_leaflet(dengue_cases_plot) %>%
            setView(lng = 103.814564, lat = 1.361085, zoom = 10.5) %>%
            setMaxBounds(lng1 = 103.6,
                         lat1 = 1.16,
                         lng2 = 104.1,
                         lat2 = 1.48)
    })
    
    output$text5_2 <- renderUI({"Note: The left histogram contains the variable distribution in their raw form. The right histogram contains the variable distribution after Cube Root Transformation to be used in GWR"})
    
    output$GWR_variable <- renderPlotly({
        if (input$variable_tab5 == "Condominiums and Other Apartments"){
            ggplotly(ggplot(data=Singapore_subzone_2020, aes(x=`Condominiums and Other Apartments`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="light blue") +
                         ggtitle("Original Data"))
        } else if (input$variable_tab5 == "HDB 1- and 2-Room Flats"){
            ggplotly(ggplot(data=Singapore_subzone_2020, aes(x=`HDB 1- and 2-Room Flats`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="light blue") +
                         ggtitle("Original Data"))
        } else if (input$variable_tab5 == "HDB 3-Room Flats"){
            ggplotly(ggplot(data=Singapore_subzone_2020, aes(x=`HDB 3-Room Flats`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="light blue") +
                         ggtitle("Original Data"))
        } else if (input$variable_tab5 == "HDB 4-Room Flats"){
            ggplotly(ggplot(data=Singapore_subzone_2020, aes(x=`HDB 4-Room Flats`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="light blue") +
                         ggtitle("Original Data"))
        } else if (input$variable_tab5 == "HDB 5-Room and Executive Flats"){
            ggplotly(ggplot(data=Singapore_subzone_2020, aes(x=`HDB 5-Room and Executive Flats`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="light blue") +
                         ggtitle("Original Data"))
        } else if (input$variable_tab5 == "Landed Properties"){
            ggplotly(ggplot(data=Singapore_subzone_2020, aes(x=`Landed Properties`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="light blue") +
                         ggtitle("Original Data"))
        } else if (input$variable_tab5 == "Others"){
            ggplotly(ggplot(data=Singapore_subzone_2020, aes(x=`Others`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="light blue") +
                         ggtitle("Original Data"))
        } else if (input$variable_tab5 == "Females"){
            ggplotly(ggplot(data=Singapore_subzone_2020, aes(x=`Females`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="light blue") +
                         ggtitle("Original Data"))
        } else if (input$variable_tab5 == "Males"){
            ggplotly(ggplot(data=Singapore_subzone_2020, aes(x=`Males`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="light blue") +
                         ggtitle("Original Data"))
        } else if (input$variable_tab5 == "Rainfall"){
            ggplotly(ggplot(data=Singapore_subzone_2020, aes(x=`Rainfall`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="light blue") +
                         ggtitle("Original Data"))
        } else if (input$variable_tab5 == "Temperature"){
            ggplotly(ggplot(data=Singapore_subzone_2020, aes(x=`Temperature`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="light blue") +
                         ggtitle("Original Data"))
        } else if (input$variable_tab5 == "Subzone Area Size"){
            ggplotly(ggplot(data=Singapore_subzone_2020, aes(x=`SHAPE_Area`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="light blue") +
                         ggtitle("Original Data"))
        }
    })
    
    output$GWR_variable_cuberoot <- renderPlotly({
        if (input$variable_tab5 == "Condominiums and Other Apartments"){
            ggplotly(ggplot(data=placeholder_cuberoot, aes(x=`Condominiums and Other Apartments`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="orange") +
                         ggtitle("Cube root Transformation")) 
        } else if (input$variable_tab5 == "HDB 1- and 2-Room Flats"){
            ggplotly(ggplot(data=placeholder_cuberoot, aes(x=`HDB 1- and 2-Room Flats`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="orange") +
                         ggtitle("Cube root Transformation"))
        } else if (input$variable_tab5 == "HDB 3-Room Flats"){
            ggplotly(ggplot(data=placeholder_cuberoot, aes(x=`HDB 3-Room Flats`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="orange") +
                         ggtitle("Cube root Transformation"))
        } else if (input$variable_tab5 == "HDB 4-Room Flats"){
            ggplotly(ggplot(data=placeholder_cuberoot, aes(x=`HDB 4-Room Flats`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="orange") +
                         ggtitle("Cube root Transformation"))
        } else if (input$variable_tab5 == "HDB 5-Room and Executive Flats"){
            ggplotly(ggplot(data=placeholder_cuberoot, aes(x=`HDB 5-Room and Executive Flats`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="orange") +
                         ggtitle("Cube root Transformation"))
        } else if (input$variable_tab5 == "Landed Properties"){
            ggplotly(ggplot(data=placeholder_cuberoot, aes(x=`Landed Properties`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="orange") +
                         ggtitle("Cube root Transformation"))
        } else if (input$variable_tab5 == "Others"){
            ggplotly(ggplot(data=placeholder_cuberoot, aes(x=`Others`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="orange") +
                         ggtitle("Cube root Transformation"))
        } else if (input$variable_tab5 == "Females"){
            ggplotly(ggplot(data=placeholder_cuberoot, aes(x=`Females`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="orange") +
                         ggtitle("Cube root Transformation"))
        } else if (input$variable_tab5 == "Males"){
            ggplotly(ggplot(data=placeholder_cuberoot, aes(x=`Males`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="orange") +
                         ggtitle("Cube root Transformation"))
        } else if (input$variable_tab5 == "Rainfall"){
            ggplotly(ggplot(data=placeholder_cuberoot, aes(x=`Rainfall`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="orange") +
                         ggtitle("Cube root Transformation"))
        } else if (input$variable_tab5 == "Temperature"){
            ggplotly(ggplot(data=placeholder_cuberoot, aes(x=`Temperature`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="orange") +
                         ggtitle("Cube root Transformation"))
        } else if (input$variable_tab5 == "Subzone Area Size"){
            ggplotly(ggplot(data=placeholder_cuberoot, aes(x=`SHAPE_Area`)) +
                         theme_classic() +
                         geom_histogram(bins=10, color="black", fill="orange") +
                         ggtitle("Cube root Transformation"))
        }
    })
    
    output$eweek_bar <- renderPlotly({
        ggplotly(ggplot(dengue_weekly, aes(x=eweek, y=weeklycases)) +
                     theme_classic() +
                     geom_bar(stat = "identity", color="black",fill = "lightblue") +
                     ggtitle("Number of Dengue Cases per EWeek") +
                     theme(plot.title = element_text(hjust = 0.5)))
        # geom_bar(stat = "identity") +
        # scale_fill_manual(values = c("yes" = "blue", "no" = "grey" ), guide = FALSE )
    })
    
    output$eweek_kde <- renderPlot({
        req(input$eweek_num)
        eweek_chosen <- dengue %>%
            filter(eweek == input$eweek_num) %>%
            select ("case ID", "longitude", "latitude","date","eweek")
        
        dengue_sf <- st_as_sf(eweek_chosen, coords = c("longitude", "latitude"), crs=4326) %>%
            st_transform(crs=3414)
        dengue_sp <- as(dengue_sf, "Spatial")
        dengue_sp <- as(dengue_sp, "SpatialPoints")
        dengue_ppp <- as(dengue_sp, "ppp")
        
        dengueSG_mpsz = dengue_ppp[mpsz_owin]
        dengueSG_mpsz.km = rescale(dengueSG_mpsz, 1000, "km")
        chosen_kde_dengue_mpsz_250_byeweek <- density(dengueSG_mpsz.km, sigma=0.25, edge=TRUE, kernel="gaussian")
        plot_title <- paste0("Kernel Density Estimation Plot for eweek ",input$eweek_num)
        plot(chosen_kde_dengue_mpsz_250_byeweek,main=plot_title)
    })
    
    
    output$eweek_quad <- renderPrint({
        req(input$eweek_num)
        eweek_chosen <- dengue %>%
            filter(eweek == input$eweek_num) %>%
            select ("case ID", "longitude", "latitude","date","eweek")
        
        dengue_sf <- st_as_sf(eweek_chosen, coords = c("longitude", "latitude"), crs=4326) %>%
            st_transform(crs=3414)
        dengue_sp <- as(dengue_sf, "Spatial")
        dengue_sp <- as(dengue_sp, "SpatialPoints")
        dengue_ppp <- as(dengue_sp, "ppp")
        
        dengueSG_mpsz <- dengue_ppp[mpsz_owin]
        dengueSG_mpsz <- quadrat.test(dengueSG_mpsz, 
                                      nx = 20, ny = 15,
                                      method="M",
                                      nsim=39,
                                      alternative=c("two.sided"))
        dengueSG_mpsz
    })
    
    output$eweek_neighbour <- renderPrint({
        req(input$eweek_num)
        eweek_chosen <- dengue %>%
            filter(eweek == input$eweek_num) %>%
            select ("case ID", "longitude", "latitude","date","eweek")
        
        dengue_sf <- st_as_sf(eweek_chosen, coords = c("longitude", "latitude"), crs=4326) %>%
            st_transform(crs=3414)
        dengue_sp <- as(dengue_sf, "Spatial")
        dengue_sp <- as(dengue_sp, "SpatialPoints")
        dengue_ppp <- as(dengue_sp, "ppp")
        
        dengueSG_mpsz <- dengue_ppp[mpsz_owin]
        dengueSG_mpsz <- clarkevans.test(dengueSG_mpsz,
                                         correction="none",
                                         clipregion="mpsz_owin",
                                         alternative=c("two.sided"),
                                         nsim=39)
        
        dengueSG_mpsz
    })
    
    output$month_bar <- renderPlotly({
        ggplotly(ggplot(dengue_monthly, aes(x=month, y=monthlycases)) +
                     theme_classic() +
                     geom_bar(stat = "identity", color="black" ,fill = "lightblue") +
                     ggtitle("Number of Dengue Cases per Month") +
                     theme(plot.title = element_text(hjust = 0.5)))
    })
    
    output$month_kde <- renderPlot({
        req(input$month_chosen)
        monthly <- dengue %>%
            filter(month == input$month_chosen) %>%
            select ("case ID", "longitude", "latitude","date","month")
        
        dengue_sf <- st_as_sf(monthly, coords = c("longitude", "latitude"), crs=4326) %>%
            st_transform(crs=3414)
        dengue_sp <- as(dengue_sf, "Spatial")
        dengue_sp <- as(dengue_sp, "SpatialPoints")
        dengue_ppp <- as(dengue_sp, "ppp")
        
        dengueSG_mpsz = dengue_ppp[mpsz_owin]
        dengueSG_mpsz.km = rescale(dengueSG_mpsz, 1000, "km")
        chosen_kde_dengue_mpsz_250_bymonth <- density(dengueSG_mpsz.km, sigma=0.25, edge=TRUE, kernel="gaussian")
        plot_title <- paste0("Kernel Density Estimation Plot for ",input$month_chosen)
        plot(chosen_kde_dengue_mpsz_250_bymonth,main=plot_title)
    })
    
    output$month_kde <- renderPlot({
        req(input$month_chosen)
        monthly <- dengue %>%
            filter(month == input$month_chosen) %>%
            select ("case ID", "longitude", "latitude","date","month")
        
        dengue_sf <- st_as_sf(monthly, coords = c("longitude", "latitude"), crs=4326) %>%
            st_transform(crs=3414)
        dengue_sp <- as(dengue_sf, "Spatial")
        dengue_sp <- as(dengue_sp, "SpatialPoints")
        dengue_ppp <- as(dengue_sp, "ppp")
        
        dengueSG_mpsz = dengue_ppp[mpsz_owin]
        dengueSG_mpsz.km = rescale(dengueSG_mpsz, 1000, "km")
        chosen_kde_dengue_mpsz_250_bymonth <- density(dengueSG_mpsz.km, sigma=0.25, edge=TRUE, kernel="gaussian")
        plot_title <- paste0("Kernel Density Estimation Plot for ",input$month_chosen)
        plot(chosen_kde_dengue_mpsz_250_bymonth,main=plot_title)
    })
    output$month_quad <- renderPrint({
        req(input$month_chosen)
        monthly <- dengue %>%
            filter(month == input$month_chosen) %>%
            select ("case ID", "longitude", "latitude","date","month")
        
        dengue_sf <- st_as_sf(monthly, coords = c("longitude", "latitude"), crs=4326) %>%
            st_transform(crs=3414)
        dengue_sp <- as(dengue_sf, "Spatial")
        dengue_sp <- as(dengue_sp, "SpatialPoints")
        dengue_ppp <- as(dengue_sp, "ppp")
        
        dengueSG_mpsz = dengue_ppp[mpsz_owin]
        dengueSG_mpsz <- quadrat.test(dengueSG_mpsz, 
                                      nx = 20, ny = 15,
                                      method="M",
                                      nsim=39,
                                      alternative=c("two.sided"))
        dengueSG_mpsz
    })
    
    output$month_neighbour <- renderPrint({
        req(input$month_chosen)
        monthly <- dengue %>%
            filter(month == input$month_chosen) %>%
            select ("case ID", "longitude", "latitude","date","month")
        
        dengue_sf <- st_as_sf(monthly, coords = c("longitude", "latitude"), crs=4326) %>%
            st_transform(crs=3414)
        dengue_sp <- as(dengue_sf, "Spatial")
        dengue_sp <- as(dengue_sp, "SpatialPoints")
        dengue_ppp <- as(dengue_sp, "ppp")
        
        dengueSG_mpsz = dengue_ppp[mpsz_owin]
        dengueSG_mpsz <- clarkevans.test(dengueSG_mpsz,
                                         correction="none",
                                         clipregion="mpsz_owin",
                                         alternative=c("two.sided"),
                                         nsim=39)
        dengueSG_mpsz
    })
    
    output$subzone_kde <- renderPlot({
        req(input$planning_area_chosen)
        
        dengue_sp <- as(dengue_sf, "Spatial")
        dengue_sp <- as(dengue_sp, "SpatialPoints")
        dengue_ppp <- as(dengue_sp, "ppp")
        
        mpsz <- readOGR(dsn = "data", layer="MP14_SUBZONE_WEB_PL")
        subzone = mpsz[mpsz@data$PLN_AREA_N == input$planning_area_chosen,]
        subzone_sp = as(subzone, "SpatialPolygons")
        subzone_owin = as(subzone_sp, "owin")
        dengue_subzone_ppp = dengue_ppp[subzone_owin]
        dengue_subzone_ppp.km = rescale(dengue_subzone_ppp, 1000, "km")
        chosen_kde_dengue_mpsz_250_bysubzone <- density(dengue_subzone_ppp.km, sigma=0.25, edge=TRUE, kernel="gaussian")
        
        plot_title <- paste0("Kernel Density Estimation Plot for ",input$planning_area_chosen)
        plot(chosen_kde_dengue_mpsz_250_bysubzone,main=plot_title)
    })
    
    #Spatio-temporal EDA--------------------------------------------------------------------------------
    inputRange <- reactiveVal()
    inputEDAPlotType <- reactiveVal()
    inputSpatioTemporalEDAPlanningArea <- reactiveVal()
    numberOfDengueCasesSpatioTemporalEDAVal <- reactiveVal()
    
    .pardefault <- par()

    # Main logic
    observeEvent(input$spatioTemporalApplyButton, {
        inputSpatioTemporalEDAPlanningArea(input$inputSpatioTemporalEDAPlanningArea)
        inputRange(seq(input$inputRange[1], input$inputRange[2], by=1))
        inputEDAPlotType(input$inputEDAPlotType)

        print("Spatio-temporal EDA")
        print(inputSpatioTemporalEDAPlanningArea())
        print(inputRange())
        print(inputEDAPlotType())

    }, ignoreNULL = F)

    output$spatioTemporalEDAPlot <- renderPlot({

        SG_planning_area_sf_filtered <- SG_planning_area_sf %>%
            filter(PLN_AREA_N == inputSpatioTemporalEDAPlanningArea())

        extractCoords <- function(sp.df){
            results <- list()
            for(i in 1:length(sp.df@polygons[[1]]@Polygons))
            {
                results[[i]] <- sp.df@polygons[[1]]@Polygons[[i]]@coords
            }
            results <- Reduce(rbind, results)
            results
        }
        SG_planning_area_spdf <- as(SG_planning_area_sf_filtered, "Spatial")
        SG_planning_area_xy <<- extractCoords(SG_planning_area_spdf)/1000

        # print(inputEDAPlotType())



        dengue_sf_temporal$eweek <- as.numeric(dengue_sf_temporal$eweek)
        dengue_sf_temporal_filtered <- dengue_sf_temporal %>%
            filter(eweek %in% inputRange())
        print(unique(dengue_sf_temporal_filtered$eweek))


        dengue_sf_intersect <- !st_intersects(SG_planning_area_sf_filtered, dengue_sf_temporal_filtered)
        if(length(dengue_sf_intersect[[1]]) > 0){
            dengue_sf_temporal_filtered <- dengue_sf_temporal_filtered[-dengue_sf_intersect[[1]],]
        }


        dengue_sf_temoporal_jitter <- st_jitter(dengue_sf_temporal_filtered)



        dengue_sf_temoporal_jitter <<- dengue_sf_temoporal_jitter
        numberOfSpatialTemporalEDADengueCase <<- nrow(dengue_sf_temoporal_jitter)
        print(numberOfSpatialTemporalEDADengueCase)

        if(length(inputRange()) > 1){
            print("More than 1 eweek selected")
            if(nrow(dengue_sf_temoporal_jitter) <10){
                numberOfDengueCasesSpatioTemporalEDAVal(FALSE)
            }
            else{
                numberOfDengueCasesSpatioTemporalEDAVal(TRUE)
                dengue_xy <- st_coordinates(dengue_sf_temoporal_jitter)
                dengueWeekInfo <- dengue_sf_temoporal_jitter[["eweek"]]
                dengue_sf_full <<- as.3dpoints(dengue_xy[,1]/1000,dengue_xy[,2]/1000,dengueWeekInfo)

                plot(dengue_sf_full, s.region=SG_planning_area_xy, type=inputEDAPlotType())
            }
        }



    })

    output$insufficientPointsSpatioTemporalEDAMessageOutput <- renderText({
        if(length(inputRange())<2){
            return("Please select more than 1 eweek.")
        }
        else{
            if(numberOfDengueCasesSpatioTemporalEDAVal()){
                paste("There are ", numberOfSpatialTemporalEDADengueCase, " dengue cases at ", inputSpatioTemporalEDAPlanningArea(), " in eweeks ", inputRange()[1], " to ", inputRange()[length(inputRange())] ,".", sep="")
            }
            else{
                paste("There are insufficient Dengue Cases for Point Patterns Analysis at ", inputSpatioTemporalEDAPlanningArea(), " in eweeks ", inputRange()[1], " to ", inputRange()[length(inputRange())] ,". Please select other options.", sep="")
            }
        }
    })

    #Spatio-temporal Analysis--------------------------------------------------------------------------------
    inputSpatioTemporalRange <- reactiveVal()
    inputSpatioTemporalAnalysisPlanningArea <- reactiveVal()
    numberOfDengueCasesSpatioTemporalAnalysisVal <- reactiveVal()
    .pardefault <- par()

    # Main logic
    observeEvent(input$spatioTemporalAnalysisApplyButton, {
        inputSpatioTemporalAnalysisPlanningArea(input$inputSpatioTemporalAnalysisPlanningArea)
        inputSpatioTemporalRange(seq(input$inputSpatioTemporalRange[1], input$inputSpatioTemporalRange[2], by=1))

        print("Spatio-temporal Analysis")
        print(inputSpatioTemporalRange())
        print(inputSpatioTemporalAnalysisPlanningArea())

    }, ignoreNULL = F)


    STIK_dengue_infectious_fn <- reactive({
        print("Calculating STIKhat...")


        #--------------------------------------------

        SG_planning_area_sf_filtered <<- SG_planning_area_sf %>%
            filter(PLN_AREA_N == inputSpatioTemporalAnalysisPlanningArea())

        extractCoords <- function(sp.df){
            results <- list()
            for(i in 1:length(sp.df@polygons[[1]]@Polygons))
            {
                results[[i]] <- sp.df@polygons[[1]]@Polygons[[i]]@coords
            }
            results <- Reduce(rbind, results)
            results
        }
        SG_planning_area_spdf <- as(SG_planning_area_sf_filtered, "Spatial")
        SG_planning_area_xy <<- extractCoords(SG_planning_area_spdf)/1000

        print(inputEDAPlotType())

        dengue_sf_temporal$eweek <- as.numeric(dengue_sf_temporal$eweek)
        dengue_sf_temporal_filtered <- dengue_sf_temporal %>%
            filter(eweek %in% inputSpatioTemporalRange())
        print(inputSpatioTemporalRange())

        dengue_sf_intersect <- !st_intersects(SG_planning_area_sf_filtered, dengue_sf_temporal_filtered)
        if(length(dengue_sf_intersect[[1]]) > 0){
            dengue_sf_temporal_filtered <- dengue_sf_temporal_filtered[-dengue_sf_intersect[[1]],]
        }


        dengue_sf_temoporal_jitter <- st_jitter(dengue_sf_temporal_filtered)



        dengue_sf_temoporal_jitter <<- dengue_sf_temoporal_jitter
        numberOfSpatialTemporalAnalysisDengueCase <<- nrow(dengue_sf_temoporal_jitter)
        print(numberOfSpatialTemporalAnalysisDengueCase)
        if(length(inputSpatioTemporalRange()) > 1){
            if(nrow(dengue_sf_temoporal_jitter) <10){
                numberOfDengueCasesSpatioTemporalAnalysisVal(FALSE)
                return(NULL)
            }
            else{
                numberOfDengueCasesSpatioTemporalAnalysisVal(TRUE)
                dengue_xy <- st_coordinates(dengue_sf_temoporal_jitter)
                dengueWeekInfo <- dengue_sf_temoporal_jitter[["eweek"]]
                dengue_sf_full <<- as.3dpoints(dengue_xy[,1]/1000,dengue_xy[,2]/1000,dengueWeekInfo)
                return(STIKhat(dengue_sf_full, correction="isotropic", infectious=TRUE, s.region = SG_planning_area_xy))
            }
        }
    })

    output$insufficientPointsSpatioTemporalAnalysisMessageOutput <- renderText({
        if(length(inputSpatioTemporalRange())<2){
            return("Please select more than 1 eweek.")
        }
        else{
            if(numberOfDengueCasesSpatioTemporalAnalysisVal()){
                paste("There are ", numberOfSpatialTemporalAnalysisDengueCase, " dengue cases at ", inputSpatioTemporalAnalysisPlanningArea(), " in eweeks ", inputSpatioTemporalRange()[1], " to ", inputSpatioTemporalRange()[length(inputSpatioTemporalRange())] ,".", sep="")
            }
            else{
                paste("There are insufficient Dengue Cases for Point Patterns Analysis at ", inputSpatioTemporalAnalysisPlanningArea(), " in eweeks ", inputSpatioTemporalRange()[1], " to ", inputSpatioTemporalRange()[length(inputSpatioTemporalRange())] ,". Please select other options.", sep="")
            }
        }
    })

    # spatioTemporalAnalysisPlot

    output$spatioTemporalAnalysisAreaPlot <- renderPlot({
        if(!is.null(STIK_dengue_infectious_fn())){
            plot(SG_planning_area_sf_filtered$geometry)
            plot(dengue_sf_temoporal_jitter, add=TRUE)
        }
    })

    output$contourPlot <- renderPlot({
        print(!is.null(STIK_dengue_infectious_fn()))
        print("Contour")
        if(!is.null(STIK_dengue_infectious_fn())){
            par(.pardefault)
            stpp::plotK(K = STIK_dengue_infectious_fn(), L = TRUE, type = "contour")
        }
    })

    output$perspPlot <- renderPlot({
        print(!is.null(STIK_dengue_infectious_fn()))
        print("Persp")
        if(!is.null(STIK_dengue_infectious_fn())){
            par(.pardefault)
            stpp::plotK(K=STIK_dengue_infectious_fn(), L=TRUE, type="persp", theta=-45, phi=35)
        }

    })

    output$imagePlot <- renderPlot({
        print(!is.null(STIK_dengue_infectious_fn()))
        print("Image")
        if(!is.null(STIK_dengue_infectious_fn())){
            par(.pardefault)
            stpp::plotK(K = STIK_dengue_infectious_fn(), L = TRUE, type = "image")
        }

    })
    
    output$spatio_text1_1 <- renderUI({"Due to deployment issues, this feature is not deployed. Therefore, please refer to our Project Write up for our Spatial Temporal Exploratory Data Analysis:"})
    output$spatio_text1_2 <- renderUI({tagList(url1)})
    
    output$spatio_text2_1 <- renderUI({"Due to deployment issues, this feature is not deployed. Therefore, please refer to our Project Write up for our Spatial Temporal Analysis:"})
    output$spatio_text2_2 <- renderUI({tagList(url1)})
}


shinyApp(ui, server)

