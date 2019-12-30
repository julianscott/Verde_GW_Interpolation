packages <- c("rgl","stars","gstat","doParallel","SDMTools","sp","raster","rgeos","rgdal","sf","spatstat","spdep","tidyverse","rasterVis",'ggsn')

#  Check to see if each is installed, and install if not.
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {    
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# load the installed libraries in the packages list 
lapply(packages,library,character.only=TRUE)


# get names of csv files in the working directory
csv_names <- list.files(pattern = ".csv",ignore.case=TRUE)
csv_names

vplot <- read_csv("UpperBeasley_ALLVegPLOTS.CSV")
head(vplot)
projr <- CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
vplot_sf <- st_as_sf(vplot,coords = c("Easting","Northing"),crs = projr)
vplot_sp <- as(vplot_sf,"Spatial")
plot(vplot_sf$Elev)
plot(vplot_sf$geometry)
# View(vplot_sf)
# click()

# add on coordinates as columns
vplot_sf <- cbind(vplot_sf,st_coordinates(vplot_sf))
vplot_sf %>%
  arrange(-X) 
vplot_sf <- vplot_sf %>%
  filter(X < 426604)
plot(vplot_sf$geometry)


wells_xy <- read_csv("UpperBeasley_Wells_Stage.csv")
head(wells_xy)
projr <- CRS("+proj=utm +zone=12 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
wells_xy_sf <- st_as_sf(wells_xy,coords = c("Easting","Northing"),crs = projr)
wells_xy_sp <- as(wells_xy_sf,"Spatial")

snsr_dat <- read_csv("BeasUpper_April1-July10_2018_wellstage.csv")
head(snsr_dat)

## format date times
tz7 = "America/Phoenix"
snsr_dat <- snsr_dat %>%
  mutate(DateTime = lubridate::mdy(Date,tz = tz7) + Time,
         Year = lubridate::year(DateTime),
         Month = lubridate::month(DateTime),
         Day = lubridate::day(DateTime)) %>%
  select(DateTime,Date,Time,Year,Month,Day,everything())
head(snsr_dat)
# filter(between(datetime7,ymd_hm("2018-03-11 13:00",tz = tz7),ymd_hm("2018-12-03 23:45",tz = tz7))) %>%

# Get list of river WSE csv files
csv_result_names <- list.files(path = ".\\_solution_directory\\",pattern = ".csv")
csv_result_path <- list.files(path = ".\\_solution_directory\\",pattern = ".csv",full.names = T)

# create table of file names and Q
filename_df <- data.frame(filename = csv_result_names) %>%
  mutate(Q = as.numeric(sub("1BeasleyUS_testResult_","",sub("_PY.csv","",filename))))
filename_df

# define the time of day for which you want a GW surface.
# define the subset of days for which you want these GW surfaces

# for each DateTime,
  # Get GW levels for the DateTime
  # Get river stage for the DateTime using snsr_dat
  # Get river Q for the river stage using rating table or Q column
  # Find river WSE csv that has the absolute closest Q to the defined river Q
  # Interpolate GW surface using river WSE and GW levels

# Define Time for groundwater surface model
GWTime <- "17:00:00"
# format GWTime
GWTime <- hms::as_hms(GWTime)

# Define the subset of days for which you want these GW surface
GW_start_y <- "2018"
GW_end_y <- "2018"

GW_start_m <- "06"
GW_end_m <- "07"

GW_start_Date <- "2017-06-15"
GW_end_Date <- "2018-07-09"

# Select just sensor data in the defined months/period. Optional date restriction
snsr_dat1 <- snsr_dat %>%
  # Optional date restriction
  filter(between(DateTime,lubridate::ymd(GW_start_Date,tz = tz7),
                 lubridate::ymd(GW_end_Date,tz = tz7))) %>%
  # Optional month restriction
  filter(Month >= as.numeric(GW_start_m) & Month <= as.numeric(GW_end_m)) %>%
  # Optional time filer
  filter(Time == GWTime)

# With the current data (as of 11/18/19), the sensor data contains both
# river stage, associated Q, and GW stage for every datetime in the 
# defined period. 
# snsr_dat1$DateTime

# for each DateTime,
  # Find river WSE csv that has the absolute closest Q to the defined river Q
  # Interpolate GW surface using river WSE and GW levels
# i=1

############ Define some functions
# Read each csv into list
# i = csv_result_path[1]
readCSV_fun <- function(list_of_csvs) {
  foreach (i = iter(list_of_csvs),.packages = 'tidyverse') %dopar% {
    df_i <-  tryCatch({read.csv(i,header = T,colClasses = c("numeric"))}, 
                      error = function(e){read.csv(i,skip = 2,header = T)}) # iric
    df_i
  }
}

# Convert each into a spatial object using sf package. 
# Only select points that are inundated
spatial_fun <- function(result_dfs,projr) {
  foreach (i = iter(names(result_dfs)),.packages = c('tidyverse',"sf")) %dopar% {
    # print(i)
    WSE_df = filter(result_dfs[[i]],Depth > 0)
    WSE_sf = st_as_sf(WSE_df,coords = c("X","Y"),crs = projr)
    WSE_sf
  }
}
##########################

#Use Parallel processing to run the functions
system.time({
  UseCores <- detectCores() - 1
  cl <- makeCluster(UseCores)
  registerDoParallel(cl)
  
  result_dfs <- readCSV_fun(list_of_csvs = csv_result_path)
  names(result_dfs) <- csv_result_names
  
  spatial_dfs <- spatial_fun(result_dfs,projr)
  names(spatial_dfs) <- csv_result_names
  
  stopCluster(cl)  
  
})

# define extent for creating GW surface

plot(spatial_dfs$`1BeasleyUS_testResult_00000.5_PY.csv`$geometry)
plot(wells_xy_sf$geometry, add = T)
plot(vplot_sf$geometry, add = T)

plot(wells_xy_sf$geometry, add = T)

# extent of veg plot box
e_veg <- st_bbox(vplot_sf$geometry)
# extent of river WSE box
e_WSE <- st_bbox(spatial_dfs$`1BeasleyUS_testResult_00000.5_PY.csv`$geometry)

# values for modeling extent
# xmin <- min(c(e_veg[["xmin"]],e_WSE[["xmin"]]))
# xmax <- max(c(e_veg[["xmax"]],e_WSE[["xmax"]]))
# ymin <- min(c(e_veg[["ymin"]],e_WSE[["ymin"]]))
# ymax <- max(c(e_veg[["ymax"]],e_WSE[["ymax"]]))

xmin <- e_veg[["xmin"]]-5
xmax <- e_veg[["xmax"]]+5
ymin <- e_veg[["ymin"]]-5
ymax <- e_WSE[["ymax"]]

e <- extent(c(xmin,xmax,ymin,ymax))
# convert raster::extent e into sp object that has a bounding box (bbox)
ep <- as(e,"SpatialPolygons")
crs(ep) <- projr

# Define resolution of GW surface
# res <- c(0.5,0.5)
res <- c(1,1)
r <- raster(x = e,resolution = res,crs = projr)

i = 2
for(i in 1:nrow(snsr_dat1)) {
  # set DateTime i
  DTi = snsr_dat1[i,"DateTime"]
  
  # set Q i
  Qi = snsr_dat1[[i,"US_Qinterp"]]
  
  # get WSEs for each well and PT
  WSEi = select(snsr_dat1[i,],
                c("UpperUS_Stage","UpperDS_Stage","Well.1","Well.1a","Well.2","Well.2b","Well.3","Well.3b")) %>%
    rename(TransBF1US = "UpperUS_Stage",TransBF1DS = "UpperDS_Stage") %>%
    pivot_longer(cols = everything(),names_to = "WellID",values_to = "WaterSurfaceElevation") %>%
    mutate(Well = sub("Well.","",WellID))
  
  # associate with wells_xy_sf
  wells_xy_sf_i <- wells_xy_sf %>%
    left_join(WSEi,by = "Well") %>%
    # Drop River sensors
    filter(!Well %in% c("TransBF1US","TransBF1DS")) %>%
    select(WaterSurfaceElevation) 

  # Identify WSEi
  filei = filename_df[[which(abs(filename_df$Q-Qi)==min(abs(filename_df$Q-Qi))),"filename"]]
  
  # Select WSEi
  # nr = 1:nrow(spatial_dfs[[filei]])
  # filter_rows <- nr[lapply(nr,"%%", 2) == 0]
  # tmp <- spatial_dfs[[filei]] %>%
  #   slice(which(row_number() %% 5 == 1))
  # 
  
  # WSEi_sf = spatial_dfs[[filei]] %>%
  #   select(WaterSurfaceElevation)
  # WSEi_sp <- as(WSEi_sf,"Spatial")
  # Select just the lower edge of the river WSE 
  # Upper right corner is the minimum of I and J cell indices
  # I increases down-valley (columns)
  # J increases cross-valley (rows)
    
  # summary(spatial_dfs[[filei]]$I)
  # summary(spatial_dfs[[filei]]$J)
  
  WSEi2_sf <- spatial_dfs[[filei]]
  # For each unique column I
  SouthernEdge <- data.frame(I = integer(),J = integer())
  for(i in unique(spatial_dfs[[filei]]$I)) {
    # select those rows that are inundated
    set_inund_rows = filter(WSEi2_sf,I == i) %>%
      filter(Depth > 0)
    # Within the set of inundated rows in column I, which is the max row (i.e. max J)?
    maxJ = min(set_inund_rows$J)
    SouthernEdge = rbind(SouthernEdge,data.frame(I = i,J = maxJ))
  }
  
  # filter river WSE for just those points defined in SouthernEdge
  SouthernEdge_WSE_sf <- WSEi2_sf %>%
    right_join(SouthernEdge,by = c("I","J")) %>%
    select(WaterSurfaceElevation)
  #################

  # bind well GWSE and river WSE XY data
  z_bind = rbind(wells_xy_sf_i,SouthernEdge_WSE_sf)
  z_bind_sp <- as(z_bind,"Spatial")
  # change bounding box to include
  z_bind_sp@bbox <- ep@bbox
  
  ### Create Thin Plate Spline model
  xy <- st_coordinates(z_bind)
  v <- z_bind$WaterSurfaceElevation
  tps <- fields::Tps(xy, v,lon.lat = T)
  
  # Use model to predict values at all locations in raster r
  WSE_interp_r <- interpolate(r, tps)

  # calculate root mean square error between modeled and measure GWSEs
  WSE_at_wells<- raster::extract(WSE_interp_r,wells_xy_sf_i,df = T)
  deltas <- data.frame(Well = wells_xy_sf[1:6,]$Well,
                       measured = wells_xy_sf_i$WaterSurfaceElevation,
                       modeled = WSE_at_wells$layer) %>%
    mutate(d = measured - modeled)

}

# Southern Edge of river and well data, veg extent, thin plate spline
model5 <- WSE_interp_r
plot(model5)
plot(SouthernEdge_WSE_sf$geometry,add = T,cex = 0.1,col = "red")
plot(wells_xy_sf_i$geometry,add = T,col = "blue",cex = 0.5)
plot(vplot_sf$geometry,add = T)
delta5 <- deltas
delta5
# RMSE 
delta5 %>% summarise(RMSE = (mean(d^2)^0.5))  
persp(model5)  

Qi
# write some results
writeRaster(model5,"GWSE_Spline_2.27cms.tif")
st_write(SouthernEdge_WSE_sf,"SouthernEdge_WSE_sf.shp")
st_write(wells_xy_sf_i,"wells_xy_sf_i.shp")
st_write(vplot_sf,"vplot_sf.shp")
Qi

