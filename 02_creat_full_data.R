
# working directory
PATH = dirname(rstudioapi::getSourceEditorContext()$path)  #加一个dirname可以返回再上一级目录
setwd(PATH)

# ====================================================================================================
# ======================= BUILDS OBJECTS FOR FITTING AND EVALUATING MODELS ===========================
# ====================================================================================================

# This script is called as source in a modelling script where various fields can be specified

library(dplyr); library(raster); library(rgdal); library(sf);library(sp);library(Matrix)
library(stringr); library(ggplot2); library(lubridate)
library(magrittr); library(INLA); library(spdep);library(rgeos)
source("00_plot_themes.R")
source("00_inla_setup_functions_r4.R")

## fields to specify
# project name
# number of bins to group climatic data
# region to subset to (N/S/C)
# plot_graph: plot neighbourhood matrix for inla model?
# province_case_threshold: only keep provinces with threshold of >n cases
# region2: subregion to subset to

if(!exists("projname")){
  projname = "temp"
}

if(!exists("n_clim_bins")){
  n_clim_bins = 40
}

if(!exists("region")){
  region = "all"
}


if(!exists("plot_graph")){
  plot_graph = TRUE
}



# ============== Set up project file name and output locations ==============

# specify project name: all outputs will be saved in a directory of this name

# create folder structure for saving outputs
save_dir = paste(c("./output/model_outputs/", projname, "/"), collapse="")
if(!dir.exists(save_dir)){ 
  dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paste(save_dir, "model_output/", sep=""), recursive = TRUE, showWarnings = FALSE) 
  dir.create(paste(save_dir, "errors/", sep=""), recursive = TRUE, showWarnings = FALSE) 
  dir.create(paste(save_dir, "models/", sep=""), recursive = TRUE, showWarnings = FALSE) 
  dir.create(paste(save_dir, "fitmetrics/", sep=""), recursive = TRUE, showWarnings = FALSE) 
}


# ================= Build dengue dataset =================

# districts to be excluded (offshore) 
offshore_areas = c(330900)

# shapefiles: district
shp <- st_read("./Data/shapefiles/县.shp") %>%
  filter(省代码 %in% c(330000)) %>%  ##筛选出浙江省的数据
  dplyr::filter(!市代码 %in% offshore_areas) %>%
  rename(province = 省,  
         provinceid = 省代码,
         city = 市,
         cityid = 市代码,
         county = 县,
         countyid = 县代码
  ) 

#names(shp)
shp <- shp[,c(1,2,3,4,5,6,13)] 



shp_prov = st_read("./Data/shapefiles/省.shp") %>%
  filter(省代码 %in% c(330000)) 

shp_prov <- shp_prov[ ,c(1,2,4)] 

st_crs(shp_prov) = st_crs(shp)  #将shp_prov的坐标参考系统（CRS）设置为与shp相同的CRS,确保两个数据集具有相同的坐标系统
shp_prov = st_crop(shp_prov, shp) #对shp_prov进行裁剪，使其与shp相交的部分保留


shp_city = st_read("./Data/shapefiles/市.shp") %>%
  filter(省代码 %in% c(330000)) %>%
  dplyr::filter(!市代码 %in% offshore_areas) %>%
  rename(province = 省,  
         provinceid = 省代码,
         city = 市,
         cityid = 市代码) 
shp_city <- shp_city[,c(1,2,3,4,7)]
st_crs(shp_city) = st_crs(shp)  
shp_city = st_crop(shp_city, shp)

MeteorologydataLag <- read.csv("./output/data_process/Meteorologydata_Lags.csv",fileEncoding = "GB18030")%>%
  dplyr::mutate(date = as.Date(date))

landcover <- read.csv("./output/data_process/landcover.csv",fileEncoding = "GB18030")

DEM <- read.csv("./output/data_process/DEM.csv",fileEncoding = "GB18030")

migration <- read.csv("./output/data_process/migration.csv",fileEncoding = "GB18030")

dd <- read.csv("./output/data_process/sfts_caseALL.csv",fileEncoding = "GB18030") %>%
  dplyr::filter(!cityid %in% offshore_areas) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::left_join(landcover, by=c("city","cityid","county","countyid","year")) 

dd <- left_join(dd,DEM, by=c("city","cityid","county","countyid"))

dd <- left_join(dd,migration[,c("city","year","month","in_migration","out_migration")], by=c("city","year","month"))

dd <- left_join(dd, MeteorologydataLag, by=c("date","city","cityid")) 

# subset shapefile to only locations within example dataset
shp = shp %>% dplyr::filter(countyid %in% dd$countyid)

sum(dd$total_cases)
sum(dd$total_deaths)
# =============== group predictors for fitting nonlinear effects ==================


# group predictors for fitting nonlinear effects 
# do this before subsetting to geographical subregions to ensure consistency for prediction/projection

print("Grouping climate predictors")

nbins = n_clim_bins
dx = dd[ , grep("Tmean|Tmin|Tmax|Precip|Wind|Rh|Sun", names(dd))]
names(dx) = paste(names(dx), "_g", sep="")

groupCols = function(x){
  x = dx[ , x, drop=FALSE ]
  x[ , 1] = inla.group(x[ , 1], n=nbins)
  x
}
dx = do.call(cbind.data.frame, lapply(1:ncol(dx), groupCols))
dd = cbind(dd, dx)






# ================ subset to specified region(s), if specified =================
# 生成region变量
dd$region1 <- ifelse(dd$cityid %in% c(330500, 330100, 330400), "1",
                     ifelse(dd$cityid %in% c(330800, 331100, 330300), "2",
                            ifelse(dd$cityid %in% c(330200, 330600, 331000, 330700), "3", NA)))

if(region != "all"){
  if(!region %in% unique(dd$region1)){
    print("Region not recognised: defaulting to 'all'")
  }
  else{
    dd = dd[ dd$region1 %in% c(region), ]
    shp = shp[ shp$countyid %in% dd$countyid, ]
    shp_city = shp_city[ shp_city$cityid %in% dd$cityid, ]
  }
}




# ================ set up spatial neighbourhood matrix for CAR model ===================

# subset shapefile and sfts cases to focal district
shpf = shp[ shp$countyid %in% dd$countyid, ]

# create neighbourhood matrix for CAR spatial
# firstly create lookup refs for polygon ids, create neighbourhood matrix, then add lookups into dataframe
id_ref = data.frame(countyid = shpf$countyid, polyid = 1:nrow(shpf))
district.nb = spdep::poly2nb(sf::as_Spatial(shpf), row.names=id_ref$polyid)  #poly2nb函数生成邻接矩阵
dd = left_join(dd, id_ref)
class(district.nb)
district.nb

# save neighbourhood matrix with focal district (if not already existing)
#region <- "excluZS"
region
nbmatrix_name = paste(save_dir, "adjmatrix_", region, "_",  paste(tolower(projname), collapse="_"), sep="") 
nb2INLA(nbmatrix_name, district.nb)
nbmatrix_name

# plot neighbourhood matrix if specified
if(plot_graph){
  xy = as.data.frame(rgeos::gCentroid(as_Spatial(shpf), byid=TRUE))
  plot(shpf$geometry)
  plot(district.nb, coords = cbind(xy$x, xy$y), col="red", add=T, cex=0.5)
}



########################## city level
district.nb2 = spdep::poly2nb(sf::as_Spatial(shpcd), row.names=id_ref_m$polyid_city)  #poly2nb函数生成邻接矩阵

# save neighbourhood matrix with focal district (if not already existing)
region <- "excluZS"
nbmatrix_name2 = paste(save_dir, "adjmatrix_city_", region, "_",  paste(tolower(projname), collapse="_"), sep="") 
nb2INLA(nbmatrix_name2, district.nb2)

# plot neighbourhood matrix if specified
if(plot_graph){
  xy = as.data.frame(rgeos::gCentroid(as_Spatial(shpcd), byid=TRUE))
  plot(shpcd$geometry)
  plot(district.nb2, coords = cbind(xy$x, xy$y), col="red", add=T, cex=0.5)
}






# =============== setup covariates, transform, scale and set grouping factors ====================

# defining offset as log population (in hundreds of thousands)
# so model is estimating incidence per 10,000,000 inhabitants
# along with defining fairly tight priors on intercept precision, this deals with numerical challenges of fitting when estimating incidence per inhabitant
dd$logpop = log(dd$pop/1000)

# replicate variables for grouping
areaidx = factor(dd$countyid, levels=unique(dd$countyid)[ order(unique(dd$countyid)) ], ordered=TRUE)
dd$areaidx = as.integer(areaidx)
dd$areaidy = as.integer(areaidx)
dd$yearx = as.integer(as.factor(dd$year))
dd$cityx = as.integer(factor(dd$city, levels=unique(dd$city)[ order(unique(dd$city)) ], ordered=TRUE))
dd$cityy = dd$cityx

#standardized and weighted the migration
dd <- dd %>%
  group_by(city) %>%  # 按city分组
  mutate(
    out_migration_norm = (out_migration - min(out_migration, na.rm = TRUE)) / (max(out_migration, na.rm = TRUE) - min(out_migration, na.rm = TRUE)),
    out_migration_norm = pmax(pmin(out_migration_norm, 1), 0)
  ) %>%
  ungroup()  # 取消分组

dd <- dd %>%
  group_by(city) %>%  # 按city分组
  mutate(
    out_migration_norm2 = (out_migration - min(out_migration, na.rm = TRUE)) / (max(out_migration, na.rm = TRUE) - min(out_migration, na.rm = TRUE)),
    out_migration_norm2 = pmax(pmin(out_migration_norm2, 1), 0),
    in_migration_norm = (in_migration - min(in_migration, na.rm = TRUE)) / (max(in_migration, na.rm = TRUE) - min(in_migration, na.rm = TRUE)),
    in_migration_norm = pmax(pmin(in_migration_norm, 1), 0)
  ) %>%
  ungroup()  # 取消分组



#write.csv(dd, file = "./output/data_process/ModelData_sfts_ALL_withLags.csv",fileEncoding = "GB18030")

