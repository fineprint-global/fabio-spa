##########################################################################
##  Worldmaps
##########################################################################
# install.packages("wbstats")
library(ggplot2)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(Matrix)

mount_wu_share()
fabiopath <- "/mnt/nfs_fineprint/tmp/fabio/"
exiopath <- "/home/bruckner/wu_share/WU/Projekte/GRU/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/pxp/"

agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}
per_capita <- function(countries, data, year = as.integer(format(Sys.Date(), "%Y"))-1){
  pop_data <- wbstats::wb(indicator = "SP.POP.TOTL", startdate = year, enddate = year)
  if(is.null(countries)) {
    data_pc <- data.frame(country = as.character(data$country), 
                          value = as.numeric(data$value))
  } else {
    data_pc <- data.frame(country = as.character(countries), 
                          value = as.numeric(data))
  }
  data_pc$pop <- pop_data$value[match(data_pc$country, pop_data$iso3c)]
  data_pc$value <- data_pc$value / data_pc$pop
  return(data_pc[,1:2])
}
#----------------------------------------
# read data
#----------------------------------------
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  select(admin, adm0_a3_is, continent, region_un, subregion, region_wb, geometry) %>% 
  group_by(admin, adm0_a3_is, continent, region_un, subregion, region_wb) %>% 
  summarise()
# theme_set(theme_bw())
# world <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))
# world$footprint <- rnorm(nrow(world), 100, 60)
# world <- map('world', projection = "gilbert")
# world <- map('world', projection = "mollweide")

items <- read.csv2("./input/Items.csv", stringsAsFactors = FALSE)
items_exio <- read.csv2("./input/items_exio.csv", stringsAsFactors = FALSE)
regions <- readODS::read_ods("./input/fabio-exiobase.ods", sheet = 3)
regions_exio <- read.csv2("./input/Regions_FAO-EXIO.csv", stringsAsFactors = FALSE)
regions_exio <- unique(regions_exio[,-(1:2)])
regions_exio$EXIOcode <- as.integer(regions_exio$EXIOcode)
regions_exio <- regions_exio[is.finite(regions_exio$EXIOcode),]
regions_exio <- regions_exio[order(regions_exio$EXIOcode),]
index_fabio <- data.frame(country = rep(regions$Country, each=130),
                          ISO = rep(regions$ISO, each=130),
                          item = rep(items$Item, 192),
                          model = "fabio")
index_exio <- data.frame(country = rep(regions_exio$EXIOregion, each=200),
                         ISO = rep(regions_exio$EXIO2digit, each=200),
                         item = rep(items_exio$Item, 49),
                         model = "exio")

######################################################
# select year and allocation
#----------------------------------------
year <- 2013
allocation <- c("mass","price")[1]
######################################################

#----------------------------------------
# load and merge Y_fabio and Y_exio
#----------------------------------------
Y_fabio <- readRDS(paste0(fabiopath,year,"_Y.rds"))
Y_fabio <- Y_fabio[,grep("Food$", colnames(Y_fabio))]
colnames(Y_fabio) <- regions$ISO
load(paste0("/mnt/nfs_fineprint/tmp/exiobase/pxp/",year,"_Y.RData"))
colnames(Y) <- rep(1:49, each = 7)
Y_exio <- agg(Y)
Y_exio <- Y_exio[,regions$EXIOBASE_code]
Y_exio[!is.finite(Y_exio)] <- 0
temp <- Y_exio[,c("45","46","47","48","49")]
Y_exio[,colnames(Y_exio) %in% c(45:49)] <- 0
Y_exio <- cbind(Y_exio, temp)
Y <- cbind(Y_fabio, matrix(0, nrow(Y_fabio), ncol(temp)))
Y <- rbind(Y, Y_exio)
colnames(Y)[193:197] <- regions_exio$EXIO2digit[45:49]
#----------------------------------------
# load and merge L_link and L_fabio
#----------------------------------------
L_link <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/hybrid/",year,"_B_inv_",allocation,".rds"))
L_fabio <- readRDS(paste0(fabiopath,year,"_L_",allocation,".rds"))
L_fabio[L_fabio<0] <- 0
L <- cbind(L_fabio, L_link)
rm(L_fabio, L_link); gc()
#----------------------------------------


######################################################
# select country & product 
#----------------------------------------
country <- c("BRA", "IDN")[1]
product <- c("Cattle", "Soyabeans", "Oil, palm fruit", "Wood")[1]
unit <- c("(kg per capita)", "(heads per capita)", "(cubic metres per capita)")[if_else(product=="Cattle", 2, if_else(product=="Wood", 3, 1))]
percapita <- TRUE
######################################################

#----------------------------------------
# calculate footprints
#----------------------------------------
if(product=="Wood") element <- colSums(L[index_fabio$ISO == country & index_fabio$item %in% items$Item[items$Com.Group=="Wood"], ]) / 1000
if(product!="Wood") element <- L[index_fabio$ISO == country & index_fabio$item == product, ]

data <- data.frame(region = colnames(Y),
                   data = colSums(element * Y))
if(percapita) {
  data <- per_capita(data$region, data$data * 1000, year)
  world$footprint <- data$value[match(world$adm0_a3_is, data$country)]
  p <- ggplot(data = world) +
    geom_sf(aes(fill = footprint), size = 0.05) +
    labs(fill=paste0(country," ",product,"\n", unit)) + 
    scale_fill_viridis_c(direction = -1, na.value = "lightgrey") + 
    theme(rect = element_blank()) + 
    coord_sf(crs = "+proj=robin")
  # coord_sf(crs = "+proj=moll")
  # coord_sf(crs = "+proj=wintri")
} else {
  # share of ROW which is lost (i.e. not plotted)
  sum(data$data[192:197] / sum(data$data))
  world$footprint <- data$data[match(world$adm0_a3_is, data$region)] / sum(data$data)
  p <- ggplot(data = world) +
    geom_sf(aes(fill = footprint), size = 0.05) +
    labs(fill=paste0("Share of ",country," ",product)) + 
    scale_fill_viridis_c(direction = -1, na.value = "lightgrey", labels = scales::percent) + 
    theme(rect = element_blank()) + 
    coord_sf(crs = "+proj=robin")
  # coord_sf(crs = "+proj=moll")
  # coord_sf(crs = "+proj=wintri")
}

p

ggsave(filename = paste0("map_",country,"_",product,"_",if_else(percapita,"percapita_",""),allocation,".tif"), 
       plot = p, device = "tiff", path = "./output", scale = 1, width = 207, height = 90, units = "mm", dpi = 300)

