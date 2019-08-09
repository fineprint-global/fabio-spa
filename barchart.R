##########################################################################
##  Bar charts
##########################################################################
# require(devtools)
# install_github("wilkox/treemapify")
# install.packages("treemapify")
library(ggplot2)
library(treemapify)
library(tidyverse)
library(Matrix)

mount_wu_share()
fabiopath <- "/mnt/nfs_fineprint/tmp/fabio/"
exiopath <- "/home/bruckner/wu_share/WU/Projekte/GRU/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/pxp/"

agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x)); return(x) }

#----------------------------------------
# read data
#----------------------------------------
items <- read.csv2("./input/Items.csv", stringsAsFactors = FALSE)
items_exio <- read.csv2("./input/items_exio.csv", stringsAsFactors = FALSE)
regions <- readODS::read_ods("./input/fabio-exiobase.ods", sheet = 3)
regions_all <- read.csv2("./input/Regions.csv")
regions_exio <- read.csv2("./input/Regions_FAO-EXIO.csv", stringsAsFactors = FALSE)
regions_exio <- unique(regions_exio[,c(3,4,5)])
regions_exio$EXIOcode <- as.integer(regions_exio$EXIOcode)
regions_exio <- regions_exio[is.finite(regions_exio$EXIOcode),]
regions_exio <- regions_exio[order(regions_exio$EXIOcode),]
regions_exio$ISO <- as.character(regions$ISO[match(regions_exio$EXIOregion,regions$EXIOBASE)])
regions_exio$ISO[45:49] <- "ROW"
index_fabio <- data.frame(country = rep(regions$Country, each=130),
                          ISO = rep(regions$ISO, each=130),
                          item = rep(items$Item, 192),
                          group = rep(items$Com.Group, 192),
                          model = "fabio")
index_exio <- data.frame(country = rep(regions_exio$EXIOregion, each=200),
                         ISO = rep(regions_exio$ISO, each=200),
                         item = rep(items_exio$Item, 49),
                         group = rep(items_exio$Group, 49),
                         model = "exio")
index <- rbind(index_fabio, index_exio)
index$continent <- regions_all$Continent[match(index$ISO,regions_all$ISO)]


######################################################
# select year
#----------------------------------------
year <- 2013
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
L_link_mass <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/hybrid/",year,"_B_inv_mass.rds"))
L_fabio_mass <- readRDS(paste0(fabiopath,year,"_L_mass.rds"))
L_fabio_mass[L_fabio_mass<0] <- 0
L_mass <- cbind(L_fabio_mass, L_link_mass)
rm(L_fabio_mass, L_link_mass); gc()
L_link_value <- readRDS(paste0("/mnt/nfs_fineprint/tmp/fabio/hybrid/",year,"_B_inv_price.rds"))
L_fabio_value <- readRDS(paste0(fabiopath,year,"_L_price.rds"))
L_fabio_value[L_fabio_value<0] <- 0
L_value <- cbind(L_fabio_value, L_link_value)
rm(L_fabio_value, L_link_value); gc()
#----------------------------------------


######################################################
# define country-product combinations 
#----------------------------------------
product_list <- data.frame(country = character(), product = character(), unit = character(), stringsAsFactors = F)
product_list[1,] <- c("BRA", "Cattle", "(heads per capita)")
product_list[2,] <- c("BRA", "Soyabeans", "(kg per capita)")
product_list[3,] <- c("IDN", "Oil, palm fruit", "(kg per capita)")
product_list[4,] <- c("IDN", "Wood", "(cubic metres per capita)")
product_list[4,] <- c("IDN", "Wood fuel", "(cubic metres per capita)")
product_list[5,] <- c("IDN", "Industrial roundwood, coniferous", "(cubic metres per capita)")
product_list[6,] <- c("IDN", "Industrial roundwood, non-coniferous", "(cubic metres per capita)")
product_list[7,] <- c("IND", "Seed cotton", "(kg per capita)")
product_list[8,] <- c("CHN", "Seed cotton", "(kg per capita)")
product_list[9,] <- c("USA", "Seed cotton", "(kg per capita)")
product_list[10,] <- c("PAK", "Seed cotton", "(kg per capita)")
product_list[11,] <- c("BRA", "Seed cotton", "(kg per capita)")
product_list[12,] <- c("UZB", "Seed cotton", "(kg per capita)")
product_list[13,] <- c("ALL", "Seed cotton", "(kg per capita)")
product_list[14,] <- c("ALL", "Cottonseed", "(kg per capita)")
product_list[15,] <- c("ALL", "Cotton lint", "(kg per capita)")


######################################################
# select product and country (and define cutoff)
#----------------------------------------
select <- 13
percapita <- TRUE
######################################################
country <- product_list$country[select]
product <- product_list$product[select]
unit <- product_list$unit[select]


if(product=="Wood") { 
  element_mass <- colSums(L_mass[index_fabio$ISO == country & index_fabio$item %in% items$Item[items$Com.Group=="Wood"], ]) / 1000
  element_value <- colSums(L_value[index_fabio$ISO == country & index_fabio$item %in% items$Item[items$Com.Group=="Wood"], ]) / 1000
} else if(country=="ALL") {
  element_mass <- colSums(L_mass[index_fabio$item == product, ])
  element_value <- colSums(L_value[index_fabio$item == product, ])
} else {
  element_mass <- L_mass[index_fabio$ISO == country & index_fabio$item == product, ]
  element_value <- L_value[index_fabio$ISO == country & index_fabio$item == product, ]
}


###################################################
# Bar plots
###################################################

data <- rbind(data.frame(region = index$ISO,
                         product = index$item,
                         continent = index$continent,
                         group = index$group,
                         allocation = "mass allocation",
                         value = rowSums(element_mass * Y)),
              data.frame(region = index$ISO,
                         product = index$item,
                         continent = index$continent,
                         group = index$group,
                         allocation = "value allocation",
                         value = rowSums(element_value * Y)))

data$group <- as.character(data$group)

data <- data[data$value>0.5,]
data$group2 <- items$Group[match(data$product, items$Item)]
data$group2[data$group %in% items_exio$Group] <- "Non-food"
data$group2[data$group2 %in% c("Livestock","Livestock products","Fish")] <- "Food" #"Food, animal-based"
data$group2[data$group2 %in% c("Primary crops","Crop products")] <- "Food" #"Food, plant-based"
data$group2 <- as.factor(data$group2)
data$group[data$group %in% c("Hides, skins, wool", "Fibre crops", "Mining", "Agriculture & forestry")] <- "Others"
data$group[data$group %in% c("Animal fats")] <- "Meat"
data$group <- as.factor(data$group)


data <- data %>% 
  # select(-product, -continent) %>% 
  group_by(group2,group,allocation) %>% 
  summarize(value = sum(value))

# data %>% 
#   group_by(group) %>% 
#   summarize(value = sum(value))

# pplot <- ggplot(data, x=continent, y=data, aes(continent, data, fill = group))

forcats::fct_reorder(data$group, data$value)

p <- ggplot(data, aes(x = group2, y = value, fill = forcats::fct_reorder(group, value))) + 
  geom_bar(stat = "identity") + 
  theme_bw() +
  theme(axis.title.x = element_blank()) + 
  labs(y = "Seed cotton (tonnes)", fill = "Products") +
  facet_grid(~ allocation) + 
  viridis::scale_fill_viridis(discrete=T)

p

ggsave(filename = paste0("barchart_",country,"_",product,".png"), 
       plot = p, device = "png", path = "./output", scale = 1, width = 150, height = 100, units = "mm", dpi = 300)


