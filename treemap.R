##########################################################################
##  Treemaps
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

agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}
#----------------------------------------
# read data
#----------------------------------------
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
index <- rbind(index_fabio, index_exio)

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
######################################################

#----------------------------------------
# calculate footprints
#----------------------------------------
if(product=="Wood") element <- colSums(L[index_fabio$ISO == country & index_fabio$item %in% items$Item[items$Com.Group=="Wood"], ]) / 1000
if(product!="Wood") element <- L[index_fabio$ISO == country & index_fabio$item == product, ]

data <- data.frame(region = index$ISO,
                   product = index$item,
                   data = rowSums(element * Y)) %>% 
  select(-region) %>% 
  group_by(product) %>% 
  summarize(data = sum(data))
data$group <- items$Group[match(data$product, items$Item)]
data$group[is.na(data$group)] <- "Non-food"
data$group[data$group %in% c("Livestock","Livestock products","Fish")] <- "Food, animal-based"
data$group[data$group %in% c("Primary crops","Crop products")] <- "Food, plant-based"
data$group[data$group=="Wood"] <- "Non-food"
data %>% 
  group_by(group) %>% 
  summarize(data = sum(data))

# treemap_coords <- treemapify::treemapify(data, area="data", label="product", group="group")
# head(treemap_coords)
# treemapify::ggplotify(treemap_coords)

p <- ggplot(data, aes(area = data, fill = group, label = product, subgroup = group)) +
  geom_treemap() +
  geom_treemap_subgroup_border(color = "white") +
  geom_treemap_subgroup_text(place = "centre", alpha = 0.5, colour = "black", fontface = "italic", min.size = 0) +
  geom_treemap_text(reflow = T, place = "topleft", colour = "white") +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "none")

# p

ggsave(filename = paste0("treemap_",country,"_",product,"_",allocation,".png"), 
       plot = p, device = "png", path = "./output", scale = 1, width = 200, height = 150, units = "mm", dpi = 300)

# ggsave(filename = paste0("treemap_",country,"_",product,"_",allocation,".tif"), 
#        plot = p, device = "tiff", path = "./output", scale = 1, width = 200, height = 150, units = "mm", dpi = 300)

