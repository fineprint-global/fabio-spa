##########################################################################
##  FABIO Structural Downstream Path Analysis
##########################################################################

mount_wu_share()
fabiopath <- "/mnt/nfs_fineprint/tmp/fabio/"
exiopath <- "/home/bruckner/wu_share/WU/Projekte/GRU/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/pxp/"

library(Matrix)
agg <- function(x) { x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x)); return(x) }

# Prepare index ----------------------------------------
countries <- readODS::read_ods("./input/fabio-exiobase.ods", sheet = 3)
countries_exio <- read.csv2("./input/Regions_FAO-EXIO.csv", stringsAsFactors = FALSE)
countries_exio <- unique(countries_exio[,-(1:2)])
countries_exio$EXIOcode <- as.integer(countries_exio$EXIOcode)
countries_exio <- countries_exio[is.finite(countries_exio$EXIOcode),]
countries_exio <- countries_exio[order(countries_exio$EXIOcode),]
items <- read.csv2("./input/Items.csv", stringsAsFactors = FALSE)
items_exio <- read.csv2("./input/items_exio.csv", stringsAsFactors = FALSE)
index <- rbind(data.frame(country = rep(countries$Country, each=130),
                          ISO = rep(countries$ISO, each=130),
                          item = rep(items$Item, 192),
                          model = "fabio"),
               data.frame(country = rep(countries_exio$EXIOregion, each=200),
                          ISO = rep(countries_exio$EXIO2digit, each=200),
                          item = rep(items_exio$Item, 49),
                          model = "exio"))


# --- Non-recursive approach, functions ---

# SPA function for a certain levels between 1 and 5
spa <- function(Lt,D,cutoff,level){
  p <- as.integer(Lt[level])
  if(p==0 | p>nrow(D)) {
    return(NULL)
  } else {
    Lt1 <- Lt$value * D[p,]
    names(Lt1) <- 1:ncol(D)
    rest <- sum(Lt1[Lt1 < cutoff])
    Lt1 <- Lt1[Lt1>=cutoff]
    if(level==1){
      if(length(Lt1>0)){
        results <- data.frame(L0 = Lt[1], L1 = names(Lt1), L2 = NA, L3 = NA, L4 = NA, L5 = NA, value=Lt1, stringsAsFactors = FALSE)
        results[nrow(results)+1,] <- data.frame(Lt[1], 0, NA, NA, NA, NA, rest)
      } else {
        results <- data.frame(L0 = Lt[1], L1 = 0, L2 = NA, L3 = NA, L4 = NA, L5 = NA, value=rest)
      }
    }
    if(level==2){
      if(length(Lt1>0)){
        results <- data.frame(L0 = Lt[1], L1 = Lt[2], L2 = names(Lt1), L3 = NA, L4 = NA, L5 = NA, value=Lt1, stringsAsFactors = FALSE)
        results[nrow(results)+1,] <- data.frame(Lt[1], Lt[2], 0, NA, NA, NA, rest)
      } else {
        results <- data.frame(L0 = Lt[1], L1 = Lt[2], L2 = 0, L3 = NA, L4 = NA, L5 = NA, value=rest)
      }
    }
    if(level==3){
      if(length(Lt1>0)){
        results <- data.frame(L0 = Lt[1], L1 = Lt[2], L2 = Lt[3], L3 = names(Lt1), L4 = NA, L5 = NA, value=Lt1, stringsAsFactors = FALSE)
        results[nrow(results)+1,] <- data.frame(Lt[1], Lt[2], Lt[3], 0, NA, NA, rest)
      } else {
        results <- data.frame(L0 = Lt[1], L1 = Lt[2], L2 = Lt[3], L3 = 0, L4 = NA, L5 = NA, value=rest)
      }
    }
    if(level==4){
      if(length(Lt1>0)){
        results <- data.frame(L0 = Lt[1], L1 = Lt[2], L2 = Lt[3], L3 = Lt[4], L4 = names(Lt1), L5 = NA, value=Lt1, stringsAsFactors = FALSE)
        results[nrow(results)+1,] <- data.frame(Lt[1], Lt[2], Lt[3], Lt[4], 0, NA, rest)
      } else {
        results <- data.frame(L0 = Lt[1], L1 = Lt[2], L2 = Lt[3], L3 = Lt[4], L4 = 0, L5 = NA, value=rest)
      }
    }
    if(level==5){
      if(length(Lt1>0)){
        results <- data.frame(L0 = Lt[1], L1 = Lt[2], L2 = Lt[3], L3 = Lt[4], L4 = Lt[5], L5 = names(Lt1), value=Lt1, stringsAsFactors = FALSE)
        results[nrow(results)+1,] <- data.frame(Lt[1], Lt[2], Lt[3], Lt[4], Lt[5], 0, rest)
      } else {
        results <- data.frame(L0 = Lt[1], L1 = Lt[2], L2 = Lt[3], L3 = Lt[4], L4 = Lt[5], L5 = 0, value=rest)
      }
    }
    return(results)
  }
}

# SPA function for all levels from 0 to 5
spa5 <- function(p,x,D,cutoff){
  # Level 0
  L0 <- data.frame(L0 = p, L1 = NA, L2 = NA, L3 = NA, L4 = NA, L5 = NA, value=x[p])
  # Level 1
  L1 <- spa(L0,D,cutoff*L0$value,level=1)
  # Level 2
  L2 <- data.frame()
  for(i in 1:nrow(L1)){
    print(paste("L2:",i,"/",nrow(L1)))
    L2 <- rbind(L2, spa(L1[i,],D,cutoff*L0$value,level=2))
  }
  # Level 3
  L3 <- data.frame()
  for(i in 1:nrow(L2)){
    print(paste("L3:",i,"/",nrow(L2)))
    L3 <- rbind(L3, spa(L2[i,],D,cutoff*L0$value,level=3))
  }
  # Level 4
  L4 <- data.frame()
  for(i in 1:nrow(L3)){
    print(paste("L4:",i,"/",nrow(L3)))
    L4 <- rbind(L4, spa(L3[i,],D,cutoff*L0$value,level=4))
  }
  # Level 5
  L5 <- data.frame()
  for(i in 1:nrow(L4)){
    print(paste("L5:",i,"/",nrow(L4)))
    L5 <- rbind(L5, spa(L4[i,],D,cutoff*L0$value,level=5))
  }
  return(rbind(L0,L1,L2,L3,L4,L5))
}


######################################################
# select year and allocation
#----------------------------------------
year <- 2013
allocation <- c("mass","price")[2]
######################################################

#----------------------------------------
# load and prepare MRIO
#----------------------------------------
load(paste0(exiopath,year,"_Z.RData"))
Z_exio <- Z
Z_fabio <- readRDS(paste0(fabiopath,year,"_Z_",allocation,".rds"))
Z_fabio[Z_fabio<0] <- 0
Z_link <- readRDS(paste0(fabiopath,"hybrid/",year,"_B.rds"))
Z_link[Z_link<0] <- 0
Z <- rbind(cbind(Z_fabio,Z_link),cbind(matrix(0,nrow(Z_exio),ncol(Z_fabio)),Z_exio))

load(paste0(exiopath,year,"_Y.RData"))
Y_exio <- Y
Y_fabio <- readRDS(paste0(fabiopath,year,"_Y.rds"))
Y_fabio[,grep("OtherUses$", colnames(Y_fabio))] <- 0
temp <- (rowSums(Z_fabio) + rowSums(Y_fabio))
Y_fabio[temp<0, grep("Stock", colnames(Y_fabio))] <- 
  Y_fabio[temp<0, grep("Stock", colnames(Y_fabio))] - temp[temp<0]
colnames(Y_fabio) <- rep(countries$ISO, each=4)
Y_fabio <- agg(Y_fabio)
colnames(Y_exio) <- rep(1:49, each=7)
Y_exio <- agg(Y_exio)
Y_exio <- Y_exio[,countries$EXIOBASE_code]
Y_exio[!is.finite(Y_exio)] <- 0
temp <- Y_exio[,c("45","46","47","48","49")]
Y_exio[,colnames(Y_exio) %in% c(45:49)] <- 0
Y_exio <- cbind(Y_exio, temp)
Y_fabio <- cbind(Y_fabio, matrix(0, nrow(Y_fabio), ncol(temp)))
Y <- rbind(Y_fabio,Y_exio)
rm(Y_fabio,Y_exio,Z_exio,Z_fabio,temp,Z_link); gc()

x <- rowSums(Z) + rowSums(Y)
B <- Z/x
B[! is.finite(B)] <- 0

C <- Y/x
C[! is.finite(C)] <- 0

D <- cbind(B,C)
rm(B); gc()



# define country-product combinations ----------------------------------------

product_list <- data.frame(country = character(), product = character(), stringsAsFactors = F)
product_list[1,] <- c("BRA", "Cattle")
product_list[2,] <- c("BRA", "Soyabeans")
product_list[3,] <- c("IDN", "Oil, palm fruit")
product_list[4,] <- c("IDN", "Wood fuel")
product_list[5,] <- c("IDN", "Industrial roundwood, coniferous")
product_list[6,] <- c("IDN", "Industrial roundwood, non-coniferous")
product_list[7,] <- c("IND", "Seed cotton")
product_list[8,] <- c("CHN", "Seed cotton")
product_list[9,] <- c("USA", "Seed cotton")
product_list[10,] <- c("PAK", "Seed cotton")
product_list[11,] <- c("BRA", "Seed cotton")
product_list[12,] <- c("UZB", "Seed cotton")


######################################################
# select product and country (and define cutoff)
#----------------------------------------
select <- 11
cutoff <- 0.00001
######################################################

for(i in 11:12){
  select <- i
  product <- product_list$product[select]
  country <- product_list$country[select]
  
  p <- which(index$ISO==country & index$item==product)
  
  results <- spa5(p,x,D,cutoff)
  
  results$fd <- results$rest <- results$value
  results$fd[apply(results[,1:6], 1, max, na.rm = T) < nrow(D)] <- 0
  results$rest[apply(results[,1:6], 1, min, na.rm = T) > 0] <- 0
  data.table::fwrite(results, paste0("./output/results_spa_",year,"_",country,"_",product,"_",allocation,".csv"))
  
  sum(results$rest) / (sum(results$fd) + sum(results$rest))
}



