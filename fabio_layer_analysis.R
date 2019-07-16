##########################################################################
##  WIOD Layer Analysis
##########################################################################

rm(list=ls())
datapath <- "W:/WU/Projekte/SRU-Projekte/04_Daten/MRIO/IO data/WIOD/WIOD_Nov16/"
resultspath <- "./output/"

library(openxlsx)
library(reshape2)
library(data.table)
library(easyNCDF)
library(ncdf4)

class.prod <- read.xlsx(xlsxFile = "./input/MRIO_classifications.xlsx", sheet = 12, startRow = 2)
class.reg <- read.xlsx(xlsxFile = "./input/MRIO_classifications.xlsx", sheet = 6, startRow = 2)
class.rows <- read.xlsx(xlsxFile = "./input/WIOD classification.xlsx", sheet = 1)
class.cols <- read.xlsx(xlsxFile = "./input/WIOD classification.xlsx", sheet = 2)
SEA <- read.xlsx(xlsxFile = "./input/WIOD_SEA_Nov16.xlsx", sheet = 2)
EXR <- read.xlsx(xlsxFile = "./input/EXR_WIOD_Nov16.xlsx", sheet = 2, startRow = 4)

# select the needed variables from the socioeconomic accounts
EMPE <- SEA[SEA$variable=="EMPE", ]
VA <- SEA[SEA$variable=="VA", ]
# convert VA into current US$
for(year in 2000:2014){
  VA[, as.character(year)] <- VA[, as.character(year)] * EXR[ match(VA$country,EXR$Acronym), as.character(year)]
}

year <- 2014
for(year in 2000:2014){
  print(year)
  load(paste0(datapath,"WIOT",year,"_October16_ROW.RData"))
  Z <- wiot[1:2464,6:(2464+5)]
  Y <- wiot[1:2464,(2465+5):2689]
  x <- rowSums(wiot[1:2464,6:2689])
  
  colnames(Y) <- substr(colnames(Y), 1, 3)
  Y <- as.matrix(Y) %*% sapply(unique(colnames(Y)),"==",colnames(Y))
  
  I <- diag(nrow(Z))
  A <- t(t(Z)/x)
  A[! is.finite(A)] <- 0
  
  # calculate layer contributions to a specific final demand
  # reg <- 2  # reg 2 = AUT
  # Yreg <- diag(rowSums(Y[,((reg-1)*5+1):(reg*5)]))
  # Yglobal <- diag(rowSums(Y))  # for the entire global economy
  
  ################################
  # without extension
  ################################
  # L0 <- I %*% Yglobal
  # L1 <- A %*% Yglobal
  results <- data.frame(L0 = 1:nrow(Y), L1 = 0, L2 = 0, L3 = 0, value = rowSums(Y))
  
  L1 <- t(t(A) * rowSums(Y))
  dimnames(L1) <- list(1:nrow(L1), 1:ncol(L1))
  
  temp <- melt(L1)
  colnames(temp) <- c("L1", "L0", "value")
  temp$L3 <- temp$L2 <- 0
  temp <- temp[,c(2,1,4,5,3)]
  results <- rbind(results, temp)
  
  get_L2 <- function(aL0, aL1, A, L1, cutoff){
    temp <- A[aL1,aL0] * L1[aL1,]
    temp <- data.frame(L0 = aL0, L1 = aL1, L2 = 1:length(temp), L3 = 0, value = temp)
    return(temp[abs(temp$value) > cutoff, ])
  }
  
  storage <- list()
  # aL0 <- aL1 <- 2
  for(aL0 in 1:ncol(A)){
    print(paste("sector",aL0))
    for(aL1 in 1:nrow(A)){
      storage[[aL0+nrow(A)*(aL1-1)]] <- get_L2(aL0, aL1, A, L1, 0.0001)
    }
  }
  results <- rbind(results, rbindlist(storage, use.names=TRUE))
  
  results <- results[!results$value<=0,]
  
  fwrite(results, file=paste0("./output/results_",year,".csv"), row.names=FALSE)
  
  
  ################################
  # with EMPE extension
  ################################
  ext_empe <- c(as.numeric(EMPE[, as.character(year)]),rep(0,56)) / x
  ext_empe[!is.finite(ext_empe)] <- 0
  
  I_empe <- ext_empe * I
  A_empe <- ext_empe * A
  
  L <- list()
  for(i in 1:length(A_empe)){
    print(paste("layer",i))
    L[[i]] <- A_empe[[i]] %*% Yreg
    rownames(L[[i]]) <- 1:2464
    colnames(L[[i]]) <- 1:2464
  }
  
  results <- data.frame(from=integer(),to=integer(),value=numeric(),year=integer(),level=integer())
  # i = 1
  for(i in 1:length(L)){
    print(paste("layer",i))
    temp <- melt(L[[i]])
    temp <- cbind(temp,year,i)
    results <- rbind(results, temp)
  }
  colnames(results) <- c("from","to","value","year","level")
  results <- results[!results$value<=0,]
  results$year <- NULL
  results$variable <- "EMPE"
  # save(results,file=paste0("./output/results_EMPE_",year,".RData"))
  # write.csv(results, file=paste0("./output/results_EMPE_",year,".csv"), row.names=FALSE)
  fwrite(results, file=paste0("./output/results_EMPE_",year,".csv"), row.names=FALSE)
}
