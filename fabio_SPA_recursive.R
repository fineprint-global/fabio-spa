##########################################################################
##  FABIO Structural Downstream Path Analysis
##########################################################################

mount_wu_share()
fabiopath <- "/mnt/nfs_fineprint/tmp/fabio/"
exiopath <- "/home/bruckner/wu_share/WU/Projekte/GRU/04_Daten/MRIO/IO data/EXIOBASE/EXIOBASE 3.6/parsed/pxp/"

library(Matrix)

countries <- readODS::read_ods("./input/fabio-exiobase.ods", sheet = 3)
# countries_exio <- read.csv2("./input/Regions_FAO-EXIO.csv", stringsAsFactors = FALSE)
items <- read.csv2("./input/Items.csv", stringsAsFactors = FALSE)
index <- rbind(data.frame(country = rep(countries$ISO, each=130),
                          item = rep(items$Item, 192),
                          model = "fabio"),
               data.frame(country = as.character(rep(1:49, each=200)),
                          item = as.character(rep(1:200, 49)),
                          model = "exio"))

agg <- function(x)
{
  x <- as.matrix(x) %*% sapply(unique(colnames(x)),"==",colnames(x))
  return(x)
}
get_L2 <- function(fname, aL0, aL1, A, L1, cutoff){
  if(A[aL1,aL0] < cutoff) return(0)
  temp <- (extension * A)[,aL1] * A[aL1,aL0] * Y[aL0,country]
  temp <- data.frame(country = unique(class.col$Country)[country], L0 = aL0, L1 = aL1, L2 = 1:length(temp), L3 = 0, value = temp)
  try(fwrite(temp[abs(temp$value) > cutoff, ], file = fname, row.names = FALSE, col.names = FALSE, append = TRUE))
}


# --- Recursive approach, functions ---

# Recursively split alpha (x[p]), given a split defined by D[p, ].
# Calculate for values >= epsilon and go through lvl_cap layers.
get_path <- function(alpha, D, p, epsilon, lvl_cap = 3, lvl = 0) {
  
  if(p > nrow(D)) {return(list(lvl = lvl, split = c("final" = alpha)))}
  
  tmp <- alpha * D[p, ]
  
  out <- tmp[tmp >= epsilon]
  rest <- sum(tmp[tmp < epsilon])
  
  if(lvl >= lvl_cap) {return(list(lvl = lvl, split = c(out, "rest" = rest)))}
  
  lvl <- lvl + 1
  
  return(
    list(
      lvl = lvl - 1,
      split = c(out, "rest" = rest),
      down = lapply(structure(names(out), names = names(out)), 
                    function(x, D, out, epsilon, lvl_cap, lvl) {
                      get_path(out[[x]], D = D,
                               p = as.integer(substr(x, 4, nchar(x))), 
                               epsilon = epsilon, lvl_cap = lvl_cap, lvl = lvl)
                    }, D, out, epsilon, lvl_cap, lvl))
  )
}

# Transform a tree-list into a data.table with level, id-path and value
library(data.table)
df_ify <- function(x, prev = "") {
  
  if(is.null(x$down)) {
    data.table("lvl" = x[[1]], 
               id = paste0(prev, names(x[[2]])),
               val = x[[2]])
  } else {
    rbindlist(
      list(
        data.table("lvl" = x[[1]],
                   id = paste0(prev, names(x[[2]])),
                   val = x[[2]]),
        rbindlist(
          lapply(names(x[["down"]]), function(name, x, prev) {
            df_ify(x[["down"]][[name]], prev = paste0(name, " > ", prev))
          }, x, prev)
        )
      )
    )
  }
}


#----------------------------------------
# load and prepare MRIO
#----------------------------------------
year <- 2013
load(paste0(exiopath,year,"_Z.RData"))
Z_exio <- Z
Z_fabio <- readRDS(paste0(fabiopath,year,"_Z_mass.rds"))
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


#----------------------------------------
# choose product and country
#----------------------------------------
country <- "BRA"
country <- "IDN"
product <- "Cattle"
product <- "Soyabeans"
product <- "Oil, palm fruit"
product <- "Wood fuel"
product <- "Industrial roundwood, coniferous"
product <- "Industrial roundwood, non-coniferous"
cutoff <- 0.001
#----------------------------------------

p <- which(index$country==country & index$item==product)

colnames(D) <- paste0("col", 1:ncol(D))
rownames(D) <- NULL
alpha <- x[[p]]

# --- Recursive approach ---

# Recursively split alpha (x[p]), given a split defined by D[p, ].
# Calculate for values >= epsilon and go through lvl_cap layers.
tree <- get_path(alpha, D, p, epsilon = cutoff * alpha, lvl_cap = 5)

# Transform a tree-list into a data.table with level, id-path and value
df <- df_ify(tree)

# Transform df to include levels in separate columns
split <- strsplit(df$id, " > ")
for(i in 0:lvl_cap) {
  df[[paste0("l_", i)]] <- sapply(split, function(x, i) x[i], i = i + 1)
}
df$id <- NULL
# df

data.table::fwrite(df, paste0("./output/results_spa_",year,"_",country,"_",product,"_recursive.csv"))
