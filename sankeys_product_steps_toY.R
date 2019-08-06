library(tidyverse)
# library(RColorBrewer)

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
product_list[13,] <- c("ALL", "Seed cotton")


######################################################
# select product and country (and define cutoff)
#----------------------------------------
select <- 13
# set the precision used to cut off flows to either [rest] or RoW 
# depending on if the flow goes to final demand (then RoW) or not ([rest])
precision <- 1*1e-2 # 0.5% precision
allocation <- c("mass","price")[2]
######################################################

product <- product_list$product[select]
country <- product_list$country[select]
rest_name <- "[rest]"

# get data  --------------------------------------------------------------------
if(product == "Wood"){
  data_1 <- read.csv(paste0("./output/results_spa_2013_", country, "_Industrial roundwood, coniferous.csv"))
  data_2 <- read.csv(paste0("./output/results_spa_2013_", country, "_Industrial roundwood, non-coniferous.csv"))
  data_3 <- read.csv(paste0("./output/results_spa_2013_", country, "_Wood fuel.csv"))
  data <- rbind(data_1, data_2, data_3)
  rm(data_1, data_2, data_3)
} else if(product == "Seed cotton" & country == "ALL"){
  data_1 <- read.csv(paste0("./output/results_spa_2013_IND_", product, "_", allocation, ".csv"))
  data_2 <- read.csv(paste0("./output/results_spa_2013_CHN_", product, "_", allocation, ".csv"))
  data_3 <- read.csv(paste0("./output/results_spa_2013_USA_", product, "_", allocation, ".csv"))
  data_4 <- read.csv(paste0("./output/results_spa_2013_PAK_", product, "_", allocation, ".csv"))
  data_5 <- read.csv(paste0("./output/results_spa_2013_BRA_", product, "_", allocation, ".csv"))
  data_6 <- read.csv(paste0("./output/results_spa_2013_UZB_", product, "_", allocation, ".csv"))
  data <- rbind(data_1, data_2, data_3, data_4, data_5, data_6)
  rm(data_1, data_2, data_3, data_4, data_5, data_6)
} else if (product == "Seed cotton" & country != "ALL"){
  data <- read.csv(paste0("./output/results_spa_2013_", country, "_", product, "_", allocation, ".csv"))
} else {
  data <- read.csv(paste0("./output/results_spa_2013_", country, "_", product ,".csv"))
} 
sum(data$rest) / (sum(data$fd) + sum(data$rest))


# Prepare index ----------------------------------------

# gives the meaning of the codes used in data
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
                          model = "exio"),
               data.frame(country = countries$Country,
                          ISO = countries$ISO,
                          item = "",
                          model = "fabio"),
               data.frame(country = countries_exio$EXIOregion[45:49],
                          ISO = countries_exio$EXIO2digit[45:49],
                          item = "",
                          model = "exio"))
index$X <- 1:nrow(index)


# prepare data  ----------------------------------------------------------------

# merge all Wood for Indonesia to one single starting point
if(product == "Wood"){
  L0 <- min(unique(data$L0))
  
  levels(index$item) <- c(levels(index$item), "Wood")
  
  index[L0,]$item <- "Wood"
  
  data$L0 <- L0
}

# create unique IDs for each step to also represent the steps before
# to make the path traceable
data <- data %>% 
  dplyr::mutate(
    L5_id = ifelse(!is.na(L5), paste(L0, L1, L2, L3, L4, L5, sep = "_"), NA),
    L4_id = ifelse(!is.na(L4), paste(L0, L1, L2, L3, L4, sep = "_"), NA),
    L3_id = ifelse(!is.na(L3), paste(L0, L1, L2, L3, sep = "_"), NA),
    L2_id = ifelse(!is.na(L2), paste(L0, L1, L2, sep = "_"), NA),
    L1_id = ifelse(!is.na(L1), paste(L0, L1, sep = "_"), NA)
  )

# now we define the step, which is later very useful for every operation
data <- data %>% 
  dplyr::mutate(
    step = ifelse(is.na(L1), 0, # we dont really need step 0 for anything
                  ifelse(is.na(L2), 1,
                         ifelse(is.na(L3), 2,
                                ifelse(is.na(L4), 3, 
                                       ifelse(is.na(L5), 4, 5)))))
  )

# data <- data[data$step != 5,]

# total footprint(s)
total_sum <- sum(data[data$step == 0,]$value)

# remove first node as we dont need it for visualisation
data <- data[data$step != 0,]

# define source and target for the link
data <- data %>%
  dplyr::mutate(
    source = dplyr::recode(step,
                            as.character(L0), # as.char is needed bc all others are chars
                            L1_id,
                            L2_id,
                            L3_id,
                            L4_id
                           ),
    target = dplyr::recode(step, # we put the as.character here for everything because
                            # if the whole column is NA, it's of class "logical", not a char
                            if_else(L1 == 0L, as.character("0"), as.character(L1_id)),
                            if_else(L2 == 0L, as.character("0"), as.character(L2_id)),
                            if_else(L3 == 0L, as.character("0"), as.character(L3_id)),
                            if_else(L4 == 0L, as.character("0"), as.character(L4_id)),
                            if_else(L5 == 0L, as.character("0"), as.character(L5_id))
                           )
  )

# define source and target for the link
data <- data %>% 
  dplyr::mutate(
    source = dplyr::recode(step,
                           L0,
                           L1,
                           L2,
                           L3,
                           L4
    ),
    target = dplyr::recode(step,
                           L1,
                           L2,
                           L3,
                           L4,
                           L5
    )
  ) %>% 
  dplyr::mutate(
    code_src = if_else(step == 1,
                       sprintf("%s (%s)", index$item[match(source, index$X)], index$ISO[match(source, index$X)]), 
                       as.character(index$item[match(source, index$X)])),
    code_tgt = if_else(target == "0", 
                       rest_name, 
                       as.character(index$item[match(target, index$X)])),
    model = index$model[match(source, index$X)]
  )

# now we filter all final demand rows that go there before step 5 (that we already removed)
data$code_tgt[data$code_tgt == ""] <- "Final Demand"
# everything that is not in final demand in the last step -> put into rest
data$code_tgt[data$step == 5 & data$code_tgt != "Final Demand"] <- rest_name

data <- data %>% 
  dplyr::group_by(step, code_src, code_tgt, model) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
  dplyr::ungroup() # ungroup because we modify later

data <- data %>% 
  dplyr::mutate(code_src_unique = sprintf("%s #%.0f", code_src, step),
                code_tgt_unique = ifelse(code_tgt == rest_name, 
                                         rest_name, 
                                         ifelse(code_tgt == "Final Demand",
                                                "Final Demand",
                                                sprintf("%s #%.0f", code_tgt, (step+1)))))

# AGGREGATION ------------------------------------------------------------------

data_tgt <- data %>% 
  dplyr::group_by(code_tgt_unique) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(value/total_sum > precision) # here we determine what to cut off

# data_src <- data %>% 
#   dplyr::group_by(code_src) %>% 
#   dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
#   dplyr::ungroup() %>% 
#   dplyr::filter(value/total_sum > precision) # here we determine what to cut off

# reroute to target "rest" if it's less than our minimum percentage
data <- data %>% 
  dplyr::mutate(code_tgt_unique = if_else(code_tgt_unique %in% data_tgt$code_tgt_unique,
                                   code_tgt_unique,
                                   rest_name # go to rest
                                   ))

data <- data %>% 
  dplyr::mutate(code_tgt = if_else(code_tgt_unique == rest_name,
                                   rest_name,
                                   code_tgt))

data <- data %>%
  dplyr::mutate(code_src_unique = if_else(step == 1 | code_src_unique %in% data$code_tgt_unique,
                                 code_src_unique,
                                 "skip"))
data <- data[data$code_src_unique != "skip",]

# now we group again the same way as before in order to aggregate
data <- data %>% 
  dplyr::group_by(step, code_src, code_tgt, model, code_src_unique, code_tgt_unique) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
  dplyr::ungroup()

# now we filter out everything that is rest!
data <- data[data$code_tgt != rest_name,]

# NODES ------------------------------------------------------------------------
# all operations below are to get unique nodes and to color them

# get all unique nodes
all_nodes <- data.frame(
  code = unique(c(data$code_src_unique, data$code_tgt_unique)),
  stringsAsFactors = FALSE
)

# get unique names without numbers
all_nodes$name <- ifelse(all_nodes$code == rest_name,
                         rest_name,
                         ifelse(all_nodes$code == "Final Demand",
                                "Final Demand",
                                substr(all_nodes$code, 1, nchar(all_nodes$code) - 3)))

# define the number of colors
n_cols <- length(unique(all_nodes$name))
# now we get the colors
if(n_cols <= 12){
  node_colors <- data.frame(
    colors = RColorBrewer::brewer.pal(n_cols, "Set3"),
    node = unique(all_nodes$name)
  )
} else { # more than 12 colors: expand palette
  node_colors <- data.frame(
    colors = colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n_cols),
    node = unique(all_nodes$name)
  )
}

all_nodes$color <- node_colors$color[match(all_nodes$name, node_colors$node)]

all_nodes$index <- c(1L:nrow(all_nodes))-1L

# LINKS ------------------------------------------------------------------------
# all operations below are to get unique nodes and to color them

# now we join the nodes back to the original data so we have the index that we need
links <- data %>%
  dplyr::filter(!is.na(value) & value > 0) %>% 
  dplyr::left_join(all_nodes[,c("code", "index")], by = c("code_src_unique" = "code")) %>% 
  dplyr::rename(source_index = index) %>% 
  dplyr::mutate(color = if_else(model == "fabio", "rgba(34, 156, 91, .4)", "rgba(160, 160, 160, .4)")) %>% #"rgba(38, 166, 91, .3)", "rgba(149, 165, 166, .3)")) %>% 
  dplyr::left_join(all_nodes[,c("code", "index")], by = c("code_tgt_unique" = "code")) %>% 
  dplyr::rename(target_index = index) %>% 
  dplyr::group_by(source_index, target_index, color) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
  dplyr::select(value, source_index, target_index, color)

link_list <- list(
  source = links$source_index,
  target = links$target_index,
  value = links$value,
  color = links$color #,
  # label = sprintf("<b>%s</b><br>%.2f %% of total", links$product, links$amount/sum(links$amount)*100)
)

# NODES, again -----------------------------------------------------------------
# now we check, how much percent flows into a node (except for step 1)

all_nodes <- all_nodes %>% 
  dplyr::left_join(links[,c("target_index", "value")], by = c("index" = "target_index")) %>% 
  dplyr::group_by(code, name, color, index) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE))

all_nodes <- all_nodes %>% 
  dplyr::left_join(links[,c("source_index", "value")], 
                   by = c("index" = "source_index"),
                   suffix = c("", ".src")) %>% 
  dplyr::group_by(code, name, color, index, value) %>% 
  dplyr::summarise(value.src = sum(value.src, na.rm = TRUE)) %>% 
  dplyr::mutate(value = max(c(value, value.src))) %>% 
  dplyr::select(-value.src) %>% 
  dplyr::mutate(percent = value/total_sum*100)

all_nodes <- all_nodes %>% 
  dplyr::arrange(index)

node_list <- list(
  label = sprintf("%s (%.0f%%)", 
                  # shorten name if it's longer than 30 chars
                  ifelse(nchar(all_nodes$name) > 23,
                         paste0(substr(all_nodes$name, 1, 20), "..."),
                         all_nodes$name),
                  all_nodes$percent),
  color = all_nodes$color,
  pad = 15,
  thickness = 30,
  line = list(
    #color = colors$nodes[5],
    width = 0 # 0 width because it doesn't look good
  )
)

# node_list$label <- paste0("<b>",node_list$label,"</b>")

p <- plotly::plot_ly(
  type = "sankey",
  orientation = "h", # alternative: v
  #valueformat = ".0f",
  valuesuffix = " (land footprint)",
  # iterations = 0,
  
  arrangement = "snap", # default: "snap"
  textfont = list(
    # family = ,
    size = 12,
    color = "black"
  ),
  node = node_list,
  link = link_list
  ) %>%
  plotly::layout(
    # title = sprintf("FABIO SPA for %s (%s) - land footprint %.2e ha",
    #                 product,
    #                 country,
    #                 total_sum),
    # paper_bgcolor = "green",
    xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
  )

p

