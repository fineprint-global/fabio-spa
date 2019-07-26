library(tidyverse)
# library(RColorBrewer)

setwd("sankey")

# control panel ----------------------------------------------------------------

# set the precision used to cut off flows to either [rest] or RoW 
# depending on if the flow goes to final demand (then RoW) or not ([rest])
precision <- 0.5*1e-2 # 0,5% precision

country <- c("BRA", "IDN")[2]
item <- c("Cattle", "Soyabeans", "Oil, palm fruit", "Wood")[4]

# get data  --------------------------------------------------------------------
if(item == "Wood"){
  data_1 <- read.csv(paste0("../output/results_spa_2013_", country, "_Industrial roundwood, coniferous.csv"))
  data_2 <- read.csv(paste0("../output/results_spa_2013_", country, "_Industrial roundwood, non-coniferous.csv"))
  data_3 <- read.csv(paste0("../output/results_spa_2013_", country, "_Wood fuel.csv"))
  
  data <- rbind(data_1, data_2, data_3)
} else {
  data <- read.csv(paste0("../output/results_spa_2013_", country, "_", item ,".csv"))
}

# get the index to know the meaning of the codes in data
# the relevant column is called "X"
index <- read.csv("../input/index.csv")

# prepare data  ----------------------------------------------------------------

# merge all Wood for Indonesia to one single starting point
if(item == "Wood"){
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

# AGGREGATION ------------------------------------------------------------------

# reroute to target "rest" if it's less than our minimum percentage
data <- data %>% 
  dplyr::mutate(target = if_else(value/total_sum < precision, 
                                 if_else(target %in% data$source, # check if it is endpoint = Consumption
                                         "0", # if not: go to rest
                                         as.character(index[index$country == "RoW" & index$item  == "","X"])), # consumption? then go to RoW
                                 target))

# remove sources that are never targeted
data <- data %>%
  dplyr::mutate(source = if_else(step == 1 | source %in% data$target,
                                 source,
                                 "skip"))
data <- data[data$source != "skip",]

# this recode of all Ls is needed because otherwise at the links below
# we take the wrong code although the code should be "0"
data <- data %>% 
  dplyr::mutate(
    L1 = if_else(target == "0" & step == 1, 0L, as.integer(L1)),
    L2 = if_else(target == "0" & step == 2, 0L, as.integer(L2)),
    L3 = if_else(target == "0" & step == 3, 0L, as.integer(L3)),
    L4 = if_else(target == "0" & step == 4, 0L, as.integer(L4)),
    L5 = if_else(target == "0" & step == 5, 0L, as.integer(L5))
  )

# NODES ------------------------------------------------------------------------
# all operations below are to get unique nodes and to color them

# get unique sources and targets that we later use to get all unique nodes
all_src_tgt_nodes <- data %>% 
  dplyr::group_by(step, source, target, L0, L1, L2, L3, L4, L5) %>%
  dplyr::summarise() %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(code_src = dplyr::recode(step,
                                          L0,
                                          L1,
                                          L2,
                                          L3,
                                          L4
                                        ),
                code_tgt = if_else(target == "0",
                                   0L,
                                   if_else(target == as.character(index[index$country == "RoW" & 
                                                                          index$item  == "","X"]),
                                           as.integer(target),
                                           dplyr::recode(step,
                                                          as.integer(L1),
                                                          as.integer(L2),
                                                          as.integer(L3),
                                                          as.integer(L4),
                                                          as.integer(L5)
                                                        )
                                          )
                                  )
                ) %>% 
  dplyr::group_by(source, target, code_src, code_tgt) %>% 
  dplyr::summarise() %>% 
  dplyr::ungroup() %>% 
  dplyr::select(source, target, code_src, code_tgt)

# get all unique nodes
all_nodes <- data.frame(
  id = c(all_src_tgt_nodes$source, all_src_tgt_nodes$target),
  code = c(all_src_tgt_nodes$code_src, all_src_tgt_nodes$code_tgt)
) %>% 
  dplyr::group_by(id, code) %>% 
  dplyr::summarise() %>% 
  dplyr::left_join(index, by = c("code" = "X"))

# we change the class of ISO to character because we get countrycodes later
all_nodes$ISO <- as.character(all_nodes$ISO)

# change the name for the "Rest" with ID 0
rest_name <- "[rest]"
levels(all_nodes$country) <- c(levels(all_nodes$country), rest_name)
# levels(all_nodes$item) <- c(levels(all_nodes$item), rest_name)
all_nodes$country[all_nodes$id == "0"] <- rest_name
all_nodes$ISO[all_nodes$id == "0"] <- ""
all_nodes$item[all_nodes$id == "0"] <- ""

# change EXIO 2-letter country codes to 3-letter ISO codes
# this is needed to have a unified output in the sankey
# (otherwise there would be two different abbreviations for the same country)
all_nodes <- all_nodes %>% 
  dplyr::mutate(ISO = if_else(nchar(ISO) == 2, 
                              countrycode::countrycode(country, 
                                                       origin = "country.name",
                                                       destination = "iso3c"),
                              ISO))

# define the number of colors
n_cols <- length(unique(all_nodes$country))
# now we get the colors
if(n_cols <= 12){
  country_colors <- data.frame(
    colors = RColorBrewer::brewer.pal(n_cols, "Set3"),
    country = unique(all_nodes$country)
  )
} else { # more than 12 colors: expand palette
  country_colors <- data.frame(
    colors = colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(n_cols),
    country = unique(all_nodes$country)
  )
}

all_nodes$color <- country_colors$color[match(all_nodes$country, country_colors$country)]

### Now we go away from the tree structure (that we would have if we keep the id)
tree <- all_nodes

all_nodes <- all_nodes %>% 
  dplyr::group_by(code, country, ISO, item, color) %>% 
  dplyr::summarise()

all_nodes$index <- c(1L:nrow(all_nodes))-1L

tree <- tree %>% 
  dplyr::left_join(all_nodes[,c("code", "index")], by = c("code" = "code"))

# LINKS ------------------------------------------------------------------------
# all operations below are to get unique nodes and to color them

# now we join the nodes back to the original data so we have the index that we need
links <- data %>%
  dplyr::filter(!is.na(value)) %>% 
  dplyr::left_join(tree[,c("id", "index", "item", "model")], by = c("source" = "id")) %>% 
  dplyr::rename(source_index = index) %>% 
  dplyr::mutate(color = if_else(model == "fabio", "rgba(38, 166, 91, .3)", "rgba(149, 165, 166, .3)"))

links <- links %>% 
  dplyr::left_join(tree[,c("id", "index")], by = c("target" = "id")) %>% 
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
  dplyr::group_by(code, country, ISO, item, color, index) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE))

all_nodes <- all_nodes %>% 
  dplyr::left_join(links[,c("source_index", "value")], 
                   by = c("index" = "source_index"), 
                   suffix = c("", ".src")) %>% 
  dplyr::group_by(code, country, ISO, item, color, index, value) %>% 
  dplyr::summarise(value.src = sum(value.src, na.rm = TRUE)) %>% 
  dplyr::mutate(value = max(c(value, value.src))) %>% 
  dplyr::select(-value.src) %>% 
  dplyr::mutate(percent = value/total_sum*100)

# 
# %>% 
#   dplyr::mutate(percent_total = if_else(index == 1,
#                                         sum(links[links$source_index == index,"value"], na.rm = TRUE),
#                                         sum(links[links$target_index == index,"value"], na.rm = TRUE)))
# 
# all_nodes$percent <- sum(links[all_nodes$index == links$target_index,"value"], na.rm = TRUE)

node_list <- list(
  label = if_else(all_nodes$item == "", 
                  sprintf("%s (%.0f%%)", as.character(all_nodes$country), all_nodes$percent), 
                  sprintf("%s (%s, %.0f%%)", all_nodes$item, all_nodes$ISO, all_nodes$percent)),
  color = all_nodes$color,
  pad = 15,
  thickness = 30,
  line = list(
    #color = colors$nodes[5],
    width = 0 # 0 width because it doesn't look good
  )
)

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
    title = sprintf("FABIO SPA for %s (%s) - land footprint %.2e ha",
                    item,
                    country,
                    total_sum),
    # paper_bgcolor = "green",
    xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
  )

p

