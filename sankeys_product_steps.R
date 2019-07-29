library(tidyverse)
# library(RColorBrewer)

# control panel ----------------------------------------------------------------

# set the precision used to cut off flows to either [rest] or RoW 
# depending on if the flow goes to final demand (then RoW) or not ([rest])
precision <- 0.5*1e-2 # 0.5% precision
rest_name <- "[rest]"

country <- c("BRA", "IDN")[2]
product <- c("Cattle", "Soyabeans", "Oil, palm fruit", "Wood")[3]

# get data  --------------------------------------------------------------------
if(product == "Wood"){
  data_1 <- read.csv(paste0("./output/results_spa_2013_", country, "_Industrial roundwood, coniferous.csv"))
  data_2 <- read.csv(paste0("./output/results_spa_2013_", country, "_Industrial roundwood, non-coniferous.csv"))
  data_3 <- read.csv(paste0("./output/results_spa_2013_", country, "_Wood fuel.csv"))
  
  data <- rbind(data_1, data_2, data_3)
  rm(data_1, data_2, data_3)
} else {
  data <- read.csv(paste0("./output/results_spa_2013_", country, "_", product ,".csv"))
}

# get the index to know the meaning of the codes in data
# the relevant column is called "X"
index <- read.csv("./input/index.csv")

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

data <- data[data$step != 5,]

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
                           L3
    ),
    target = dplyr::recode(step,
                           L1,
                           L2,
                           L3,
                           L4
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
data <- data[data$code_tgt != "",]

data <- data %>% 
  dplyr::group_by(step, code_src, code_tgt, model) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% 
  dplyr::ungroup() # ungroup because we modify later

data <- data %>% 
  dplyr::mutate(code_src_unique = ifelse(code_src == rest_name, 
                                         rest_name, 
                                         sprintf("%s #%.0f", code_src, step)),
                code_tgt_unique = ifelse(code_tgt == rest_name, 
                                         rest_name, 
                                         sprintf("%s #%.0f", code_tgt, (step+1))))

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
                                   ))#,
                # code_src = if_else(step == 1 | code_src %in% data_tgt$code_tgt,
                #                    as.character(code_src),
                #                    "skip")
  #               ) %>% 
  # dplyr::filter(value > 0, 
  #               code_src != "skip")

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
                         substr(all_nodes$code, 1, nchar(all_nodes$code) - 3))

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
  dplyr::mutate(color = if_else(model == "fabio", "rgba(38, 166, 91, .3)", "rgba(149, 165, 166, .3)")) %>% 
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
  dplyr::group_by(code, color, index) %>% 
  dplyr::summarise(value = sum(value, na.rm = TRUE))

all_nodes <- all_nodes %>% 
  dplyr::left_join(links[,c("source_index", "value")], 
                   by = c("index" = "source_index"),
                   suffix = c("", ".src")) %>% 
  dplyr::group_by(code, color, index, value) %>% 
  dplyr::summarise(value.src = sum(value.src, na.rm = TRUE)) %>% 
  dplyr::mutate(value = max(c(value, value.src))) %>% 
  dplyr::select(-value.src) %>% 
  dplyr::mutate(percent = value/total_sum*100)

all_nodes <- all_nodes %>% 
  dplyr::arrange(index)

node_list <- list(
  label = sprintf("%s (%.0f%%)", all_nodes$code, all_nodes$percent),
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
                    product,
                    country,
                    total_sum),
    # paper_bgcolor = "green",
    xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
    yaxis = list(showgrid = F, zeroline = F, showticklabels = F)
  )

p

