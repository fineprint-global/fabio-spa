
#' Recursively split alpha (x[p]), given a split defined by D[p, ].
#' Calculate for values >= epsilon and go through lvl_cap layers.
#' Uses .get_path internally to minimise redundant operations.
#' 
#' @param alpha Numeric scalar. Starting value to split according to D.
#' @param D Numeric row-stochastic matrix. Used to split up alpha. If ncol(D) >
#' nrow(D) these positions are considered to be final.
#' @param p Integer scalar. Indicates the row of D corresponding to alpha.
#' @param epsilon Numeric scalar. Value at which to stop splitting up alpha.
#' @param lvl_cap Integer scalar- Maximum number of splits to apply to alpha.
#' @param parallel Integer scalar. Whether to initialise the algorithm using 
#' parallel::parLapply and if so taken to be the number of cores to use.
#' This implementation will need to replicate D once per core, potentially 
#' causing memory issues. Turned off by default, i.e. set to NULL.
#' @param lvl Integer scalar. Used internally to keep track of the number of 
#' splits to apply.
#'
#' @return Returns a (recursive) list with the following elements:
#' lvl, split & down. These hold the current level (an integer), the split 
#' applied at this level (a vector with the resulting values, named after the
#' target columns of D) and the next iteration of the algorthm
get_path <- function(
  alpha, 
  D, 
  p, 
  epsilon, 
  lvl_cap = 3L, 
  parallel = NULL,
  lvl = 0L) {
  
  # Used internally to avoid redundancy
  .get_path <- function(
    alpha, p, lvl = 0L) {
    
    tmp <- alpha * D[p, ]
    
    out <- tmp[tmp >= epsilon]
    rest <- sum(tmp[tmp < epsilon])
    
    if(lvl >= lvl_cap) {return(list(lvl = lvl, split = c(out, "rest" = rest)))}
    lvl <- lvl + 1L
    
    ps <- as.integer(gsub("c([0-9]+)", "\\1", names(out)))
    alphas <- names(out)[ps <= nrow(D)]
    
    down <- lapply(alphas, function(x) {
      .get_path(out[[x]], p = as.integer(gsub("c([0-9]+)", "\\1", x)), lvl = lvl)
    })
    names(down) <- alphas
    
    list(lvl = lvl - 1L,
         split = c(out, "rest" = rest),
         down = down)
  }
  # End .get_path
  
  # Adjust the colnames of D to help identify the next split to apply
  colnames(D) <- paste0("c", 1:ncol(D))
  
  # Apply split
  tmp <- alpha * D[p, ]
  
  # Cut values < epsilon , i.e. move them to "rest"
  out <- tmp[tmp >= epsilon]
  rest <- sum(tmp[tmp < epsilon])
  
  # Final node reached
  if(lvl >= lvl_cap) {return(list(lvl = lvl, split = c(out, "rest" = rest)))}
  lvl <- lvl + 1L
  
  ps <- as.integer(gsub("c([0-9]+)", "\\1", names(out)))
  alphas <- names(out)[ps <= nrow(D)]
  
  # Go down one node
  down <- if(is.null(parallel)) {
    lapply(alphas, function(x) {
      .get_path(out[[x]], p = as.integer(gsub("c([0-9]+)", "\\1", x)),  lvl = lvl)
    })
  } else {
    # Objects accessed from the parent environment need to be handed over explicitly
    library(parallel)
    n_cores <- parallel
    cl <- makeCluster(n_cores)
    parLapply(cl, alphas, function(x, D, epsilon, lvl_cap) {
      .get_path(out[[x]], p = as.integer(gsub("c([0-9]+)", "\\1", x)), lvl = lvl)
    }, D, epsilon, lvl_cap, .get_path)
    stopCluster(cl)
  }
  names(down) <- alphas
  
  # Done
  list(lvl = lvl - 1L,
       split = c(out, "rest" = rest),
       down = down)
}


#' Transform a tree-list into a data.table with level, id-path and value
#'
#' @param tree A list with a tree output by get_path.
#' @param prev String used internally to keep track of the splits applied.
#'
#' @return Returns a data.table version of the information contained in x.
#' 
#' @import data.table
df_ify <- function(tree, prev = "") {
  
  library(data.table)
  
  if(is.null(tree$down)) { # Final node
    return(data.table("lvl" = tree[[1]], 
                      id = paste0(prev, names(tree[[2]])),
                      val = tree[[2]]))
  }
  
  rbindlist(list(
    data.table("lvl" = tree[[1]], 
               id = paste0(prev, names(tree[[2]])),
               val = tree[[2]]),
    rbindlist(lapply(names(tree[["down"]]), function(name, x, prev) {
      df_ify(x[["down"]][[name]], prev = paste0(name, " > ", prev))
      }, tree, prev))
  ))
}
