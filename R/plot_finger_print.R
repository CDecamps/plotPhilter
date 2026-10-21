#' plot_finger_print
#'
#' @param raw_counts raw count matrix 
#' @param var_metadata vector of groups
#'
#' @returns
#' @export
#'
#' @examples
plot_finger_print = function(raw_counts, var_metadata, xmin = 0, xmax = 1){
  
  data = as.matrix(raw_counts)
  
  res = c()
  for(i in 1:ncol(data)){
    reads_loc = sort(data[,i ])
    res = cbind(res, cumsum(reads_loc) / sum(reads_loc))
  }
  
  df = data.frame(prop = c(res), 
                  rank = rep(1:nrow(res)) / nrow(res),
                  sample = rep(colnames(data), each = nrow(res)),
                  group_var = factor(rep(var_metadata, each = nrow(res))))
  
  return(ggplot(df, aes(x = rank, y = prop, group = sample, color = group_var)) +
           geom_line() +
           theme_minimal() + xlim(xmin, xmax) + 
           labs(title = "", 
                x = "Ranked genomic tiles",
                y = "Cumulative fraction of signal",
                color = ""))
  
}
