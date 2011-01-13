find_x_label <- function(df) {
  vars <- setdiff(names(df), c(".wt", "l", "r", "t", "b", "level"))

  axis.set <- subset(df, (b==min(b)) &  (level==max(level)))
  
  paste(vars[sapply(vars, function(x) return(length(unique(axis.set[,x]))>1))],"")
}

find_y_label <- function(df) {
  vars <- setdiff(names(df), c(".wt", "l", "r", "t", "b", "level"))

  axis.set <- subset(df, (l==min(l)) & (level==max(level)))
  
  paste(vars[sapply(vars, function(x) return(length(unique(axis.set[,x]))>1))],"")
}