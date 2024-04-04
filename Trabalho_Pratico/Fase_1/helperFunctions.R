fun1 <- function(i) { #read CSV data
  read.csv(paste(basepath,i,sep = "/"), header=FALSE,stringsAsFactors = FALSE)
}