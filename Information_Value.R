## Roughly replicate information value from "woe" package

library(woe)
library(h2o)
h2o.init(nthreads = -1)
h2o.removeAll()

## Still need to add support for Enums

iv.H2OFrame <- function(frame, x, y) {
  unique_responses = length(h2o.levels ( frame[,y]))
  if( ! unique_responses == 2) stop("Response variable is not binary!")
  
  ## Subset frame and turn response to binary numeric
  fr = frame[,c(x,y)]
  fr[,y] = as.numeric(fr[,y])
  total_class_1 = sum(fr[,y])
  total_class_0 = nrow(fr) - sum(fr[,y])
  
  ## Sort frame into bins (make shift bins--NOT based off rpart)
  if(is.numeric( fr[,x]) ) {
    bins = h2o.quantile(fr[,x], probs = c(0.1, 0.25,0.333, 0.5, 0.667, 0.75))
    if(any(is.na(bins)) | sum(bins == 0) > 1) {
      fr_unique = as.data.frame( h2o.table(fr[,x]))
      bins = fr_unique[,1] [-c(1, nrow(fr_unique))]
    }
    bins = sort( unique( bins), decreasing = T)
    fr[,"Bin"] = 0
    for(i in 1:length(bins)){
      fr[,"Bin"] = ifelse ( fr[,x] < bins[i], i,  fr[,"Bin"])
    }
    ## Calculate the information value 
    count = as.data.frame(h2o.group_by(fr, by = "Bin", nrow(x), sum(y)))
    names(count) = c("Bin", "Total", "Total_Class_1")
    count[,"Total_Class_0"] = count$Total - count$Total_Class_1
  } else {
    count = as.data.frame( h2o.group_by(fr, by = x, nrow(x), sum(y)))
    names(count) = c("Bin", "Total", "Total_Class_1")
    count[,"Total_Class_0"] = count$Total - count$Total_Class_1
  }
  
  d0 = count[, "Total_Class_0"]/total_class_0
  d1 = count[, "Total_Class_1"]/total_class_1
  woe = ifelse(d0 == 0 | d1 == 0, 0, log(d0/d1))
  miv = (d0-d1) * woe
  if(is.numeric(fr[,x])) {
    upper_bound = c(max(fr[,x], na.rm = T),bins)
  } else {
    upper_bound = count$Bin
  }
  res = data.frame(upper_bound = upper_bound,
                   outcome_0 = count[, "Total_Class_0"],
                   outcome_1 = count[, "Total_Class_1"],
                   distribution_0 = d0,
                   distribution_1 = d1,
                   woe = woe,
                   miv = miv)
  iv = sum( res[, "miv"]) 
  gc()
  cat("Variable :", x, " \n")
  cat("Information Value :", iv, " \n")
  res = res[order(res$upper_bound, decreasing = F),]
  attr(res, "IV") = iv
  return(res)
}

iris_2 <- iris[1:100,]
iris_2$Species = ifelse(iris_2$Species == "setosa", "setosa", "versicolor")
iris_2$Species = as.factor(iris_2$Species)
iris.hex <- as.h2o(iris_2)

x = names(iris)[1:4]
iv.num(df = iris_2, x = x[1], y = "Species")
iv.H2OFrame(frame = iris.hex, x = x[1] , y = "Species")

## LOOP THROUGH ALL FEATURES -- expensive
ivs = lapply(x, function(x) {
  res = iv.H2OFrame(frame = iris.hex, x = x[1] , y = "Species")
  return( attr(res, "IV")) })

iv_table = data.frame(names = x, ivs = unlist(ivs))
## To write out to disk
write.table(x = iv_table, file = "...")