## VIF Function
h2o.vif <- function(data,cols) {
  #   dat = h2o.getFrame(model@parameters$training_frame)
  #   x = model@parameters$x
  #   X = dat[,x]
  
  dat = data
  x = cols
  X = dat[,cols]
  
  types = as.character(h2o.describe(frame = X)$Type)
  cats = x[types == "enum"]
  xi = x
  ## Run one-hot encoding manually
  for(cat in cats) {
    lvls = h2o.levels(X[,cat])
    ## Set level that's most frequently occuring as the reference level
    ## If there are ties, take the first occuring level
    cnts = as.data.frame(h2o.table(X[,cat]))
    lvls = lvls[-(which(cnts$Count == max(cnts$Count))[1])]
    for(lvl in lvls) {
      X[, paste0(cat,".", lvl)] = ifelse(X[,cat] == lvl, 1, 0)
      xi = c(xi, paste0(cat,".", lvl))
    }
    X = X[-which(xi == cat)]
    xi = setdiff(xi, cat)
  }
  
  ## Find Constant Columns
  desc = h2o.describe(X)
  constant_cols = row.names(desc)[(desc$Missing == 0) & (desc$Min == desc$Max)]
  # X = X[-which(names(X) %in% constant_cols)]
  xi = setdiff(xi, constant_cols)
  colnames(X) = xi
  ## Run regression of each predictors against all others
  r2 = c()
  i = 1
  h2o.no_progress()
  print("Proportion completed:")
  while(i <= length(xi) ) {
    lm = h2o.glm(y = xi[i],
                 x = setdiff(xi, xi[i]),
                 training_frame = X,
                 lambda = 0,
                 model_id = paste0("VIF", "_", x[i]))
    r2 = c(r2, h2o.r2(lm) )
    h2o.rm(lm@model_id)
    print(i/length(xi))
    i = i + 1 
  }
  res = data.frame(names = xi, r2 = r2, tolerance = 1-r2, vif = 1/(1-r2))
  res = res[order(res$vif, decreasing = T),]
  return(res)
}

## Example code
library(h2o)
h2o.init(nthreads = -1)
prostate.hex = h2o.importFile( path = system.file("extdata", "prostate.csv", package="h2o"))
prostate.hex$RACE = as.factor(prostate.hex$RACE)
prostate.hex$DCAPS = as.factor(prostate.hex$DCAPS)
prostate.glm = h2o.glm(x = 3:9, y = 2, family =  "gaussian", training_frame = prostate.hex, lambda = 0)
h2o.vif(data = prostate.hex, cols = 3:9)

# # Comparing to SAS docs
# # doc link: https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_reg_sect038.htm
# fitness = h2o.importFile(path = "/path/to/fitness.csv")
# fit.glm = h2o.glm(x = setdiff(1:ncol(fitness), 3), y = 3, training_frame = fitness, lambda = 0)
# h2o.vif(fit.glm)
