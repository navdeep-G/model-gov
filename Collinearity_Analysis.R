## Collinearity Analysis
h2o.collin <- function(data, cols, add.intercept=TRUE, impute_missing = TRUE) {
  #   dat = h2o.getFrame(model@parameters$training_frame)
  #   x = model@parameters$x
  #   X = dat[,x]
  
  dat = data
  x = cols
  X = dat[,cols]
  if(add.intercept) X[,"INTERCEPT"] = 1 
  
  ## Run one-hot encoding manually
  types = as.character(h2o.describe(frame = X)$Type)
  cats = x[types == "enum"]
  xi = x
  for(cat in cats) {
    lvls = h2o.levels(X[,cat])
    ## Set level that's most frequently occuring as the reference level
    cnts = as.data.frame(h2o.table(X[,cat]))
    lvls = lvls[-which(cnts$Count == max(cnts$Count))]
    # If equal counts occur
    if (length(lvls) == 0) lvls = h2o.levels(X[,cat])[-1]
    for(lvl in lvls) {
      X[, paste0(cat,".", lvl)] = ifelse(X[,cat] == lvl, 1, 0)
      xi = c(xi, paste0(cat,".", lvl))
    }
    X = X[-which(xi == cat)]
    xi = setdiff(xi, cat)
  }
  
  ## Find Constant Columns
  desc = h2o.describe0(X)
  constant_cols = row.names(desc)[(desc$Missing == 0) & (desc$Min == desc$Max)]
  # X = X[-which(names(X) %in% constant_cols)]
  xi = setdiff(xi, constant_cols)
  
  ## Scale X using the root mean square defined as sqrt(sum(x^2)/(n-1))
  ## Impute 0 for NA when calculating sum (h2o's apply don't accept sum(na.rm))
  X2 = X^2
  X2 = ifelse(is.na(X2), 0, X2)
  rms = as.matrix(sqrt( apply(X2, 2, sum)/(nrow(X2)-1)))[1,]
  for(i in 1:length(rms)){
    X[,i] = X[,i]/rms[i]
  }
  
  ## Run SVD in H2O
  h2o.show_progress()
  pca = h2o.prcomp(training_frame = X, k = ncol(X), ignore_const_cols = F, transform = "NONE", pca_method = "GramSVD", impute_missing = impute_missing)
  vecs = as.data.frame(pca@model$eigenvectors)
  vals = as.data.frame(t(pca@model$importance))
  vals = nrow(vals) * vals$`Proportion of Variance`
  
  ## Calculate condition index which is defined as the sqrt(max_eigenvalue/eigenvalues)
  condition_index = sqrt(max(vals)/vals)
  
  collinearity_diag = data.frame(number = 1:nrow(vecs), 
                                 eigenvalue = vals, 
                                 condition_index = condition_index)
  return(collinearity_diag)
}

## Example code
library(h2o)
h2o.init(nthreads = -1)
prostate.hex = h2o.importFile( path = system.file("extdata", "prostate.csv", package="h2o"))
prostate.hex$RACE = as.factor(prostate.hex$RACE)
prostate.hex$DCAPS = as.factor(prostate.hex$DCAPS)
prostate.glm = h2o.glm(x = 3:9, y = 2, family =  "gaussian", training_frame = prostate.hex, lambda = 0)
h2o.collin(data = prostate.hex, cols = 3:9, impute_missing = T)

# ## Comparing to SAS docs
# ## doc link: https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_reg_sect038.htm
# ## Run with fitness dataset.
# fitness = h2o.importFile(path = "/Users/amy/0xdata/data/fitness.csv")
# fit.glm = h2o.glm(x = setdiff(1:ncol(fitness), 3), y = 3, training_frame = fitness, lambda = 0)
# ## To run collin set add.intercept = T
# h2o.collin(model = fit.glm, add.intercept = T)
# ## To run with collinoint set add.intercept = F
# h2o.collin(model = fit.glm, add.intercept = F)
