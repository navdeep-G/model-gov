### Regression Performance Chart
### Requires data.table version 1.9.6 and above
h2o.regression_perf_table <- function(pred, actual, weight) {
  require(h2o)
  require(data.table)
  
  if(missing(pred)) stop("Please supply predictions frame...")
  if(missing(actual)) stop("Please supply frame of actual values...")
  if(missing(weight)) weight = rep(1, nrow(pred))
  
  ## Calculate r2 manually
  SSE = sum((actual - pred)^2)
  SST = sum((mean(actual) - actual)^2)
  r2 = 1 - (SSE/SST)
  
  ## Set your percentiles or bins
  cumulative_data_fraction = seq(0, 1, 0.1)

  ## Calculate the bounds of each bin
  bins = h2o.quantile(pred[,"predict"], probs = cumulative_data_fraction)
  hists = h2o.hist(x = pred[,"predict"], breaks = bins, plot = F)
  lower_threshold = sort(hists$breaks, decreasing = T)
  lower_threshold[ length(cumulative_data_fraction) ] = min(pred ) - 0.01
  lower_threshold[ 1 ] = max(pred ) + 0.01
  
  preds = data.table(as.data.frame(pred))
  actual = data.table(as.data.frame(actual))
  weights = data.table(as.data.frame(weight))
  
  ## Create data.table frame of necessary info
  dat = cbind(preds, actual, weights)
  names(dat) = c("pred", "actual", "weight")
  dat = dat[ order(dat$pred, decreasing = T),]

  ## Calculate actual total and actual total average
  tot = sum(dat$weight)
  actual_ttot = sum( dat$actual)
  actual_tavg = sum( dat$weight * dat$actual)/tot
  
  res = dat[, .(accounts = sum(weight),
                actual_avg = sum(weight * actual)/sum(weight),
                pred_avg = sum(weight * pred)/sum(weight),
                actual_tot = sum(actual),
                pred_tot = sum(pred),
                ratio_overallavg = (sum( weight * actual)/sum(weight) )/actual_tavg,
                ape = round( 100 * abs( sum(weight * actual) - sum(weight * pred))/sum(weight * actual), digits = 2)),
             by = list(interval = cut(pred, breaks = unique(lower_threshold), right = T))]
  
  res[, pct_accounts := round(100 * (accounts/tot), digits = 2)]
  res[, pct_actual := actual_tot/actual_ttot]
  res[, cumpct_accounts := cumsum(pct_accounts)]
  res[, cumpct_actual := 100 * cumsum(pct_actual)]
  res[, ro_brk := c(NA, -1 * diff(actual_avg))]
  res[, area_roc := round( 0.5 * (cumpct_actual + shift(cumpct_actual, 1L,type = "lag")) * c(cumpct_accounts[1], diff(cumpct_accounts)) / 100, digits = 2)]
  res[1,area_roc := round( 0.5 * cumpct_actual[1] * cumpct_accounts[1]/100, digits = 2)]  
  res = as.data.frame(res)

  ## Generate output object with perf table, r2, and mape slots 
  output = NULL
  output$perf_table = res
  output$mape = mean(res$ape)
  output$r2 = r2
  return(output)
}

h2o.r2 <- function(model, valid = F) {
  if(valid) {
    r2 = model@model$validation_metrics@metrics$r2
  } else {
    r2 = model@model$training_metrics@metrics$r2
  }
  return(r2)
}

library(h2o)
h2o.init(nthreads = -1)
prostate.hex = h2o.uploadFile(path = system.file("extdata", "prostate.csv", package="h2o"), destination_frame = "prostate.hex")
prostate.gbm = h2o.gbm(x = setdiff(colnames(prostate.hex), "AGE"), y = "AGE", training_frame = prostate.hex, ntrees = 10, max_depth = 5, learn_rate = 0.1)

pred = h2o.predict(object =  prostate.gbm, prostate.hex)
actual = prostate.hex[,"AGE"]

perf_table = h2o.regression_perf_table(pred = pred, actual = actual)
h2o.r2(prostate.gbm, valid = F)
