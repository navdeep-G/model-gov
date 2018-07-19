## KS chart function returns max ks
h2o.ks_chart <- function(model, data) {
  perf = h2o.performance(model = model, newdata = data)
  
  lift = as.data.frame(h2o.gainsLift(perf))
  
  rows = nrow(data)
  lift$accum_data = lift$cumulative_data_fraction * rows
  lift$accum_responders = lift$cumulative_response_rate * lift$accum_data
  lift$accum_nonresponders = lift$accum_data - lift$accum_responders
  lift$accum_responders_percentage = lift$cumulative_capture_rate
  lift$accum_nonresponders_percentage = lift$accum_nonresponders/max(lift$accum_nonresponders)
  lift$ks = abs(lift$accum_responders_percentage - lift$accum_nonresponders_percentage) * 100
  lift2 = lift[,c( "cumulative_data_fraction", "lower_threshold", "accum_data", "accum_responders", "accum_nonresponders", "accum_responders_percentage","accum_nonresponders_percentage", "ks")]
  attr(lift2, "max_ks") = max(lift2$ks)
  return(lift2)
}

## Example Code:
# demo(h2o.glm)
# ks_chart = h2o.ks_chart(prostate.glm, prostate.hex)
# max_ks = attr(ks_chart, "max_ks")


## Requires data.table to do a quick sort
h2o.manual_ks_chart <- function(pred, actual) {
  require(data.table)
  pred = data.table(as.data.frame(pred))
  actual = data.table(as.data.frame(actual))
  
  rows = nrow(pred)
  
  dat = cbind(pred, actual)
  names(dat) = c("prob", "label")
  dat = dat[ order(dat$prob, decreasing = T),]
  
  cumulative_data_fraction = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7 ,0.8, 0.9, 1.0)
  lower_threshold = quantile(dat$prob, probs = 1 - cumulative_data_fraction)
  lower_threshold[ length(cumulative_data_fraction) ] = min(dat$prob ) - 0.01
  
  class0 = names(table(actual))[1]
  class1 = names(table(actual))[2]
  
  dat = dat[, .(responders_count = sum(label == class1), nonresponders_count = sum(label == class0)), by = list(prob = cut(prob, breaks = lower_threshold, right = F))] 
  ## Alternative ways to do rollups
  # (1)  dat[, {tt=label=="0"; list(sum(tt),sum(!tt))}, by=...]
  # (2)  dat[, .N, by=.(cut(...),label)] %>% dcast 
  # dat[, cumulative_data_fraction := cumulative_data_fraction]
  dat[, accum_responders := cumsum(responders_count)]
  dat[, accum_nonresponders := cumsum(nonresponders_count)]
  dat[, accum_data := accum_responders + accum_nonresponders] 
  dat[, accum_responders_percentage := accum_responders/max(accum_responders)]
  dat[, accum_nonresponders_percentage := accum_nonresponders/max(accum_nonresponders)]
  dat[, ks := abs(accum_responders_percentage-accum_nonresponders_percentage) * 100]
  dat[, "lower_threshold"] = lower_threshold
  dat[, "cumulative_data_fraction"] = cumulative_data_fraction
  dat = as.data.frame(dat)
  
  dat = dat[,c( "cumulative_data_fraction", "lower_threshold", "accum_data", "accum_responders", "accum_nonresponders", "accum_responders_percentage","accum_nonresponders_percentage", "ks")]
  attr(dat, "max_ks") = max(dat$ks)
  return(dat)
}
# Example Code:
# predictions = h2o.predict(prostate.glm, prostate.hex)[,3]
# actual = prostate.hex[,"CAPSULE"]
# ks_chart2 = h2o.manual_ks_chart(pred = predictions, actual = actual)

