## In R Create ROC curve and return AUC
library(ROCR)
## Requires library ROCR
h2o.manual_perf <- function(pred, actual, measure = "auc") {
  pred = as.matrix(pred)[,1]
  actual = as.matrix(actual)[,1]
  auc = performance(prediction(pred, actual), measure = measure)
  auc = auc@y.values[[1]]
  return(auc)
}
## Example Code:
# demo(h2o.glm)
# predictions = h2o.predict(prostate.glm, prostate.hex)[,3]
# actual = prostate.hex[,"CAPSULE"]
# prostate_auc = h2o.manual_perf(predictions, actual)
# prostate_auc