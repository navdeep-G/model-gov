## Logit Plot Example
library(h2o)
h2o.init(nthreads = -1)
prostate.hex = h2o.uploadFile(path = system.file("extdata", "prostate.csv", package="h2o"), destination_frame = "prostate.hex")
prostate.glm = h2o.glm(x = c("AGE","RACE","PSA","DCAPS"), y = "CAPSULE", training_frame = prostate.hex, family = "binomial", alpha = 0.5)

model = prostate.glm
dat = h2o.getFrame(model@parameters$training_frame)
pred = h2o.predict(object = model, newdata = dat)
pred = pred[,3]
names(pred) = "prob"

by_col = "PSA"
combined = h2o.cbind(pred, dat[,by_col])
combined = as.data.frame(combined)

combined = combined[order(combined[,by_col], decreasing = F),] 
plot(x = combined[,by_col], xlab = by_col, y = combined[,1], ylab = 'prob')
plot(x = combined[,by_col], xlab = by_col, y = log(combined[,1]/(1-combined[,1])), ylab = 'logit')