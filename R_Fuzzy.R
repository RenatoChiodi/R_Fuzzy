library("frbs")
data <- read.csv("~/Documentos/Mestrado/IA/bancario.csv")
head(data)


dataShuffled <- data[sample(nrow(data)), ]
dataShuffled[, 5] <- unclass(irisShuffled[, 5])
range.data.input <- apply(data[, -ncol(data)], 2, range)
tra.data <- irisShuffled[1:1000, ]
tst.data <- irisShuffled[1001:nrow(irisShuffled), 1:4]
real.data <- matrix(irisShuffled[1001:nrow(irisShuffled), 5], ncol = 1)

object.frbcs.w <- frbs.learn(tra.data, range.data = NULL, method.type = c("WM"), control = list(num.labels = 3, type.mf = "GAUSSIAN"))
summary(object.frbcs.w)

pred <- predict(object.frbcs.w, tst.data)
err <- 100 * sum((real.data==0 && pred > 0.5)||(real.data == 1 && pred <= 0.5)) / length(pred)
err