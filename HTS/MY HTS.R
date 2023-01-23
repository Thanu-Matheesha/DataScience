library(hts)
library(Matrix)

bts <- ts(5 + matrix(sort(rnorm(500)), ncol=5, nrow=100))
y <- hts(bts, nodes=list(2, c(3, 2)))

# Returns all series in the hierarchy
ally <- aggts(y)

# Returns time series at levels 0 and 2
somey <- aggts(y, levels = c(0, 2)) 

# Returns the summing matrix
S <- smatrix(y)

# Graph
plot(y, levels = c(0, 1))

allts_infant <- allts(y)
allf <- matrix( nrow=10, ncol=ncol(allts_infant))

for(i in 1:ncol(allts_infant))
  allf[,i] <- forecast(auto.arima(allts_infant[,i]), h=10)$mean
allf <- ts(allf)

y.f <- combinef(allf, get_nodes(y), weights = NULL, keep = "gts", algorithms = "lu")
plot(y.f)


