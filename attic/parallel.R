library("plyr")
library("doParallel")

# detect cores
cores = detectCores()
cores

registerDoParallel(cores = cores)

result = llply(x, func, .parallel = TRUE)