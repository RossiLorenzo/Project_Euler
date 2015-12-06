# The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
# 
# Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

# Libraries
library(gmp)

# Simply loop through the first 1000 numbers and get the last ten
# digits of n^n
X = lapply(1:1000, function(j){
  k = as.bigz(j)
  as.numeric(paste0(tail(strsplit(as.character(pow.bigz(k, k)), "", fixed = TRUE)[[1]], 10), collapse = ""))
})

# Sum all them together and get the last 10 digits
as.numeric(paste0(tail(strsplit(as.character(sum(unlist(X))), "", fixed = TRUE)[[1]], 10), collapse = ""))

