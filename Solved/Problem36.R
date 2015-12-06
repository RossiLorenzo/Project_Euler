# The decimal number, 
# 585 = 1001001001 (binary), is palindromic in both bases.
# 
# Find the sum of all numbers, less than one million, 
# which are palindromic in base 10 and base 2.
# 
# (Please note that the palindromic number, in either base,
# may not include leading zeros.)

# Library
library(parallel)

# Function to check palyndroms
is_palyndrome = function(x){ 
  y = paste0(rev(strsplit(as.character(x), "", fixed = TRUE)[[1]]), collapse = "")
  x == y
}

# Function to calcluate base 2
to_base2 = function(x){
  y = sapply(strsplit(paste(rev(intToBits(x))), "", fixed = TRUE),`[[`,2)
  min_non0 = min(which(y != 0))
  paste0(y[min_non0 : length(y)], collapse = "")
}

# For all number <= 10^6 check if they are palyndorm and in case
# they are return them
cores = makeCluster(10)
clusterExport(cores, c("is_palyndrome", "to_base2"))
X = parLapplyLB(cores, 1:10^6, function(i){
  if(!is_palyndrome(i))
    return(0)
  if(!is_palyndrome(to_base2(i)))
    return(0)
  return(i)
})
stopCluster(cores)

# Print result
sum(unlist(X))
