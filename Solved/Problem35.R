# The number, 197, is called a circular prime because all rotations 
# of the digits: 197, 971, and 719, are themselves prime.
# 
# There are thirteen such primes below 100: 
# 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
# 
# How many circular primes are there below one million?

# Library
library(Rcpp)
library(parallel)
sourceCpp("Functions/Eratosthenes_Sieve_for_R.cpp")

# All primes <= 10^6
top_primes = Eratosthenes_Sieve(10^6)

# If a prime contains 0,2,4,5,6,8 and is >= 100 it can't be circular 
to_check = union(
  top_primes[top_primes < 100],
  top_primes[!grepl("0|2|4|5|6|8", top_primes)]
)

# For every prime calculate the permutations and check if are still primes 
digits = strsplit(as.character(to_check), "", fixed = TRUE)
cores = makeCluster(10)
clusterExport(cores, "top_primes")
circular_primes = unlist(parLapplyLB(cores, digits, function(x){
  all_perms = NULL
  for(i in 1:length(x)){
    x = c(x[length(x)], x[-length(x)])
    all_perms = c(all_perms, paste0(x, collapse = ""))
  }
  all(all_perms %in% top_primes)
}))
stopCluster(cores)
sum(circular_primes)
