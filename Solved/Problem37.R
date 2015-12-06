# The number 3797 has an interesting property. 
# Being prime itself, it is possible to continuously remove digits 
# from left to right, and remain prime at each stage: 3797, 797, 97, and 7. 
# Similarly we can work from right to left: 3797, 379, 37, and 3.
# 
# Find the sum of the only eleven primes that are both truncatable 
# from left to right and right to left.
# 
# NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

# Libraries
library(Rcpp)
sourceCpp("Functions/Eratosthenes_Sieve_for_R.cpp")

# Get first 10^6 primes
top_primes = Eratosthenes_Sieve(10^6)

# If a prime contains 0,2,4,5,6,8 and is >= 100 it can't be truncable 
to_check = union(
  top_primes[top_primes < 100 & top_primes > 10],
  top_primes[!grepl("0|2|4|5|6|8", top_primes)]
)

# For every prime calculate the truncated values
all_primes = unlist(lapply(to_check, function(i){
  # Truncate left and right by dividing for 10^i
  max_div = floor(log10(i))
  trunc_left = floor(i / 10^(1:max_div))
  trunc_rigth = i - (10^(1:max_div) * trunc_left)
  all(c(trunc_rigth, trunc_left) %in% top_primes)
}))

# Print result
sum(possible_primes)
