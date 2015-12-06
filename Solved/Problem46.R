# It was proposed by Christian Goldbach 
# that every odd composite number can be written as 
# the sum of a prime and twice a square.
# 
# 9 = 7 + 2×1^2
# 15 = 7 + 2×2^2
# 21 = 3 + 2×3^2
# 25 = 7 + 2×3^2
# 27 = 19 + 2×2^2
# 33 = 31 + 2×1^2
# 
# It turns out that the conjecture was false.
# 
# What is the smallest odd composite that cannot 
# be written as the sum of a prime and twice a square?

# Libraries
library(Rcpp)
library(dplyr)
sourceCpp("Functions/Eratosthenes_Sieve_for_R.cpp")

# Generate the first 10^6 primes
top_primes = Eratosthenes_Sieve(1000000)

# Filter only numbers that are composite
composite_numbers = setdiff(seq(1, max(top_primes), 2), top_primes)[-1]

# The number can be written as n = p + 2r^2
# Therefore r = sqrt((n - p) / 2) <= sqrt(n/2)
H = TRUE
i = 1
while(H){
  number = composite_numbers[i]
  max_root = floor(sqrt(number/2))
  H = any((number - 2*(1:max_root)^2) %in% top_primes)
  i = i + 1
}

composite_numbers[i-1]
