# We shall say that an n-digit number is pandigital if 
# it makes use of all the digits 1 to n exactly once. 
# For example, 2143 is a 4-digit pandigital and is also prime.
# 
# What is the largest n-digit pandigital prime that exists?

# Libraries
library(Rcpp)
library(dplyr)
sourceCpp("Functions/Eratosthenes_Sieve_for_R.cpp")

# It cannot be long 9 or 8 because that would be divisible by 3
# sum(1:9) = 45 %% 3 == 0, sum(1:8) = 36 %% 3 == 0
# Therefore we find primes <= 8 * 10^6
primes = Eratosthenes_Sieve(10000000)
primes = primes[primes >= 10^6 & primes < 8 * 10^6]

# Check that the prime has all digits 1->7
one_to_seven = function(x){
  all(unlist(lapply(1:7, function(y){ grepl(y, x) })))
}
# Start from the biggest primes and go down until the pandigital is found
found = FALSE
i = length(primes)
while(!found){
  if(one_to_seven(primes[i]))
    found = TRUE
  i = i - 1
}
primes[i + 1]
