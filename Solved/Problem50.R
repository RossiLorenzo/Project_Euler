# The prime 41, can be written as the sum of six 
# consecutive primes:
#   
# 41 = 2 + 3 + 5 + 7 + 11 + 13
# This is the longest sum of consecutive primes that 
# adds to a prime below one-hundred.
# 
# The longest sum of consecutive primes below one-thousand 
# that adds to a prime, contains 21 terms, 
# and is equal to 953.
# 
# Which prime, below one-million, can be written as 
# the sum of the most consecutive primes?

# Libraries
library(Rcpp)
sourceCpp("Functions/Eratosthenes_Sieve_for_R.cpp")

# Generate all the primes >= 10^6
all_primes = Eratosthenes_Sieve(1000010)

# The max number length can be obtained adding together
# the first i primes. When the sum is >= 10^6 we have
# the upper limit for the sequence
x = 0
i = 1
while(x <= 10^6){
  x = sum(all_primes[1:i])
  i = i + 1
}

# Now for every sequence of length i, with i <= then the
# upper limit calculated above, we find all the possible 
# sums that are <= 10^6 and we check if they return a prime
# If not we decrease i
i = i - 1
found = FALSE
while(!found){
  t = -1
  x = 0
  while(x <= 10^6){
    if(x %in% all_primes){
      print(paste("The prime", x, "can be written as sum of", i, "primes"))
      found = TRUE
    }
    t = t + 1
    x = sum(all_primes[(1 + t) : (i + t)])
  }
  i = i - 1
}
