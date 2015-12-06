# The first two consecutive numbers to have two
# distinct prime factors are:
#   
# 14 = 2 × 7
# 15 = 3 × 5
# 
# The first three consecutive numbers to have three
# distinct prime factors are:
#   
# 644 = 2² × 7 × 23
# 645 = 3 × 5 × 43
# 646 = 2 × 17 × 19.
# 
# Find the first four consecutive integers to have four 
# distinct prime factors. What is the first of these numbers?

library(dplyr)
library(parallel)

# For every number <= 10^6 count the number of factors
df = data_frame(numbers = 600:1000000)
cores = makeCluster(10)
df$nfactors = unlist(parLapplyLB(cores, df$numbers, function(i){
  library(gmp)
  length(unique(factorize(i)))
}))
stopCluster(cores)

# Filter out only the sequences with 4 consecutive numbers
# having 4 factors
res = df %>% 
  mutate(lagfact = lag(nfactors),
         laglagfact = lag(lagfact),
         laglaglagfact = lag(laglagfact)) %>% 
  filter(laglaglagfact == 4,
         laglagfact == 4,
         lagfact == 4,
         nfactors == 4)

# Print the result
min(res$numbers) - 3
