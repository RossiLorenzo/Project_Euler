# If p is the perimeter of a right angle triangle 
# with integral length sides, {a,b,c}, there are exactly three
# solutions for p = 120.
# 
# {20,48,52}, {24,45,51}, {30,40,50}
# 
# For which value of p ≤ 1000, is the
# number of solutions maximised?

# Libraries
library(parallel)

# Let's consider a triangle with sides a <= b <= c
# The perimiter is a + b + c. 
# Since is right angle c^2 = a^2 + b^2
# So c = sqrt(a^2 + b^2) and p = a + b + sqrt(a^2 + b^2)
# Very easily p = a + b + sqrt(a^2 + b^2) <= 2a + 2b
# p <= 2a+2b <= 4b -> b >= p/4

# For every possible perimeter get possible sides
all_p = 8:1000
cores = makeCluster(10)
X = unlist(parLapplyLB(cores, all_p, function(p){
  library(dplyr)
  # Calculate possible a, b
  b_min = p/4
  a = seq(1, b_min, 1)
  b = seq(b_min, p - b_min)
  triangle = expand.grid(a, b)
  # Calculate c from a, b with c^2 = a^2 + b^2
  triangle$Var3 = sqrt(triangle$Var1^2 + triangle$Var2^2)
  # Check that permiter is right and that c is a perfect square
  triangle = filter(triangle, 
        Var3 + Var2 + Var1 == p, 
        round(Var3) == Var3)
  nrow(triangle)
}))
stopCluster(cores)

# Print results
all_p[which(X == max(X))]
