#include <Rcpp.h>
using namespace Rcpp;

#include <math.h>

// [[Rcpp::export]]
IntegerVector Eratosthenes_Sieve(int n) {
  
  IntegerVector values(n - 1);
  IntegerVector is_prime(n - 1);
  for(int i = 0; i < n - 1; i++){
    values[i] = i + 2;
    is_prime[i] = 0;
  }
    
  int curr_prime = 2;
  int sqrt_n = sqrt(n);
  int removed = 1;
  while(curr_prime <= sqrt_n){
    
    // Remove all dividends of curr_prime
      
    removed = 0;
    for(int i = curr_prime - 1; i < n - 1; i++){
      if(is_prime[i] == 0){
        if(values[i] % curr_prime == 0){
          is_prime[i] = 1;
          removed++; 
        }
      }
    }

    // If nothing is removed than we converged 
    if(removed == 0)
      break;
    
    // Update curr_prime
    for(int i = curr_prime - 1; i < n -1; i++)
      if(is_prime[i] == 0){
        curr_prime = values[i];
        break;
      }
  }
  
  return(values[is_prime == 0]);
}
