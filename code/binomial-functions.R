# ===================================================================
# Title: binomial function
# Description: This script performs code implementing the functions
# Input(s): NA
# Output(s):NA 
# Author(s): Minjeong Jeong
# Date: 03-21-2018
# ===================================================================

#Function is_integer()
# ===================================================================
#' @title is_integer
#' @description determine if a numeric value is an integer number  
#' @param x a single numeric value
#' @return is_integer() if it's integer, it returns TRUE 
# ===================================================================
is_integer<-function(x){
             if(is.numeric(x)){
                return(x%%1==0)
                }else{
               return(FALSE)}
}

#TRUE's
is_integer(-1)
is_integer(0)
is_integer(2L)
is_integer(2)
#FALSE's
is_integer(2.1)
is_integer(pi)
is_integer(0.01)

#Function is_positive()
# ===================================================================
#' @title is_poisitive
#' @description test if a numeric value is a positive number
#' @param x a sigle numeric value
#' @return is_positive() if the number is positive, return TRUE
# ===================================================================

is_positive<-function(x){
             if(x>0){
               return(TRUE)
             }else{
               return(FALSE)}
}

#TRUE's
is_positive(0.01)
is_positive(2)
#FALSE's
is_positive(-2)
is_positive(0)

#Function is_nonnegative()
# ===================================================================
#' @title is_nonnegative
#' @description test if a numeric value is a non-negative number
#' @param  x a single numeric value
#' @return is_nonnegative() if the number is non-negative,return TRUE
# ===================================================================

is_nonnegative<-function(x){
               if(x>=0){
                return(TRUE)
               }else{
                return(FALSE)}
}

#TRUE's
is_nonnegative(0)
is_nonnegative(2)
#FALSE's
is_nonnegative(-0.00001)
is_nonnegative(-2)

#Function is_positive_integer()
# ===================================================================
#' @title is_positive_integer
#' @description test if a numeric value is a positive integer number
#' @param  x a single numeric value
#' @return is_positive_integer if it's a positive integer number,TRUE
# ===================================================================

is_positive_integer<-function(x){
                   if(x>0){
                     return(x%%1 == 0)
                   }else{
                     return(FALSE)}
}

#TRUE's
is_positive_integer(2)
is_positive_integer(2L)
#FALSE's
is_positive_integer(0)
is_positive_integer(-2)


#Function is_nonneg_integer()
# ===================================================================
#' @title is_nonneg_integer
#' @description test if a numeric value is a nonnegative integer number
#' @param  x a single numeric value
#' @return is_nonneg_integer if it's a nonnegative integer number,TRUE
# ===================================================================

is_nonneg_integer<-function(x){
                   if(x>=0){
                    return(x%%1 == 0)
                   }else{
                     return(FALSE)}
}

#TRUE's
is_nonneg_integer(0)
is_nonneg_integer(1)
#FALSE's
is_nonneg_integer(-1)
is_nonneg_integer(-2.5)

#Function is_probability()
# ===================================================================
#' @title is_probability
#' @description test if a given number p is a valid probability
#' @param  p a probability 0<=p<=1
#' @return is_probability if the inputs is a valid probability,TRUE
# ===================================================================

is_probability<-function(p){
                if(0<=p & 1>=p){
                  return(TRUE)
                }else{
                   return(FALSE)}
}

#TRUE's
is_probability(0)
is_probability(0.5)
is_probability(1)
#FALSE's
is_probability(-1)
is_probability(1.0000001)

#Function bin_factorial()
# ===================================================================
#' @title bin_factorial
#' @description calculate the factorial of a nonnegative integer n
#' @param  n nonnegative integer
#' @return bin_factorial
# ===================================================================

bin_factorial<-function(n){
                if(n < 0) {
             stop("factorial does not exist for negative numbers")
                } else if(n == 0) {
                return(1)}
                y<-1
                for(k in 1:n){
                y<-y*k
                }
               return(y)
}

#valid
bin_factorial(5)
bin_factorial(0)

#Function bin_combiations()
# ===================================================================
#' @title bin_combination
#' @description calculate the number of combinations 
#'              where k successes can occur in n trials
#' @param  n nonnegative integer
#' @param  k a variable that represents the number of success 
#' @return bin_combinations
# ===================================================================

bin_combinations<-function(n,k){
                  c<-bin_factorial(n)/(bin_factorial(k)*(bin_factorial(n-k)))
                  return(c)}

#valid
bin_combinations(5,2)
bin_combinations(10,3)
bin_combinations(4,4)

#Function bin_probability()
# ===================================================================
#' @title bin_probability
#' @description calculate probability of getting s sucesses in t trials  
#' @param  t trial
#' @param  s success
#' @param  p probability
#' @return bin_probability 
# ===================================================================

bin_probability <- function(t,s,p){
                    if(!is_nonneg_integer(t)){
                      stop('Trials must be a positive-integer number')}   
                    if(!is_nonneg_integer(s)){
                      stop('Success must be a positive-integer number')}
                    if(!is_probability(p)){
                      stop('probability must be between 0 and 1')}
                    prob<-bin_combinations(t,s)*(p^s)*((1-p))^(t-s)
                    return(prob)
                    }

#valid
bin_probability(5,2,0.5)

#Function bin_distribution()
# ===================================================================
#' @title bin_distribution
#' @description create distribution function
#' @param  t trial
#' @param  p probability
#' @return bin_distribution
# ===================================================================


bin_distribution <- function(t, p) {
  probability <- rep(0,t+1)
  success <- 0:t
  for(i in 0:t) {
    probability[i+1] <- bin_probability(t, i, p)
  }
  return(data.frame(success, probability))
}

bin_distribution(5,0.5)