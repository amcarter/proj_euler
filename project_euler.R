# Project Euler
# 11/20/2020

# problem 5 ####


# problem 4 ####
int_rev <- function(s){
  s = as.character(s)
  s = strsplit(s, '')[[1]] %>%
    rev() %>%
    paste(collapse = '')
  
  return(s)
}



for(n in 997:100){
  nn <- as.numeric(paste0(n, int_rev(n)))
  for(d in 999:100){
    r <- nn %% d
    if(r == 0){
      if(nchar(nn/d) < 3) next else break
    }
  }
  if(nchar(nn/d) == 3){ break }
}




# problem 3 ####
largest_prime_factor <- function(n){
  
  p <- 2  
  while(n > p){
    r <- n %% p
    if(r == 0){
      n <- n/p
    } else {
      p <- p + 1
    }
  }
  
  return(n)
}
