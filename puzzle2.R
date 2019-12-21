# puzzle 2

int_comp <- function(vector){
  i = 1
  opt <- vector[1]
  
  while (opt %in% c(1, 2)) {
    
    input1 = vector[vector[i+1] + 1] # adjust for 0 indexing
    input2 = vector[vector[i+2] + 1] # adjust for 0 indexing
    pos = vector[i+3] + 1 # adjust for 0 indexing
  
    if(opt == 1){
    vector[pos] <- sum(input1, input2)
  } 
    
    
  if(opt == 2){
    vector[pos] <-  prod(input1, input2)
  }
    
  i = i + 4
  
  opt <- vector[i]
  }
  
  if(!is.na(opt) & opt != 99) stop( "Somthing went wrong...", call. = F)
  
  return(vector) 
  
} 

expect_equal(int_comp(c(1,0,0,0,99)), c(2,0,0,0,99))
expect_equal(int_comp(c(2,3,0,3,99)), c(2,3,0,6,99))
expect_equal(int_comp(c(2,4,4,5,99,0)), c(2,4,4,5,99,9801))
expect_equal(int_comp(c(1,1,1,4,99,5,6,0,99)), c(30,1,1,4,2,5,6,0,99))

int_comp(c(1,1,1,4,99,5,6,0,99))


input2 <- read_lines("input2.txt") %>%  str_split(.,",", simplify = T) %>% as.integer()
program1202 <- input2 
program1202[2] <- 12
program1202[3] <- 2


int_comp(program1202)[1]


### part 2! 
# ugly brute force


moonlander <- function(noun, verb, vector = input2){
  vector[2] <- noun
  vector[3] <- verb
  
  int_comp(vector)[1]
}

moonlander(12, 2)


for(i in 0:100){
  for(j in 0:100){
    result <- moonlander(i, j)
    
    if(result == 19690720) print(c(i, j))
    
  }
}
# 37, 49

100*37+49


