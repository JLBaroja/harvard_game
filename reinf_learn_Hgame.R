
prev_choices <- 0:10
lt_parameters <- (2/3)*prev_choices/10
st_parameters <- (1/3)+(2/3)*prev_choices/10

plot(lt_parameters,ylim=c(0,1),pch=21,bg='black',type='o',cex=2)
points(st_parameters,pch=21,bg='white',type='o',cex=2)


havard_game <- function(choice) {
  
  if(choice=='short_term'){
    result <- rbinom(1,size=1,prob=st_parameters[pos])
    if(pos!=1){
      pos <- pos-1
    }
  }
  else if(choice=='long_term'){
    result <- rbinom(1,size=1,prob=lt_parameters[pos])
    if(pos!=11){
      pos <- pos+1
    }
  }
  
  return(result)
}


wsls <- function(previous_choice,
                 previous_result,
                 trial,
                 theta) {

  if(trial!=1){
    if(previous_result=='win'){
      next_choice <- previous_choice
    }
    else{
      next_choice <- possible_choices[which(possible_choices!=previous_choice)]
    }
  }  
  else{
    next_choice <- rbinom(1,size=1,prob=theta)
  }
  
}


# previous_choice <- 'short_term'

# Global Variables
possible_choices <- c('short_term','long_term')
pos <- 5 # starting indexing of alternatives' vectors






