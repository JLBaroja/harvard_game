
prev_choices <- 0:10
lt_parameters <- (2/3)*prev_choices/10
st_parameters <- (1/3)+(2/3)*prev_choices/10

plot(lt_parameters,ylim=c(0,1),pch=21,bg='black',type='o',cex=2)
points(st_parameters,pch=21,bg='white',type='o',cex=2)



harvard_game <- function(choice) {
  possible_outcomes <- c('loss','win')
  
  if(choice=='short_term'){
    result <- possible_outcomes[rbinom(1,size=1,prob=st_parameters[pos])+1]
    if(pos!=1){
      pos <- pos-1
    }
  }
  else if(choice=='long_term'){
    result <- possible_outcomes[rbinom(1,size=1,prob=lt_parameters[pos])+1]
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
      next_choice <- sample(c(previous_choice,possible_choices[which(possible_choices!=previous_choice)]),
                            size=1,prob=c(theta,1-theta))
    }
    else if(previous_result=='loss'){
      next_choice <- sample(c(previous_choice,possible_choices[which(possible_choices!=previous_choice)]),
                            size=1,prob=c(1-theta,theta))
    }
  }  
  else{
    next_choice <- possible_choices[rbinom(1,size=1,prob=theta)+1]
  }
  
  return(next_choice)
}



rndm <- function(theta){
  next_choice <- possible_choices[rbinom(1,size=1,prob=theta)+1]
  return(next_choice)
}



# previous_choice <- 'short_term'


# Global Variables
n_trials <- 2000
sims_per_parameter <- 100
possible_choices <- c('short_term','long_term')
pos <- 5 # starting indexing of alternatives' vectors

wsls_results <- array(dim=c(sims_per_parameter,n_trials))
wsls_choices <- array(dim=c(sims_per_parameter,n_trials))
rndm_results <- array(dim=c(sims_per_parameter,n_trials))
rndm_choices <- array(dim=c(sims_per_parameter,n_trials))

for(spp in 1:sims_per_parameter){
  previous_choice_wsls <- NULL
  previous_result_wsls <- NULL
  for(trial in 1:n_trials){
    wsls_choices[spp,trial] <- wsls(previous_choice=previous_choice_wsls,
                                    previous_result=previous_result_wsls,
                                    trial=trial,theta=.8)
    rndm_choices[spp,trial] <- rndm(theta=.8)
    wsls_results[spp,trial] <- harvard_game(wsls_choices[spp,trial])
    rndm_results[spp,trial] <- harvard_game(rndm_choices[spp,trial])
    previous_choice_wsls <- wsls_choices[spp,trial]
    previous_result_wsls <- wsls_results[spp,trial]
  }
}





