

prev_choices <- 0:10
lt_parameters <- (2/3)*prev_choices/10
st_parameters <- (1/3)+(2/3)*prev_choices/10

# plot(lt_parameters,ylim=c(0,1),pch=21,bg='black',type='o',cex=2)
# points(st_parameters,pch=21,bg='white',type='o',cex=2)



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
                                    trial=trial,theta=1)
    rndm_choices[spp,trial] <- rndm(theta=.8)
    wsls_results[spp,trial] <- harvard_game(wsls_choices[spp,trial])
    rndm_results[spp,trial] <- harvard_game(rndm_choices[spp,trial])
    previous_choice_wsls <- wsls_choices[spp,trial]
    previous_result_wsls <- wsls_results[spp,trial]
  }
}


# results <- wsls_results[1,]
# choices <- wsls_choices[1,]
# alternative <- 'short_term'
# results
# choices


add_tseries <- function(results_array,choices_array,alternative){
  for(i in 1:dim(results_array)[1]){
    results <- wsls_results[i,]
    choices <- wsls_choices[i,]
    
    points(which(choices==alternative),
           (cumsum(choices==alternative&results=='win')/cumsum(choices==alternative))[choices==alternative],
           type='l',lwd=1,col='#00000015')
    
  }
}

add_cumrec <- function(results_array,choices_array,alternative){
  
  for(i in 1:dim(results_array)[1]){
    results <- wsls_results[i,]
    choices <- wsls_choices[i,]
    lines(1:sum(choices==alternative),
          cumsum(results[which(choices==alternative)]=='win'))
    lines(c(sum(choices==alternative),n_trials),
          rep(sum(results[which(choices==alternative)]=='win'),2),
          col='#cccccc')
    }
  
}

add_winmargin <- function(results_array,choices_array,alternative){
  for(i in 1:dim(results_array)[1]){
    results <- wsls_results[i,]
    choices <- wsls_choices[i,]
    lines(c(-0.5,0),
          rep(sum(results[which(choices==alternative)]=='win'),2),
          col='#cccccc')
    lines(c(0,1),
          c(sum(choices==alternative&results=='win'),
            sum(choices==alternative&results=='loss')))
  }
}

display_alternative <- function(alternative) {
  
  par(mar=c(5,6,4,1))
  plot(0,type='n',xlim=c(1,n_trials),ylim=c(-.25,1.25),ann=F,axes=F)
  lines(c(1,n_trials),rep(0.5,2),lty='dashed')
  add_tseries(wsls_results,wsls_choices,alternative)
  axis(1,at=c(1,seq(200,n_trials,200)))
  axis(2,at=c(0,1))
  mtext(paste('proportion of wins at ',alternative),2,line=2)
  mtext('trials',1,line=3)
  mtext(alternative,2,line=4,cex=1.5)
  
  par(mar=c(5,4,4,0))
  plot(0,type='n',xlim=c(1,n_trials),ylim=c(1,n_trials),ann=F,axes=F)
  add_cumrec(wsls_results,wsls_choices,alternative)
  axis(2,at=c(1,seq(400,n_trials,400)))
  axis(1,at=c(1,seq(400,n_trials,400)))
  mtext(paste('cumulative',alternative),1,line=3)
  mtext(paste('cumulative wins at ',alternative),2,line=2)
  
  par(mar=c(5,0,4,2))
  plot(0,type='n',xlim=c(-.5,1.5),ylim=c(0,n_trials),ann=F,axes=F)
  add_winmargin(wsls_results,wsls_choices,alternative)
  axis(1,at=c(0,1),labels=c('wins','losses'))
  
}


pdf(file='wsls_sim_Hgame.pdf',width=11,height=8)
layout(matrix(1:6,ncol=3,byrow=T),widths=c(2,1.5,1))
display_alternative('short_term')
display_alternative('long_term')
dev.off()
