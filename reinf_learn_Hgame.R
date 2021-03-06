# Simulating reinforcement learning models playing against the Hgame
# jlbm
# feb 2017

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
      pos <<- pos-1
    }
  }
  else if(choice=='long_term'){
    result <- possible_outcomes[rbinom(1,size=1,prob=lt_parameters[pos])+1]
    if(pos!=11){
      pos <<- pos+1
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

wsls_results <- array(dim=c(sims_per_parameter,n_trials))
wsls_choices <- array(dim=c(sims_per_parameter,n_trials))
wsls_states <- array(dim=c(sims_per_parameter,n_trials))
# rndm_results <- array(dim=c(sims_per_parameter,n_trials))
# rndm_choices <- array(dim=c(sims_per_parameter,n_trials))
# rndm_states <- array(dim=c(sims_per_parameter,n_trials))

for(spp in 1:sims_per_parameter){
  pos <- 6 # starting indexing of alternatives' vectors
  previous_choice_wsls <- NULL
  previous_result_wsls <- NULL
  for(trial in 1:n_trials){
    wsls_states[spp,trial] <- pos
    wsls_choices[spp,trial] <- wsls(previous_choice=previous_choice_wsls,
                                    previous_result=previous_result_wsls,
                                    trial=trial,theta=1)
    # rndm_choices[spp,trial] <- rndm(theta=.8)
    wsls_results[spp,trial] <- harvard_game(wsls_choices[spp,trial]) # Careful with global variable 'pos'
    # rndm_results[spp,trial] <- harvard_game(rndm_choices[spp,trial]) # ibid.
    previous_choice_wsls <- wsls_choices[spp,trial]
    previous_result_wsls <- wsls_results[spp,trial]
  }
}


results <- wsls_results[1,]
choices <- wsls_choices[1,]
states <- wsls_states[1,]
# alternative <- 'short_term'
# results
# choices
# 
# t <- 2
# alternative_X <- 'short_term'
# alternative_Y <- 'long_term'
# alpha <- NULL
# beta <- NULL
# gamma <- NULL
# delta <- NULL
# for(t in 2:n_trials){
#   alpha[t] <- choices[(t-1)]==alternative_X&choices[t]==alternative_X&results[(t-1)]=='win'
#   beta[t] <- choices[(t-1)]==alternative_X&choices[t]==alternative_Y&results[(t-1)]=='loss'
#   gamma[t] <- choices[(t-1)]==alternative_Y&choices[t]==alternative_Y&results[(t-1)]=='win'
#   delta[t] <- choices[(t-1)]==alternative_Y&choices[t]==alternative_X&results[(t-1)]=='loss'
# }


add_tseries <- function(results_array,choices_array,states_array,alternative){
  for(i in 1:dim(results_array)[1]){
    results <- results_array[i,]
    choices <- choices_array[i,]
    states <- states_array[i,]
    # alternative <- 'long_term'
    
    state_colors <- rainbow(11,alpha=.4)
    for(stt in 1:11){
      stt_indx <- which(states==stt&choices==alternative)
      
      xx <- stt_indx
      yy <- cumsum(results[stt_indx]=='win')/cumsum(choices==alternative&states==stt)[stt_indx]
      
      points(xx,yy,type='l',lwd=.2,col=state_colors[stt])
    }
    
    
  }
}

add_cumrec <- function(results_array,choices_array,alternative){
  
  for(i in 1:dim(results_array)[1]){
    results <- results_array[i,]
    choices <- choices_array[i,]
    lines(1:sum(choices==alternative),
          cumsum(results[which(choices==alternative)]=='win'))
    lines(c(sum(choices==alternative),n_trials),
          rep(sum(results[which(choices==alternative)]=='win'),2),
          col='#cccccc')
  }
  
}

add_win_margin <- function(results_array,choices_array,alternative){
  for(i in 1:dim(results_array)[1]){
    results <- results_array[i,]
    choices <- choices_array[i,]
    lines(c(-0.5,0),
          rep(sum(results[which(choices==alternative)]=='win'),2),
          col='#cccccc')
    lines(c(0,1),
          c(sum(choices==alternative&results=='win'),
            sum(choices==alternative&results=='loss')))
  }
}

add_states_series <- function(states_array){
  for(i in 1:dim(states_array)[1]){
    lines(1:n_trials,states_array[i,],
          lwd=1,col='#00000022')
  }
}

add_states_margin <- function(states_array) {
  for(i in 1:dim(states_array)[1]){
    lines(as.numeric(table(states_array[i,])),
          as.numeric(names(table(states_array[i,]))),
          lwd=1.5,col='#00000055')
  }
}


display_alternative <- function(alternative) {
  
  par(mar=c(5,6,4,1))
  plot(0,type='n',xlim=c(1,n_trials),ylim=c(0,1),ann=F,axes=F)
  lines(c(1,n_trials),rep(0.5,2),lty='dashed')
  polygon(c(1,n_trials,n_trials,1),c(rep(0,2),rep(1,2)),border=F,col='#000000')
  add_tseries(wsls_results,wsls_choices,wsls_states,alternative)
  axis(1,at=c(1,seq(200,n_trials,200)))
  axis(2,at=c(0,1))
  mtext(paste('proportion of wins at ',alternative),2,line=3)
  mtext('trials',1,line=3)
  mtext(alternative,2,line=5,cex=1.5)
  
  par(mar=c(5,4,4,0))
  plot(0,type='n',xlim=c(1,n_trials),ylim=c(1,n_trials),ann=F,axes=F)
  abline(0,1,lty='dashed')
  add_cumrec(wsls_results,wsls_choices,alternative)
  axis(2,at=c(1,seq(400,n_trials,400)))
  axis(1,at=c(1,seq(400,n_trials,400)))
  mtext(paste('cumulative',alternative),1,line=3)
  mtext(paste('cumulative wins at ',alternative),2,line=3)
  
  par(mar=c(5,0,4,2))
  plot(0,type='n',xlim=c(-.5,1.5),ylim=c(0,n_trials),ann=F,axes=F)
  add_win_margin(wsls_results,wsls_choices,alternative)
  axis(1,at=c(0,1),labels=c('wins','losses'))
  
  par(mar=c(5,6,4,1))
  plot(0,type='n',xlim=c(1,400),ylim=c(1,11),ann=F,axes=F)
  add_states_series(wsls_states)
  axis(1,at=c(1,seq(200,n_trials,200)))
  axis(2,at=1:11)
  mtext('Hgame states ',2,line=3)
  mtext('trials',1,line=3)
  
  par(mar=c(5,4,4,2))
  plot(0,type='n',xlim=c(1,n_trials),ylim=c(1,11),ann=F,axes=F)
  add_states_margin(wsls_states)
  axis(1,at=c(1,seq(400,n_trials,400)))
  axis(2,at=1:11)
  mtext('game states',2,line=3)
  mtext('# of visits',1,line=3)
}

cumcum_alternatives <- function(choices) {
  plot(0,type='n',xlim=c(1,1500),ylim=c(1,1500),ann=F)
  abline(0,1,lty='dashed',col='#ee0000',lwd=2)
  for(k in 1:dim(choices)[1]){
    lines(cumsum(choices[k,]=='short_term'),
          cumsum(choices[k,]!='short_term'),
          lwd=3,col='#00000011')
  }
  mtext('cumulative long term choices',2,line=3)
  mtext('cumulative short term choices',1,line=3)
}

cumcum_results <- function(results) {
  plot(0,type='n',xlim=c(1,2000),ylim=c(1,2000),ann=F)
  abline(0,1,lty='dashed',col='#ee0000',lwd=2)
  for(k in 1:dim(results)[1]){
    lines(cumsum(results[k,]=='win'),
          cumsum(results[k,]!='win'),
          lwd=3,col='#00000011')
  }
  mtext('cumulative losses',2,line=3)
  mtext('cumulative wins',1,line=3)
}

pdf(file='wsls_vs_Hgame.pdf',width=22,height=6.5)
layout(cbind(matrix(1:10,ncol=5,byrow=T),11:12),
       widths=c(1.5,1,.5,1.5,1,1))
par(oma=c(0,2,0,0))
display_alternative('short_term')
display_alternative('long_term')
cumcum_alternatives(wsls_choices)
cumcum_results(wsls_results)
dev.off()
