data_folder <- '/home/lab25/Documents/Luis/harvard_game/Experiment Pigeons/Data/'
results_folder <- '/home/lab25/Documents/Luis/harvard_game/Experiment Pigeons/Results/'

setwd(data_folder)
td <- read.csv('test_data.csv')
td
head(td)
unique(td$event)

# Required functions:
# add_puntual_event
# add_interval_event

add_event <- function(h_event,
                      y_coord,
                      ...) {
  ix_event <- which(td$event==h_event)
  points(td$session_time[ix_event],rep(y_coord,length(ix_event)),...)
}

interval_event <- function(event_name,
                           y_coord,
                           bw=.1,
                           color='#0055ff66') {
  
  # event_name <- 'max_light'
  # y_coord <- .5
  # band_width <- .1
  
  name_on <- paste(event_name,'_on',sep='')
  name_off <- paste(event_name,'_off',sep='')
  times_on <- td$session_time[which(td$event==name_on)]
  times_off <- td$session_time[which(td$event==name_off)]
  
  for(tt in 1:length(times_on)){
    polygon(x=c(rep(times_on[tt],2),rep(times_off[tt],2)),
            y=c(y_coord+c(+bw,-bw,-bw,+bw)/2),col=color)
    
  }
  
}
  
setwd(results_folder)
plot(0,type='n',
     xlim=c(1,max(td$session_time)))
interval_event('chamber_light',0,bw=2,color = '#ccee0044')
interval_event('central_light',0,color = '#00cc77cc')  
interval_event('mel_light',0.5,color = '#33cc00cc')  
interval_event('max_light',-0.5,color = '#33cc00cc')  
interval_event('feeder',0,color = '#ee1100cc')  
add_event('resp_max_key',.5)
add_event('resp_central_key',0)
add_event('resp_mel_key',-.5)
add_event('trial_start',.75,pch=16)
