
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

plot(0,type='n',
     xlim=c(1,max(td$session_time)))
add_event('resp_max_key',.5)
add_event('resp_central_key',0)
add_event('resp_mel_key',-.5)
add_event('trial_start',.75,pch=16)