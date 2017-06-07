rm(list=ls())
data_folder <- '/home/lab25/Documents/Luis/harvard_game/Experiment Pigeons/Data/'


setwd(data_folder)
td <- read.csv('p498.csv')
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
                           bw=.07,
                           lab_on='_on',
                           lab_off='_off',
                           color='#0055ff66') {
  
  # event_name <- 'max_light'
  # y_coord <- .5
  # band_width <- .1
  
  name_on <- paste(event_name,lab_on,sep='')
  name_off <- paste(event_name,lab_off,sep='')
  times_on <- td$session_time[which(td$event==name_on)]
  times_off <- td$session_time[which(td$event==name_off)]
  
  for(tt in 1:length(times_on)){
    polygon(x=c(rep(times_on[tt],2),rep(times_off[tt],2)),
            y=c(y_coord+c(+bw,-bw,-bw,+bw)/2),col=color)
    
  }
  
  add_event(name_on,y_coord=y_coord+bw*.8,pch=25,bg=color) 
  add_event(name_off,y_coord=y_coord-bw*.8,pch=24,bg=color) 
  
  text(-max(td$session_time)*.01,y_coord,event_name,adj=1,cex=1.5)
}


# pdf(file='test_results.pdf',width=15,height=5)
# dev.new(width=5,height=3)
dev.off()
x11(width=20,height=7)
y_lims <- c(-2,1)
par(bg='#cccccc',col.axis='#555555',fg='#333333',
    mar=c(3,1,3,1))
plot(0,type='n',
     xlim=c(-max(td$session_time)*.07,
            max(td$session_time)),
     ylim=c(-2,1),
     ann=F,axes=F)
axis(1,at=0:ceiling(max(td$session_time)),cex.axis=.6)
interval_event('trial',.9,
               lab_on = '_start',
               lab_off = '_end',color = '#bbbbbb')
interval_event('chamber_light',.7,color = '#ccee0044')
interval_event('central_light',0,color = '#00cc77cc')  
interval_event('mel_light',0.4,color = '#33cc00cc')  
interval_event('max_light',-0.4,color = '#33cc00cc')  
interval_event('feeder',-.85,color = '#ee7700cc')  
add_event('resp_max_key',.4,pch=4,cex=1.5,col='#ee0000')
add_event('resp_central_key',0,pch=4,cex=1.5,col='#ee0000')
add_event('resp_mel_key',-.4,pch=4,cex=1.5,col='#ee0000')

z_labels <- c('(to trial start)',
              '(to wait for peck)',
              '(to feeder)',
              '(to next trial)',
              '(to turn lights off)',
              '(to trial result)')
for(zz in 1:6){
  y_crd <- seq(-1.2,-1.9,length.out = 6)[zz]
  ev_nm <- paste('z00',zz,sep='')
  add_event(ev_nm,
            y_coord = y_crd,
            pch=23,bg='#9922eedd')
  text(-max(td$session_time)*.01,y_crd,
       paste(z_labels[zz],ev_nm),adj=1,cex=1)
}
# add_event('trial_start',.75,pch=24)
# add_event('trial_end',.7,pch=25)
# add_event('mel_light_on',.4,pch=24,bg='#33cc00cc')
# add_event('mel_light_off',.35,pch=25,bg='#33cc00cc')
# add_event('max_light_on',-.6,pch=24,bg='#33cc00cc')
# add_event('max_light_off',-.65,pch=25,bg='#33cc00cc')
# add_event('_on',,pch=)
# add_event('_off',,pch=)
# add_event('_on',,pch=)
# add_event('_off',,pch=)
segments(x0=0:ceiling(max(td$session_time)),
         x1=0:ceiling(max(td$session_time)),
         y0=rep(y_lims[1],ceiling(tail(td$session_time,1))),
         y1=rep(y_lims[2],ceiling(tail(td$session_time,1))),lwd=.5,col='#0088ee22')
# dev.off()