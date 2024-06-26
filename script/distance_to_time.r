rm(list=ls())

distances<-data.frame(case = c("1","2","2","3","3","4","5"), 
                      relative = c("before","before", "after","before", "after","before","before"), 
                      distance = c(0.030927,0.002871885,0.007033073,0.003376,0.002090,0.001961,0.002667), 
                      MRCA_years_gen = NA, MRCA_years_rate = NA, case_time_days = c(5447,309,1212,178,758,299,1266))

for (i in 1:length(distances$case)) {
  distances$MRCA_years_gen[i]<-((((distances$distance[i]*11000)/0.17)*23)/365)/2
  distances$MRCA_years_rate[i]<-distances$distance[i]/0.000244/2
}

dist<-0.0012
sub_gen<-0.17
genome_length<-11000
serial_interval<-34

total_sub<-dist*11000

generations<-total_sub/0.17

time<-generations*34

time/365
