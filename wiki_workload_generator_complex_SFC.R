# This workload generator creates a workload for SFC experiment.

# Arrival rate: following the Wikipedia trace.
# Network worklaod: each pakcet has the same constant packet size, to simplify the network simulation
# CPU workload: each workload has the same CPU size.

# We assumed that Load Balancer already worked, so that the workload is assigned in round-robin.
# Two kinds of workoad
# 1. Cached workload which only arrives at Web server, no access to App or DB.
#    This implies the Web server had a cached data.
# 2. Traditional worklaod that passes Web, App, and DB servers.

library(actuar)
library(pracma)
B=1
KB=B*1024
MB=KB*1024

MAX_CPU_MI = 20000 #400 MIs = 0.2 sec for 2000 MIPS vm


options(scipen=500) #instead of E notation, use full representation.

# Parameters for history based workloads
NETWORK_PACKET_SIZE_MULTIPLY = 1
CPU_MULTIPLY  = 1
NUM_WORKLOAD_RATE_SCALE = 1

#average FRONT(web) MID(app) BACK(bd)
PACKETSIZE_FRONT_MID = 1500
PACKETSIZE_MID_BACK = 1500
PACKETSIZE_BACK_MID = 65000
PACKETSIZE_MID_FRONT = 65000

#PACKETSIZE_FRONT_MID = 500
#PACKETSIZE_MID_BACK = 500
#PACKETSIZE_BACK_MID = 22800
#PACKETSIZE_MID_FRONT = 345800

CPUSIZE_FRONT_1 = 72*CPU_MULTIPLY
CPUSIZE_MID_1 = 585*CPU_MULTIPLY
CPUSIZE_BACK_1 = 306*CPU_MULTIPLY
CPUSIZE_MID_2 = 67*CPU_MULTIPLY
CPUSIZE_FRONT_2 = 68*CPU_MULTIPLY

rlnorm2 <-function(n,u,s) {
  r = rnorm(n, mean=u, sd=s)
  return(2^r)
}

generate_atimes_lnorm<-function(n,u,s, scale=1000) {
  r <- rlnorm2(n, u, s)/scale
  #cat("median inter-arrival time:",  median(r),", throughput:", 1/mean(r),"req/sec\n")
  return(round(cumsum(r), digits=4))
}


generate_atimes_dur<-function(u,s, target.rate = -1, dur=1, scale=1) {
  n = target.rate*dur*1.5
  
  times = generate_atimes_lnorm(n,u,s,scale)
  if(target.rate != -1) {
    # Spread out the time.
    throughput = n / tail(times,n=1)
    ratio = throughput / target.rate
    times = times * ratio
  }
  
  times=times[times<=dur]
  
  #cat("Adjusted throughput=",  n / tail(times,n=1),"req/sec\n")
  
  return(round(times, digits=4))
}


generate_repeat<-function(n,mean) {
  sd = mean*0.2
  r <- rnorm(n, mean, sd)
  return(round(r))
}


generate_packet_size<-function(n,u,s, scale) {
  r <- rlnorm2(n, u, s) * scale
  cat("median packet size:",  median(r), "\n")
  
  return(round(r))
}

generate_cpu_size<-function(n, location, shape, vm_cap=1) {
  #vm_cap = vm_cap*SCALE_FACTOR
  
  r = rpareto(n*2, shape=shape, scale=location)
  r = round(r*vm_cap)
  r = r[r<MAX_CPU_MI]
  while(length(r) < n) {
    rn = rpareto(n, shape=shape, scale=location)
    rn = round(rn*vm_cap)
    r = c(r, rn[rn<MAX_CPU_MI])
  }
  r=r[1:n]
  #cat("cpu workloads -- median size: ", median(r), ", max:", max(r), ", mean:", mean(r),"\n" )
  return(r)
}

combine_works<-function(atime, name.1, name.2, name.3, 
                            w.1.1, p.1.2, link.1.2,
                            w.2.1, p.2.3, link.2.3, 
                            w.3, p.3.2,  link.3.2,
                            w.2.2, p.2.1,  link.2.1,
                            w.1.2) {
  #cat("length of atime=", length(atime), "\n")
  #atime=aggregate_first(atime, aggregate_num)
  #cat("aggregated to=", length(atime), "\n")
  
  "
  w.1.1=aggregate_sum(w.1.1, aggregate_num)
  p.1.2=aggregate_sum(p.1.2, aggregate_num) 
  w.2.1=aggregate_sum(w.2.1, aggregate_num) 
  p.2.3=aggregate_sum(p.2.3, aggregate_num)
  w.3=aggregate_sum(w.3, aggregate_num)
  p.3.2=aggregate_sum(p.3.2, aggregate_num)
  w.2.2=aggregate_sum(w.2.2, aggregate_num)
  p.2.1=aggregate_sum(p.2.1, aggregate_num)
  w.1.2=aggregate_sum(w.1.2, aggregate_num)
  "


  zeros= rep(0, length(atime))
  
  wl.0.1=cbind(          name.1, zeros, w.1.1)
  wl.1.2=cbind(link.1.2, name.2, p.1.2, w.2.1)
  wl.2.3=cbind(link.2.3, name.3, p.2.3, w.3)
  wl.3.2=cbind(link.3.2, name.2, p.3.2, w.2.2)
  wl.2.1=cbind(link.2.1, name.1, p.2.1, w.1.2)
  
  workload = cbind(atime, wl.0.1, wl.1.2, wl.2.3, wl.3.2, wl.2.1)
  return(workload)
}

# This function aggregates the number of workloads into a single workload by summing the aggregating members
aggregate_sum<-function(vec, membernum) {
  if(membernum == 1)
    return(vec)
  
  n = length(vec)
  k = ceiling(n/membernum)
  summed = tapply(vec,as.integer(gl(k,membernum, n)),sum)

  return(as.vector(summed))
}

# This function aggregates the number of workloads into a single workload by choosing the first member among aggregating member
aggregate_first<-function(vec, membernum) {
  n = length(vec)
  k = ceiling(n/membernum)
  summed = tapply(vec,as.integer(gl(k,membernum, n)),head, n=1)
  
  return(as.vector(summed))
}

create_workload<-function(n = -1, durtime = -1, 
                          target.rate=-1, starttime=0, seed = 10, filename="workloads.csv", 
                          name1set, name2set, name3set, 
                          is.dedicated=FALSE, is.append = FALSE) {
  
  #cat("=== Generating workload.. Start=",starttime,", Target rate =",target.rate, ", n=",n,", dur=",durtime,"\n")
  
  #WebServer      tail_index location K-S Test Statistic
  #RUBiS, 800 Clients 0.3164 21.364 0.02941
  #RUBiS, 1600 Clients 0.5456 22.630 0.03647
  #RUBiS, 2400 Clients 0.6423 28.065 0.04529
  #RUBiS, 3200 Clients 1.1108 67.729 0.03317
  #SPEC, txRate=10 0.6271 24.432 0.06182
  #SPEC, txRate=20 0.8202 99.031 0.05742
  #SPEC, txRate=40 1.1580 614.98 0.05911
  
  #AppServer      tail_index location K-S Test Statistic
  #RUBiS, 800 Clients 0.1030 24.6649 0.03554
  #RUBiS, 1600 Clients 0.3860 25.2826 0.04398
  #RUBiS, 2400 Clients 0.4642 29.8927 0.05172
  #RUBiS, 3200 Clients 0.9822 67.8342 0.04439
  #SPEC, txRate=10 0.5838 24.4444 0.02021
  #SPEC, txRate=20 0.8995 41.2255 0.06374
  #SPEC, txRate=40 1.2331 534.159 0.02218
  
  #DBServer      tail_index location K-S Test Statistic
  #RUBiS, 800 Clients 0.9296 6.7933 0.04681
  #RUBiS, 1600 Clients 1.0264 13.1436 0.03266
  #RUBiS, 2400 Clients 1.2391 23.3437 0.03541
  #RUBiS, 3200 Clients 1.4648 49.2665 0.06184
  #SPEC, txRate=10 0.9713 12.3486 0.05922
  #SPEC, txRate=20 1.1673 34.6912 0.08214
  #SPEC, txRate=40 1.2791 82.5146 0.07734
  ####################################################
  # For SPEC tx=10
  t.user.front.mean = 1.56270
  t.user.front.sd = 1.5458

  #t.front.mid.mean = 2.0815
  #t.front.mid.sd = 1.1506
  #t.mid.back.mean = 1.0354
  #t.mid.back.sd = 0.4157
  
  w.back.location = 12.3486
  w.back.shape = 0.9713
  #w.front=generate_cpu_size(n,24.432, 0.6271)
  #w.mid=generate_cpu_size(n,24.4444,0.5838)
  
  psize.front.mid.mean = 5.6129
  psize.front.mid.sd = 0.1343
  
  psize.mid.back.mean = 4.6455
  psize.mid.back.sd = 0.8013
  
  psize.back.mid.mean = 3.6839
  psize.back.mid.sd = 0.8261
  
  psize.mid.front.mean = 7.0104
  psize.mid.front.sd = 0.8481
  
  #psize.front.user.mean = 7.01865
  #psize.front.user.sd = 0.4894
  ####################################################
  
  ####################################################
  # For RUBIS 3200 client
  t.user.front.mean = 0.43154
  t.user.front.sd = 1.1225
  
  #t.front.mid = generate_atimes_wei(n, 0.03548, 0.24606)
  #t.mid.back.mean = 1.6770
  #t.mid.back.sd = 0.8912
  w.web.location = 67.729
  w.web.shape = 1.1108

  w.app.location = 67.8342
  w.app.shape = 0.9822

  w.db.location = 49.2665
  w.db.shape = 1.4648
  
  psize.front.mid.mean = 5.5974
  psize.front.mid.sd = 0.1602
  
  psize.mid.back.mean = 4.7053
  psize.mid.back.sd = 0.4034
  
  psize.back.mid.mean = 2.2134
  psize.back.mid.sd = 0.5126
  
  psize.mid.front.mean = 5.6438
  psize.mid.front.sd = 0.5436
  
  #psize.front.user.mean = 5.06502
  #psize.front.user.sd = 0.5458
  ####################################################


  w.1.1.location = w.1.2.location = w.web.location
  w.2.1.location = w.2.2.location = w.app.location
  w.3.location = w.db.location
  
  w.1.1.shape = w.1.2.shape = w.web.shape
  w.2.1.shape = w.2.2.shape = w.app.shape
  w.3.shape = w.db.shape

  scale_factor_net = NETWORK_PACKET_SIZE_MULTIPLY

  t.user.front.mean = 0.59481
  t.user.front.sd = 1.5284
  
  set.seed(seed)
  if( durtime != -1) {
    t.user.front = generate_atimes_dur(t.user.front.mean, t.user.front.sd, target.rate/durtime, dur=durtime)
    n=length(t.user.front)
  } else {
    t.user.front = generate_atimes(n, t.user.front.mean, t.user.front.sd, target.rate)
    durtime = tail(t.user.front, n=1)
  }
  #cat("=== Generating workload.. Start=",starttime,", Target rate =",target.rate, ", n=",n,", dur=",durtime,"\n")
  
  if(n == 0L)
    return(NA)
  t.user.front = t.user.front + starttime

  # Option 1: Use a constant size.
  psize.front.mid = round(generate_repeat(n, PACKETSIZE_FRONT_MID)*scale_factor_net)
  psize.mid.back  = round(generate_repeat(n, PACKETSIZE_MID_BACK)*scale_factor_net)
  psize.back.mid  = round(generate_repeat(n, PACKETSIZE_BACK_MID)*scale_factor_net)
  psize.mid.front = round(generate_repeat(n, PACKETSIZE_MID_FRONT)*scale_factor_net)
  "
  w.1.1 = generate_repeat(n=n, CPUSIZE_FRONT_1)
  w.2.1 = generate_repeat(n=n, CPUSIZE_MID_1)
  w.3   = generate_repeat(n=n, CPUSIZE_BACK_1)
  w.2.2 = generate_repeat(n=n, CPUSIZE_MID_2)
  w.1.2 = generate_repeat(n=n, CPUSIZE_FRONT_2)
  "

  # Option 2: Generate new workloads
  "
  set.seed(seed+1)
  psize.front.mid = generate_packet_size(n, psize.front.mid.mean, psize.front.mid.sd, KB)*scale_factor_net

  set.seed(seed+2)
  psize.mid.back =  generate_packet_size(n, psize.mid.back.mean, psize.mid.back.sd, KB)*scale_factor_net

  set.seed(seed+3)
  psize.back.mid =  generate_packet_size(n, psize.back.mid.mean, psize.back.mid.sd, KB)*scale_factor_net

  set.seed(seed+4)
  psize.mid.front =  generate_packet_size(n, psize.mid.front.mean, psize.mid.front.sd, KB)*scale_factor_net
  #psize.front.user = generate_packet_size(n, psize.front.user.mean, psize.front.user.sd, KB)

  "
  
  set.seed(seed+5)
  w.1.1=generate_cpu_size(n=n, location=w.1.1.location, shape=w.1.1.shape)
  set.seed(seed+6)
  w.1.2=generate_cpu_size(n=n, location=w.1.2.location, shape=w.1.2.shape)
  set.seed(seed+7)
  w.2.1=generate_cpu_size(n=n, location=w.2.1.location, shape=w.2.1.shape)
  set.seed(seed+8)
  w.2.2=generate_cpu_size(n=n, location=w.2.2.location, shape=w.2.2.shape)
  set.seed(seed+9)  
  w.3=generate_cpu_size(n=n, location=w.3.location, shape=w.3.shape)
  
  # Prepare server names (webX-Y, appX-Y, dbX-Y)
  name1=rep_len(name1set, n)
  name2=rep_len(name2set, n)
  name3=rep_len(name3set, n)
  
  link.1.2 = link.2.3 = link.3.2 = link.2.1 = "default"
  if(is.dedicated) {
    link.1.2 = paste(name1,name2, sep="")
    link.2.3 = paste(name2,name3, sep="")
    link.3.2 = paste(name3,name2, sep="")
    link.2.1 = paste(name2,name1, sep="")
  }
    
  workload= combine_works(t.user.front, name1, name2, name3, 
                            w.1.1, psize.front.mid, link.1.2,
                            w.2.1, psize.mid.back, link.2.3,
                            w.3, psize.back.mid, link.3.2,
                            w.2.2, psize.mid.front, link.2.1,
                            w.1.2)
  
  #write.table(workload, file=filename, row.names=FALSE, quote = FALSE, sep=",", col.names = !is.append, append = is.append)
  #return(length(t.user.front)) # returns median of the first CPU MIs
  return(workload)
} 

is.list.empty<-function(workload_list, index) {
  return(is.na(workload_list[[index]][1]))
}

create_historical_data<-function(langId, timelist, ratelist, webNum, appNum, dbNum,
                                 startTime=0, endTime=.Machine$integer.max,
                                 use.dedicated.network = TRUE, fileId=-1
                                 ) {
  if(fileId == -1)
    fileId = langId
  
  is_append=FALSE
  interevent = timelist[2] - timelist[1]

  maxNum = Lcm(webNum, Lcm(appNum, dbNum))
  webSeq = rep_len(seq(0,webNum-1), maxNum)
  appSeq = rep_len(seq(0,appNum-1), maxNum)
  dbSeq = rep_len(seq(0,dbNum-1), maxNum)
  webNamePool = paste(sprintf("web%d-", langId), webSeq, sep="")
  appNamePool = paste(sprintf("app%d-", langId), appSeq, sep="")
  dbNamePool = paste(sprintf("db%d-", langId), dbSeq, sep="")
  
  workload_list = list() # List to store all workloads
  
  for(workloadId in 1:(maxNum))   {
    workload_list[[workloadId]] = NA
  }
    
  for(i in 1:(length(ratelist))) {
    
    stime = timelist[i]
    if( (stime < startTime) || (stime > endTime)) {
      next
    }
    stime = stime - startTime

    rate_per_vm = NUM_WORKLOAD_RATE_SCALE*ratelist[i] / maxNum
    
    
    for(workloadId in 1:(maxNum))   {
      seed = langId*31+i*13+(workloadId-1)*7+127

      webName = webNamePool[workloadId]
      appName = appNamePool[workloadId]
      dbName = dbNamePool[workloadId]
      
      workload = create_workload(durtime=interevent, n=-1,
                      seed=seed, target.rate = rate_per_vm, starttime=stime,
                      name1set=webName, name2set=appName, name3set=dbName, 
                      filename=NA , is.dedicated=use.dedicated.network, is.append=is_append)
      
      # Store workload in the list. for each workload ID.
      if(!is.na(workload[1])) {
        if(!is.list.empty(workload_list, workloadId))
          workload_list[[workloadId]] = rbind(workload_list[[workloadId]], workload)
        else
          workload_list[[workloadId]] = workload
      }
    }
    
    # every 300 rounds, flush workload_list to a file
    if(i %% 100 == 0) {
      cat("=== Saving workload.. Start=",stime,", Target rate =",rate_per_vm, ", dur=",interevent,"\n")
      
      for(workloadId in 1:(maxNum)) {
        if(!is.list.empty(workload_list, workloadId)) {
          filename = sprintf("%d_%d_workload_wiki.csv", langId, workloadId-1)
          write.table(workload_list[[workloadId]], file=filename, row.names=FALSE, quote = FALSE, sep=",", col.names = !is_append, append = is_append)
          workload_list[[workloadId]] = NA
        }
      }
      is_append=TRUE
    }
  }
  
  for(workloadId in 1:(maxNum)) {
    if(!is.list.empty(workload_list, workloadId)) {
      filename = sprintf("%d_%d_workload_wiki.csv", langId, workloadId-1)
      write.table(workload_list[[workloadId]], file=filename, row.names=FALSE, quote = FALSE, sep=",", col.names = !is_append, append = is_append)
      workload_list[[workloadId]] = NA
    }
  }
}
#endTime=.Machine$integer.max
main_history<-function(lang="de", startTime=0, endTime=5000) {
  #history_file="overallstatpersec9-1-mod3.csv"
  history_file="overallstatpersec9-1-sfc.csv"
  historyData = read.csv(file=history_file,sep=',',header=F)

  if(lang != 0) {
    # pass lang either "de", "fr", "ru", "zh", or "es"
    "
    langId=0
    fileId=1
    create_historical_data(langId, timelist=historyData$V1[historyData$V2==lang],
                           ratelist=historyData$V3[historyData$V2==lang],
                           webNum=8,
                           appNum=0,
                           dbNum=0,
                           startTime=startTime, endTime=endTime, fileId=fileId)
    "
    
    langId=0
    fileId=0
    lang="de"
    create_historical_data(langId, timelist=historyData$V1[historyData$V2==lang],
                           ratelist=historyData$V3[historyData$V2==lang],
                           webNum=40,
                           appNum=120,
                           dbNum=10,
                           startTime=startTime, endTime=endTime, fileId=fileId)

  }
  else {
    # create workload for each language set
    i=0
    for(lang in levels(historyData$V2)) {
      create_historical_data(i, timelist=histoinsinryData$V1[historyData$V2==lang],
                                ratelist=historyData$V3[historyData$V2==lang],
                             webNum=8,
                             appNum=24,
                             dbNum=2,
                             startTime=startTime, endTime=endTime)
      i=i+1
    }
  }
}

