pointfourer<-function(x, threshold=0.04)
	{
	if (x<threshold)
		{
		return(x)
		}
	else
		{
		return(threshold)
		}
	}
activityduration<-function(input,threshold, allowance)
  {

  input = c(input,-1)

  j=1
  i = 0
  counter = 0
  tuples1 = numeric(length(input)/2)
  tuples2 = numeric(length(input)/2)
  while (input[j] !=-1) 
    {
    if (input[j] >= threshold)
      {
      k=j
      while (mean(input[j:(k+i)])>=threshold*allowance)
        {
        i= i+1
        if (input[k+i] == -1)
          {
          tuples1[counter+1] = j
          tuples2[counter+1] = (k+i-1)
          return(data.frame(tuples1,tuples2))
          }
        }
	    counter = counter+1
	    tuples1[counter] = j
	    tuples2[counter] = (k+i)-1
	    j =k+i
	    i=0
	    }
	  j = j+1
    }
  return(data.frame(tuples1,tuples2))
  }

enmoSum<-function(a,b, dataFrame)
  {
  return(sum(as.numeric(as.character(dataFrame[a:b,2]))))
  }

files <- binfiles
counter = 0
for (j in files)
  {
  counter = counter +1
  load(j,.GlobalEnv)
  ENMO_data <-as.numeric(as.character(M$metashort[,2]))
  ENMO_data2<-apply(as.matrix(ENMO_data),1, pointfourer,threshold = 0.04)
  times<-activityduration(as.numeric(ENMO_data2),0.04,0.8)
  times = times[times[,1]!=0,]
  times["Dur"] = apply(times, 1, function(m){(m[2]-m[1] +1)})
  times = times[times[,"Dur"]>10,]
  times["Volume"] = apply(times, 1, function(m){enmoSum(m[1],m[2], M$metashort)})
  times["WeekDay"] = apply(times, 1, function(m){(as.POSIXlt(M$metashort[m[1],1])$yday)})
  times["DayNumber"] = times[,"WeekDay"] - (as.POSIXlt(M$metashort[1,1])$yday - 1)
  times["WeekDay"] = apply(times, 1, function(m){(as.POSIXlt(M$metashort[m[1],1])$wday)})
  times["WeekDay"] = apply(times, 1, function(m){(weekdays(as.POSIXlt(M$metashort[m[1],1])))})
  t = M$metashort[times[,1],][,1]
  times["Time"] =t
  times["Time"] = apply(times["Time"],1 , function(m){substr(as.character(m),12,20)})
  times["ID"] = rep(j,nrow(times))
  times["ID2"] = rep(counter,nrow(times))
  print(counter)
  times["NonWear"] =  as.numeric(apply(times, 1, function(m){(as.POSIXlt(M$metashort[as.numeric(m[1]),1]))$yday}))
  q = aggregate(M$metalong[,2], by=list((as.POSIXlt(as.character(M$metalong[,1])))$yday),function(x){length(x[x==0])/4})
  q["NonWear"] = q$Group.1
  times = merge(times, q)
  times = times[-1]
  times = times[-10]
  q = as.numeric(rownames(unique(times[,c(6,5)])))
  t = rbind(times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,],times[q,])
  t["tuples1"] = rep(0, nrow(t))
  t["tuples2"] = rep(0, nrow(t))
  t["Dur"] = rep(0, nrow(t))
  t["Volume"] = rep(0, nrow(t))
  qtimes = c("00:00:01","01:00:01","02:00:01","03:00:01","04:00:01","05:00:01","06:00:01","07:00:01","08:00:01","09:00:01","10:00:01","11:00:01","12:00:01","13:00:01","14:00:01","15:00:01","16:00:01","17:00:01","18:00:01","19:00:01","20:00:01","21:00:01","22:00:01","23:00:01")
  t["Time"] = rep(qtimes,each = length(q))
  times = rbind(times,t)
  q = rep(M$metalong[,2],each = 900)
  t = apply(times[,c(1,2)],1,function(x){sum((q[x[1]:x[2]]!=0))})
  times["NonWear Code"] = t
  write.table(times, paste(j, ".csv", sep = ""), append = TRUE, sep = ",", col.names = FALSE, row.names = FALSE)

}
