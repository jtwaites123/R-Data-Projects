Angle_calc_360<-function(vect)
	{
	# Get accelerations
	x = vect[1]
	z = vect[3]
	# Workout angle and convert to degrees
	Angle = atan(abs(z)/abs(x))
	Angle = Angle*(180/pi)
			
	if (sign(x) == 1 & sign(z)==1) {
		return((90-Angle))
	}
	if (sign(x) == -1 & sign(z)==1) {
		return((180-Angle))
	}
	if (sign(x) == -1 & sign(z)==-1) {
		return((180+Angle))
	}
	return((360-Angle))
	}
	
Up_Down<-function(vect, hand)
	{
	handedness = 1
	if (hand != "left wrist") {
	handedness = -1
	}
		
	y = vect[2]
	if (y>1){
	return(handedness * asin(1)*(180/pi))
	}
	if (y< -1){
	return(handedness * asin(-1)*(180/pi))
	}
	return(handedness*asin(y)*(180/pi))
	
	}


SedSphere<-function(binfile, thresholdUpDown = -15, thresholdIntensity = 489, hand = c("left wrist"))
	{
	proc<-read.bin(binfile)
	handUsed = proc$header[9,][[1]]
	if (handUsed == ""){
	handUsed = hand
	}
	proc<-proc$data.out
	times<-proc[,c(1)]
	proc<-proc[,c(2,3,4)]
	grouper<-rep(1:((nrow(proc)/1500)+1), each = 1500, len = nrow(proc))
	proc1<-aggregate(proc, by = list(grouper), mean)[-1]
	times<-aggregate(times, by = list(grouper), mean)[-1]
	proc2<-apply(proc, 1, function(x){abs(sqrt(sum(x^2))-1)})
	proc2<-aggregate(proc2, by = list(grouper), sum)[-1]
	proc1["VM"] = proc2
	proc1["360_Angle"] = apply(as.matrix(proc1),1,Angle_calc_360)
	proc1["Up_Down"] = apply(as.matrix(proc1),1,Up_Down, hand = handUsed )
	proc1["Class"]="Sedentary"
	proc1$Class[proc1["VM"]>thresholdIntensity] <- "Upright"
	proc1$Class[proc1["Up_Down"]<thresholdUpDown] <- "Upright"
	proc1["Times"]<-as.POSIXct((times[,]-7.49), origin="1970-01-01")
	
	return(proc1)
	}

summary<-function(listOfFiles, filename = "NoNameGiven.csv",time = c(0,1), how = 1, span  ="Full", SpanSize = c(0,24),hands = c("left wrist"),...)
	{
	if (listOfFiles == "*"){
	listOfFiles = list.files(pattern = "\\.bin$")
	hands = rep(hands, length(listOfFiles))
	}
	if (filename == "NoNameGiven.csv"){
	filename = paste(listOfFiles,".csv",sep = "")
	
	}
	if (length(listOfFiles) != length(hands)){
	print ("Hands is not same length as files: have tried to repair the issue but no promises")
	}
	hands = rep(hands, length(listOfFiles))[1:length(listOfFiles)]
	counter = 0
	for (file in listOfFiles)
		{
		counter = counter+1
		table<-SedSphere(file,hand = hands[counter],...)
		
		if(time[1]>=0 & time[2]<=1)
			{
			
			lower_lim <- (time[1]*nrow(table))
			upper_lim <- (time[2]*nrow(table))
			table<-table[-upper_lim:-nrow(table),]
			table<-table[-1:-lower_lim,]
			
			}
		if(is.character(time[1]))
			{
			
			z <- strptime(time[1], "%Y-%m-%d %T")
			y <- strptime(time[2], "%Y-%m-%d %T")
			truthy1<-table["Times"][,]>z
			truthy2<-table["Times"][,]<y
			
			table[truthy1,]
			table[truthy2,]
			}
		
		
		if (how ==1){
			days = as.POSIXlt(table[,8])
			days2=paste(weekdays(days),days$yday,sep="-")
			j = c(file)
			x<-c("ID")
			counter1 = -1
			for (i in unique(days2)){
			counter1 = counter1+1
				j = c(j,hour_by_hour(table[days2 == i,],file))
				for (k in (0:23)){
				x = c(x,paste("Sed in Hour",(k+(counter1*24)),i),paste("Up in Hour",(k+(counter1*24)),i))			
				}
			}
			df<-data.frame(matrix(ncol = (length(x)), nrow = 0))
			
			colnames(df) <- x
			df<-rbind(as.matrix(df),j)
		}
		if (how == 2){
			df<-data.frame(matrix(ncol = 15, nrow = 0))
			x<-c("ID", "Sed in Day 1", "Up in Day 1","Sed in Day 2", "Up in Day 2","Sed in Day 3", "Up in Day 3","Sed in Day 4", "Up in Day 4","Sed in Day 5", "Up in Day 5","Sed in Day 6", "Up in Day 6","Sed in Day 7", "Up in Day 7")
			colnames(df) <- x
			df<-rbind(as.matrix(df),day_by_day(table,file))
		}
		if (how ==3){
			df<-data.frame(matrix(ncol = 3, nrow = 0))
			x<-c("ID", "Sed", "Up")
			colnames(df) <- x
			df<-rbind(as.matrix(df),c(file,length(table$Class[table$Class=="Sedentary"])/length(table$Class),length(table$Class[table$Class=="Upright"])/length(table$Class)))
		}
		
		
		if (how == 1 & span == "Awake")
			{
		df<-df[,-44:-48]
		df<-df[,-1:-12]
			}
		if (how == 1 & span == "Custom")
			{
			df<-df[,-(2*(24-SpanSize[2])):-48]
			df<-df[,-1:-(SpanSize[1]*2)]
			}
		
		write.table(data.frame(df), filename[counter], append = TRUE, sep = ",",  col.names=!file.exists(filename[counter]), row.names = FALSE)
		}
		
}

hour_by_hour<-function(table, ID)
	{
	
	grouper<-apply(table["Times"],1,function(x) {as.POSIXlt(x)$hour})
	proc<-aggregate(table$Class == "Upright",by = list(grouper), sum)
	proportions<-proc[-1]/aggregate(grouper,by = list(grouper), length)[-1]
	proportions["Hours"] <- proc[-2]
	proportions["Time Spend in Sedentary"]<-1-proportions[-2]
	row<-rep(0,48)
	apply(proportions,1,function(x){row[2*x[2]+2] <<- x[1]})
	apply(proportions,1,function(x){row[2*x[2]+1] <<- x[3]})
	return(row)
	}
	
	
	
	
	
day_by_day<-function(table, ID)
	{
	grouper<-apply(table["Times"],1,function(x) {as.POSIXlt(x)$wday})
	proc<-aggregate(table$Class == "Upright",by = list(grouper), sum)
	proportions<-proc[-1]/aggregate(grouper,by = list(grouper), length)[-1]
	proportions["Days"] <- proc[-2]
	proportions["Time Spend in Sedentary"]<-1-proportions[-2]
	row<-rep(0,14)
	apply(proportions,1,function(x){row[2*x[2]+2] <<- x[1]})
	apply(proportions,1,function(x){row[2*x[2]+1] <<- x[3]})
	return(c(ID,row))
	}
