bintoAWD<-function(binfile, epoch,output){

	#Install
	install.packages("GENEAread",repos = "http://cran.us.r-project.org") 
	
	# Load in the libraries
	library(GENEAread)
	

	#Read core binfile
	procfile<-read.bin(binfile)
	header<-procfile$header
	proc<-procfile$data.out
	
	filename = toString(as.character(output))
	filename<- paste(filename, "AWD", sep = ".")
	#header contains most of the relevant information
	
	#Subject code
	ID <-as.character(header$Value[10])
	
	#Start date
	date<-(header$Value[4])
	date <-parse.time(as.character(date), format = "POSIX")
	startDate<-strftime(date, "%d-%b-%Y")
	writeD<-c(as.character(ID),as.character(startDate))
	
	#Start time
	startTime<-strftime(date, "%H:%M")
	writeD<-append(writeD,as.character(startTime))
	
	#Epoch length
	writeD<-append(writeD,paste(" ",as.character(epoch/15),sep=""))

	#Subject age based on their birth date
	time <-parse.time(as.character(Sys.time()), format = "POSIX")
	Age<-floor(as.numeric(difftime(time, date, units = "days")/365.25))

	writeD<-append(writeD,as.character(Age))

	#actigraph ID
	ID<-header$Value[1]
	writeD<-append(writeD,as.character(ID))

	#Gender
	Gender<-header$Value[12]
	if (Gender == "male"){
		Gender="M"}
	if (Gender=="female"){
	Gender = "F"}
	
	writeD<-append(writeD,as.character(Gender))

	# Outputs this to a AWD file
	write(writeD,file = filename)

	# works out the SVM of all the data point
	df <-apply(proc[,c(2,3,4)],1,function(x) {abs(sqrt(sum(x^2))-1)})

	# Finds the number of segments based on the frequency and the epoch size
	n<-((as.numeric(header$Value[2]))*(epoch))

	#Breaks SVM into segments and finds the mean
	df2<-aggregate(as.matrix(df),list(rep(1:(nrow(as.matrix(df))%/%n+1),each=n,len=nrow(as.matrix(df)))),sum)[-1];
	df2<-round(df2,0)
	#Breaks light into segments and finds the mean
	df<-proc[,5]
	
	df3<-aggregate(as.matrix(df),list(rep(1:(nrow(as.matrix(df))%/%n+1),each=n,len=nrow(as.matrix(df)))),mean)[-1];
	df3<-round(df3,1)

	#Assigns an index so that they can be merged in place
	df2["index"]<-seq(1,nrow(df2),1)
	df3["index"]<-seq(1,nrow(df3),1)
	df<-merge(df2,df3, by="index")
	
	
	#Appends this information to the file
	write.table(df[,2:3], file = filename, row.names=FALSE,col.names=FALSE, append=TRUE, sep =" , ")
}
