##First, we should set the main directory.
setwd("E:/Rprogrammingsourse")
##Q1 Construct a complete data by using labels and measurements
data<-read.table("UCI HAR Dataset/train/X_train.txt",sep="")
data1<-read.table("UCI HAR Dataset/test/X_test.txt",sep="")
datafin<-data.frame(rbind(data,data1),stringsAsFactors=FALSE)
##datafin is the merged result of X_train.txt and X_test.txt.

##For convenience of the rest questions, I will first get the final data.
datalab<-read.table("UCI HAR Dataset/train/y_train.txt")$V1
data1lab<-read.table("UCI HAR Dataset/test/y_test.txt")$V1
datalabfin<-c(datalab,data1lab)
features<-as.vector(read.table("UCI HAR Dataset/features.txt")$V2)
datanew<-data.frame(datalabfin,datafin)
colnames(datanew)<-c("activity_num",features)##datanew is the complete data possessing labels,measurements.

##Q2 extraction of mean and standard deviation for measurements
extract<-grep("mean|std",features)
getdata<-datanew[,(extract+1)]

##Q3 Add descriptive activities to the datanew
datalabfin[datalabfin==1]<-"WALking"
datalabfin[datalabfin==2]<-"WALKING_UPSTAIRS"
datalabfin[datalabfin==3]<-"WALKING_DOWNSTAIRS"
datalabfin[datalabfin==4]<-"SITTING"
datalabfin[datalabfin==5]<-"STANDING"
datalabfin[datalabfin==6]<-"LAYING"
datanew<-data.frame("activity"=datalabfin,datanew)
##Q4 Name variables with descriptive labels
##This assignemnt has already been done in the first question. The final datanew has variable labels.
##Q5 
datasubject<-datalab<-read.table("UCI HAR Dataset/train/subject_train.txt")$V1
data1subject<-datalab<-read.table("UCI HAR Dataset/test/subject_test.txt")$V1
subject<-c(datasubject,data1subject)
datanew<-data.frame("subject"=subject,datanew)
x<-matrix(nrow=6,ncol=561)
for (i in 1:6){
  dat<-datanew[datanew$activity_num==i,]
  y<-numeric()
  for (j in 4:564){
    y[j-3]<-mean(dat[,j])
  }
  x[i,]<-y
}
y<-matrix(nrow=30,ncol=561)
for (i in 1:30){
  dat<-datanew[datanew$subject==i,]
  s<-numeric()
  for (j in 4:564){
    s[j-3]<-mean(dat[,j])
  }
  y[i,]<-s
}
meandata<-data.frame(rbind(x,y))
colnames(meandata)<-features
rownames(meandata)<-c(paste("mean for activity",1:6,sep=" "),paste("mean for subject",1:30,sep=" "))
##meandata is the final result for mean values of each subjects and activities
