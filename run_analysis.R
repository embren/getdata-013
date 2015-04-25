a <- setwd("C:\\Users\\A580489\\Documents\\R\\Coursera\\Cleaning Data\\UCI HAR Dataset")
x <- c("/train/","/test/")
for(i in x){
  setwd(paste0(a,i))
  filenames <- dir()[-1]
  for(j in filenames){
    assign(gsub(".txt","",j),read.table(j,header=F))
  }  
}

data <- cbind(y_train,subject_train,X_train)
data2 <- cbind(y_test,subject_test,X_test)
data3 <- rbind(data,data2)

features <- t(read.table(paste0(a,"/features.txt")))
colnames(data3) <- c("Activity","Subject",features[2,])
mean <- which(grepl("mean",colnames(data3)))
sd <- which(grepl("std",colnames(data3)))

data4 <- data3[,c(1,2,mean,sd)]
data4 <- data4[order(data4[,1],decreasing=F),]
data4[,1] <- sapply(data4[,1], function(x) switch(x,'1'="WALKING",
                    '2'="WALKING_UPSTAIRS",'3'="WALKING_DOWNSTAIRS",
                      '4'="SITTING",'5'="STANDING",'6'="LAYING","UNK"))

clean <- data.frame(matrix(data = NA,nrow = 180,ncol=ncol(data4)))
colnames(clean) <- colnames(data4)
rw <- 1
for(i in 1:30){
  for(j in c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")){
    clean[rw,1] <- i
    clean[rw,2] <- j
    activ <- data4[data4$Activity==j,]
    subj <- activ[activ$Subject==i,]
    clean[rw,3:ncol(clean)] <- colMeans(subj[,3:ncol(subj)])
    rw <- rw+1
  }
}
setwd("C:/Users/A580489/Documents/R/Coursera/Cleaning Data")
write.table(clean,"Tidy Data.txt",row.name=F)
