
#--------- Membaca file dataset
student <-read.csv("E:/Frequently Used/Edukasi/Kuliah/Semester 7/Optimasi Kombinatorik dan Heuristik/Toronto/lse-f-91.stu",header=FALSE, sep=" ")
course <-read.csv("E:/Frequently Used/Edukasi/Kuliah/Semester 7/Optimasi Kombinatorik dan Heuristik/Toronto/lse-f-91.crs",header=FALSE, sep=" ", col.names = paste0("V",seq_len(7)))


#--------- Membuat conflict matrix
conflictMatrix <- matrix(0,nrow=nrow(course), ncol=nrow(course))

for (stud in 1:nrow(student)) {
  for (i in 1:ncol(student)-1){
    for (j in(i+1): ncol(student)){
      a <- as.integer(student[stud,i])
      b <- as.integer(student[stud,j]) 
      conflictMatrix[a,b] = conflictMatrix[a,b]+1
      conflictMatrix[b,a] = conflictMatrix[b,a]+1
    }
  }
}

#--------- Membuat binary conflict matrix
conflictMatrix1 <- conflictMatrix
for (i in 1:nrow(conflictMatrix)) {
  for ( j in 1:ncol(conflictMatrix)) {
    if (conflictMatrix[i,j]>0){
      conflictMatrix1[i,j]<-1
    }
  }
}
conflictMatrix1<-as.data.frame(conflictMatrix1)

#--------- Menghitung jumlah course
number_course <- 0
for (i in 1:number_student) {
  for (j in 1:max_clique)
    number_course <- pmax(number_course, student[i,j])
}
number_course <- as.integer(number_course);
course_tt <- cbind(c(1:number_course), 0)

library(tibble)
library(dplyr)

##Timeslot
#--------- Membuat timeslot dengan largest degree
start_time <- Sys.time()
conflictMatrix1$sum<-sapply(conflictMatrix1, sum)
conflictMatrix1<-rownames_to_column(conflictMatrix1, "rowname")
conflictMatrix1<-conflictMatrix1 %>% arrange(desc(sum))
conflictMatrix1<-column_to_rownames(conflictMatrix1, "rowname")
conflictMatrix1<-conflictMatrix1[,-ncol(conflictMatrix1)]
conflictMatrix1[nrow(conflictMatrix1)+1,] <- sapply(conflictMatrix1,sum)
conflictMatrix1 <- conflictMatrix1[,order(-conflictMatrix1[nrow(conflictMatrix1),])]
conflictMatrix1 <- conflictMatrix1[-nrow(conflictMatrix1),]
timeslot <- list()
timeslot[[1]]<-list()
timeslot[[1]]<-append(timeslot[[1]], rownames(conflictMatrix1[1,]))

for(i in 2:nrow(conflictMatrix1)){
  for(j in 1:length(timeslot)){
    crash<-0
    for (k in 1:length(timeslot[[j]])){
      crash<-pmax(crash, conflictMatrix1[timeslot[[j]][[k]],i])
    }
    if (crash==0){
      timeslot[[j]]<-append(timeslot[[j]], rownames(conflictMatrix1[i,]))
      break 
    }
    else if (j==length(timeslot)){
      timeslot[[length(timeslot)+1]]<-list()
      timeslot[[length(timeslot)]]<-rownames(conflictMatrix1[i,])
    }
  }
}

#--------- Mencetak Solusi
for (i in 1:length(timeslot)){
  for (j in 1:length(timeslot[[i]])){
    a<-paste(timeslot[[i]][[j]])
    b<-paste(a, i-1)
    write(b, file="E:/Frequently Used/Edukasi/Kuliah/Semester 7/Optimasi Kombinatorik dan Heuristik/Evaluate/timeslot.txt", sep="", append=TRUE)
  }
}


#--------- Menghitung Proximity Cost
penalty<-function (conflictMatrix, course_tt, number_student) {
  penalty2 = 0;
  row = nrow(conflictMatrix);
  for (i in 1:(row-1)) {
    for (j in (i+1):row) {
      if(conflictMatrix[i,j]>0)
      {
        if(abs(course_tt[j,2]-course_tt[i,2])>=1 && abs(course_tt[j,2]-course_tt[i,2])<=5)
        {
          penalty2 = penalty2 + (conflictMatrix[i,j]*(2 ^ (5-(abs(course_tt[j,2]-course_tt[i,2])))));
        }						
      }
    }
  }
  return(penalty2/number_student);
}

print(penalty(conflictMatrix, course_tt, number_student))
end_time <- Sys.time()
end_time - start_time