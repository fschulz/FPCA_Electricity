# Loads and processes data for BU Saarbruecken

setwd("C:/Users/Franziska Schulz/Dropbox/Phd")

#---------------------------------------------

Load          = read.delim("Data/EntnahmeSaarbrucken.txt", header=F)[,c(1,2,8)]
Temperature   = read.delim("C:/Users/Franziska Schulz/Dropbox/Phd/Data/Wetter Saarbruecken/Wetter1012.txt", header=F)[,7]
Sun           = read.delim("C:/Users/Franziska Schulz/Dropbox/Phd/Data/Wetter Saarbruecken/Sonnenscheindauer1012.txt", header=F)
Wind          = read.delim("Data/Wetter Saarbruecken/Wind1012.txt", header=F)
Load_mat      = reshape(Load,timevar="V2", idvar="V1", direction="wide")
Load_mat[,1]  = as.Date(Load_mat[,1])

# Remove weeks around christmas
Load_mat      = Load_mat[-816,-98]

remove        = c(seq.Date(from=as.Date("2010-01-01"),to=as.Date("2010-01-05"),by="day"),
                  seq.Date(from=as.Date("2010-12-23"),to=as.Date("2011-01-05"),by="day"),
                  seq.Date(from=as.Date("2011-12-23"),to=as.Date("2012-01-05"),by="day"),
                  seq.Date(from=as.Date("2012-12-23"),to=as.Date("2012-12-31"),by="day"))
#replace       = c(as.Date(c("2010-04-02","2010-04-05","2010-05-01","2010-05-13","2010-05-24","2010-06-03","2010-08-15","2010-10-03","2010-11-01")),
#                  as.Date(c("2011-04-22","2011-04-25","2011-05-01","2011-06-02","2011-06-13","2011-06-23","2011-08-15","2011-10-03","2011-11-01")),
#                  as.Date(c("2012-04-06","2012-04-09","2012-05-01","2012-05-17","2012-05-28","2012-06-07","2012-08-15","2012-10-03","2012-11-01")))

#Load_mat[which(Load_mat[,1]%in%replace),2:97]<-NA
Temperature   = Temperature[-c(which(Load_mat[,1]%in%remove))]
Sun           = Sun[-c(which(Load_mat[,1]%in%remove)),2]
Load_mat      = Load_mat[-c(which(Load_mat[,1]%in%remove)),]   

for(i in 2:97){
    a             = which(is.na(Load_mat[,i]))
    weekm         = a-rep(7,length(a))
    weekp         = a+rep(7,length(a))
    Load_mat[a,i] = (Load_mat[weekm,i]+Load_mat[weekp,i])/2
}

rm(a,i,remove,weekm,weekp)
