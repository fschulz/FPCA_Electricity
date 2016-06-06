## Load and Process Data

Load          = read.csv("C:/Users/Franziska Schulz/Dropbox/Phd/Data/Amprion1012.csv", sep=";")
Load_mat      = reshape(Load[,-3],timevar="Uhrzeit", idvar="Datum", direction="wide")
Load_mat[,1]  = as.Date(Load_mat[,1])
Load_mat      = Load_mat[order(Load_mat[, 1]),-98 ]
time          = seq.Date(from=as.Date("2010-01-01"),to=as.Date("2012-12-31"),by="day")
missing       = time[which(time%in%Load_mat[,1]==FALSE)]
na            = matrix(ncol=97,nrow=1)
Load_mat      = rbind(Load_mat[1:8,],NA,Load_mat[9:308,],NA,Load_mat[309:327,],NA,Load_mat[328:1093,])
Load_mat[,1]  = time
rownames(Load_mat) = Load_mat[,1]

Load_pred     = reshape(Load[,-4],timevar="Uhrzeit", idvar="Datum", direction="wide")
Load_pred[,1] = as.Date(Load_pred[,1])
Load_pred     = Load_pred[order(Load_pred[, 1]),-98 ]
Load_pred     = rbind(Load_pred[1:8,],NA,Load_pred[9:308,],NA,Load_pred[309:327,],NA,Load_pred[328:1093,])
Load_pred[,1] = time

remove<-c(seq.Date(from=as.Date("2010-01-01"),to=as.Date("2010-01-05"),by="day"),
          seq.Date(from=as.Date("2010-12-23"),to=as.Date("2011-01-05"),by="day"),
          seq.Date(from=as.Date("2011-12-23"),to=as.Date("2012-01-05"),by="day"),
          seq.Date(from=as.Date("2012-12-23"),to=as.Date("2012-12-31"),by="day"))

#replace<-c(as.Date(c("2010-04-02","2010-04-05","2010-05-01","2010-05-13","2010-05-24","2010-06-03","2010-10-03","2010-11-01")),
#           as.Date(c("2011-04-22","2011-04-25","2011-05-01","2011-06-02","2011-06-13","2011-06-23","2011-10-03","2011-11-01")),
#           as.Date(c("2012-04-06","2012-04-09","2012-05-01","2012-05-17","2012-05-28","2012-06-07","2012-10-03","2012-11-01")))

#Load_mat[which(Load_mat[,1]%in%replace),2:97]<-NA
#for(i in 2:97){
#    Load_mat[which(Load_mat[,i]==0),i]<-NA
#}

Load_mat[16,2:97] = (Load_mat[23,2:97]) # replace by hand because of adjacent missings

for(i in 2:97){
    a             = which(is.na(Load_mat[,i]))
    print(a)
    print(Load_mat[a,1])
    weekm         = a-rep(7,length(a))
    weekp         = a+rep(7,length(a))
    Load_mat[a,i] = (Load_mat[weekm,i]+Load_mat[weekp,i])/2
}
rm(a,i,weekm,weekp)

Load_mat    = Load_mat[-c(which(Load_mat[,1]%in%remove)),]  # Load_mat: observed load 
Load_pred   = Load_pred[-c(which(Load_pred[,1]%in%remove)),] # Load_pred : prediction from Amprion

## load explanatory variables
Temperature = read.csv("C:/Users/Franziska Schulz/Dropbox/Phd/Data/AveTemp1012.csv", sep=";")
Spot        = read.table("C:/Users/Franziska Schulz/Dropbox/Phd/Data/SpotPreiseBloomberg1012.csv", sep=";", quote="\"")
Sun_K       = read.delim("C:/Users/Franziska Schulz/Dropbox/Phd/Data/Koeln Klima/Sonnenscheindauer1012.txt", header=F)
Sun_K[,1]   = as.Date(Sun_K[,1])
Sun_F       = read.delim("C:/Users/Franziska Schulz/Dropbox/Phd/Data/Frankfurt Klima/Sonnenscheindauer1012.txt", header=F)
Sun         = rowMeans(cbind(Sun_K[,2],Sun_F[,2]))
Daily_Spot<-rev(rowMeans(Spot,na.rm=TRUE))

Sun         = Sun[-c(which(Sun_K[,1]%in%remove))]
Daily_Spot  = Daily_Spot[-c(which(Sun_K[,1]%in%remove))]
Temperature = Temperature[-c(which(Sun_K[,1]%in%remove)),]
Sun_K       = Sun_K[-c(which(Sun_K[,1]%in%remove)),]

######
k=0
n=702
weekhours       = 7*96
hours           = 96
week            = 1:weekhours

yearhours       = 351
year            = 1:yearhours

training        = Load_mat[1:(n),-1] # 
training_temp   = Temperature[1:(n)] # 2 day forecast
training_sun    = Sun[1:(n)]

#leap            = which(Load_mat[,1]=="2012-02-28")

# transpose and reshape
Load_t          = as.data.frame(t((training))) # log transformation
Load_list       = unlist(Load_t,recursive=TRUE)
