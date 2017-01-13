## Loads and processes Data of the TSO Amprion

Load          = read.delim("Data/LoadTSO.txt", sep=";") # change to LoadBU.txt for data of BU
Load_mat      = reshape(Load[,-3],timevar="Uhrzeit", idvar="Datum", direction="wide")
Load_mat[,1]  = as.Date(Load_mat[,1])
Load_mat      = Load_mat[order(Load_mat[, 1]),-98 ]
time          = seq.Date(from=min(Load_mat[,1]),to=max(Load_mat[,1]),by="day")
Load_mat      = merge(as.data.frame(time),Load_mat,all.x=TRUE,by.x = 1, by.y = 1)
rownames(Load_mat) = time

Load_pred     = reshape(Load[,-4],timevar="Uhrzeit", idvar="Datum", direction="wide")
Load_pred[,1] = as.Date(Load_pred[,1])
Load_pred     = Load_pred[order(Load_pred[, 1]),-98 ]
Load_pred     = merge(as.data.frame(time),Load_pred,all.x=TRUE,by.x = 1, by.y = 1)
Load_pred[,1] = time

remove = c(seq.Date(from=as.Date("2010-01-01"),to=as.Date("2010-01-05"),by="day"),
          seq.Date(from=as.Date("2010-12-23"),to=as.Date("2011-01-05"),by="day"),
          seq.Date(from=as.Date("2011-12-23"),to=as.Date("2012-01-05"),by="day"),
          seq.Date(from=as.Date("2012-12-23"),to=as.Date("2012-12-31"),by="day"))

Load_mat[which(Load_mat<=10000,arr.ind = TRUE)] = NA

Load_mat[16,2:97] = (Load_mat[23,2:97]) 

for(i in 2:97){
  a             = which(is.na(Load_mat[,i]))
  weekm         = a-rep(7,length(a))
  weekp         = a+rep(7,length(a))
  Load_mat[a,i] = (Load_mat[weekm,i]+Load_mat[weekp,i])/2
}

Load_mat    = Load_mat[-c(which(Load_mat[,1]%in%remove)),]
Load_pred   = Load_pred[-c(which(Load_pred[,1]%in%remove)),]

Temperature = read.delim("Data\\TempTSO.txt", sep=";")
Sun_K       = read.delim("Data\\SunKoeln.txt", header=F)
Sun_K[,1]   = as.Date(Sun_K[,1])
Sun_F       = read.delim("Data\\SunFF.txt", header=F)
Sun         = rowMeans(cbind(Sun_K[,2],Sun_F[,2]))

Sun         = Sun[-c(which(Sun_K[,1]%in%remove))]
Temperature = Temperature[-c(which(Sun_K[,1]%in%remove)),2]
Sun_K       = Sun_K[-c(which(Sun_K[,1]%in%remove)),]

dummy       = cbind(as.numeric(Load_mat[,1] %in% as.Date(c("2010-04-02", "2010-04-05", "2010-05-13", "2010-05-24", "2010-06-03", 
              "2010-11-01", "2011-04-22", "2011-04-25", "2011-06-02", "2011-06-13", "2011-06-23", "2011-10-03", "2011-11-01", 
              "2012-04-06", "2012-04-09", "2012-05-01", "2012-05-17", "2012-05-28", "2012-06-07", "2012-10-03", "2012-11-01"))), 
               as.numeric(Load_mat[,1] %in% as.Date(c("2010-05-14", "2010-06-04", "2011-06-03", "2011-06-24", "2012-04-30", 
               "2012-05-18", "2012-06-08", "2013-05-10", "2013-05-31"))), as.numeric(Load_mat[,1] %in% as.Date(c("2010-10-03",
                                                                                                                                                        "2011-05-01"))), as.numeric(Load_mat[,1] %in% as.Date(c("2010-05-01"))))

rm(list=ls()[! ls() %in% c("Load","Load_mat","Temperature","Sun","Load_pred","dummy")])

