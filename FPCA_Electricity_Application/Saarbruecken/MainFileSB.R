
# Description
# Loads consumption for Saarbr?cken from 20100104 to 20130408
# Deaseasonalizing and logarithm
# Estimates expectiles with tau= 0.01,0.05,0.25,0.5,0.75,0.95,0.99

#---------------------------------------------
libraries<-c("fda.usc","ggplot2","vars","dse","CADFtest","forecast","gdata","urca","car","np","sm","DierckxSpline","mFilter","stats","tseries","zoo","quantreg", "orthogonalsplinebasis","expectreg", "splines","Matrix", "MatrixModels", "cobs","fields","stargazer")
lapply(libraries,function(x)if(!(x %in% installed.packages())){install.packages(x)})
lapply(libraries,require,quietly=TRUE,character.only=TRUE)

setwd("C:\\Users\\Franziska Schulz\\Desktop\\Github\\FPCA_Electricity\\FPCA_Electricity_Application\\Saarbruecken")

load("Data_SB.RData")

## Table 1: Summary Statistics
statistics = function(x){c(median(x),mean(x),sd(x),min(x),max(x))}
stat = statistics(as.matrix(Load_mat[,-1]))
names(stat) = c("Median","Mean","SD","Min","Max")
stat

## Plot 2: Temperature, Sunshine
plot(Temperature, type="l", lwd = 3, cex.axis = 2,xaxt='n', cex.lab = 2, xlab = "Time")
axis(1,c(175.5,526.5,877.5),c("2010","2011","2012"),cex.axis=2)
plot(Sun, type="l", lwd = 3, cex.axis = 2,xaxt='n', cex.lab = 2, xlab = "Time")
axis(1,c(175.5,526.5,877.5),c("2010","2011","2012"),cex.axis=2)


## Deterministic Seasonal Component
k=0
n=702
weekhours       = 7*96
hours           = 96
week            = 1:weekhours

yearhours       = 351
year            = 1:yearhours

training        = Load_mat[1:n,-1] # 
training_temp   = Temperature[1:n] # 2 day forecast
training_sun    = Sun[1:n]

#leap            = which(Load_mat[,1]=="2012-02-28")

# transpose and reshape
Load_t          = as.data.frame(t((training))) # log transformation
Load_list       = unlist(Load_t,recursive=TRUE)

####
fourier.series = function(t,terms,period)
{
    n = length(t)
    X = matrix(,nrow=n,ncol=2*terms)
    for(i in 1:terms)
    {
        X[,2*i-1] = sin(2*pi*i*t/period)
        X[,2*i]   = cos(2*pi*i*t/period)
    }
    colnames(X)   = paste(c("S","C"),rep(1:terms,rep(2,terms)),sep="")
    return(X)
}

dsc = function(Load_list, WD, PH){
    # Load: Load in long format
    # WD: Dummie for Weekdy
    # PH: Dummie for Public Holiday

    RLoad = Load_list
    DT_RLoad = RLoad
    season_forecast = matrix(ncol=1,nrow=96)
    for(i in 1:96){
        index = as.numeric(seq(from=i, to=(702+k)*96, by=96))
        temp = RLoad[index]
        temp_date = Load_mat[index,1] 
        t = 1:length(temp)
        ltsc = lm(temp ~ t + WD[-(702+k+1),] + PH[-(702+k+1)] + fourier.series(t,4,365.25))#+ Dec24 + Dec25 + Dec26 + Dec31 + Jan01 + WH
        DT_RLoad[index] = ltsc$residuals
        
    }
    return(DT_RLoad)
}

## Dummies
tempseq = as.numeric(rep(rep(c(1:7),each=1),length.out=702+k+1))
Mo = as.numeric(tempseq==6)
Tu = as.numeric(tempseq==7)
We = as.numeric(tempseq==1)
Th = as.numeric(tempseq==2)
Fr = as.numeric(tempseq==3)
Sa = as.numeric(tempseq==4)
WD  = cbind(Mo, Tu, We, Th, Fr, Sa)

PH_dummy =as.Date(c("2010-04-02","2010-04-05","2010-05-13","2010-05-24","2010-06-03","2010-11-01", #,"2010-08-15","2010-10-03" ,"2010-05-01"
                    "2011-04-22","2011-04-25","2011-06-02","2011-06-13","2011-06-23","2011-08-15","2011-10-03","2011-11-01", #,"2011-05-01" 
                    "2012-04-06","2012-04-09","2012-05-01","2012-05-17","2012-05-28","2012-06-07","2012-08-15","2012-10-03","2012-11-01", # 
                    "2013-03-29","2013-04-01","2013-05-01","2013-05-09","2013-05-20","2013-05-30"))
#BD_dummy = as.Date(c("2011-06-03","2011-06-24","2012-04-30","2012-06-08"))
#WH_dummy = as.character(c(seq.Date(from=as.Date("2010-01-01"),to=as.Date("2010-01-06"),by="day"),
#                          seq.Date(from=as.Date("2010-12-24"),to=as.Date("2011-01-08"),by="day"),
#                          seq.Date(from=as.Date("2011-12-21"),to=as.Date("2012-01-08"),by="day")))
#WH = as.numeric(Load_mat[(1:(702+k+1)),1] %in% WH_dummy)# Winter holiday
PH = as.numeric(Load_mat[(1:(702+k+1)),1] %in% PH_dummy)# public holiday
#BD = as.numeric(Load_mat[(1:(702+k+1)),1] %in% BD_dummy)# Bridge Day
#MariaH = as.numeric(Load_mat[(1:(702+k+1)),1] %in% as.Date(c("2010-08-15","2011-08-15","2012-05-18")))
#PH_Sun = as.numeric(Load_mat[(1:(702+k+1)),1] %in% as.Date(c("2010-10-03","2011-05-01")))
#PH_Sat = as.numeric(Load_mat[(1:(702+k+1)),1] %in% as.Date(c("2010-05-01")))

#PH.Dummie = cbind(WH, PH, BD, MariaH, PH_Sun, PH_Sat) 

DT_RLoad = dsc(Load_list, WD, PH)
plot(RLoad,type="l")
plot(DT_RLoad,type="l")

#####
season.cov = function(x, year){
    mat = as.matrix(x)
    YN1 = matrix(data=mat,nrow=length(year),byrow=FALSE)
    Y1  = rowMeans(YN1,na.rm = TRUE) #mean over years of hourly demand
    hs = h.select(year,Y1, method="cv")
    s = sm.regression(year,Y1,hs,eval.points=year)
    season1 = s$estimate
    season11 = rep(season1,dim(mat)[1]/length(year))
    resid = (mat-season11)
    return(resid)
}

resid_temp = season.cov(training_temp, year)
resid_sun  = season.cov(training_sun, year)


