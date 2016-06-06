

#### Functions

fourier.series = function(t,terms,period){
    n = length(t)
    X = matrix(,nrow=n,ncol=2*terms)
    for(i in 1:terms)
    {
        X[,2*i-1] = sin(2*pi*i*t/period)
        X[,2*i]   = cos(2*pi*i*t/period)
    }
    colnames(X) = paste(c("S","C"),rep(1:terms,rep(2,terms)),sep="")
    return(X)
}

detrend = function(Load, X, Dummies, index){  
    # returns deseasonalized time series of Load
    # Load has to be in long form
    # size of X, Dummies: m + 1
    temp = Load[index]
    N = length(temp)
    t = 1:N
    ltsc = lm(temp ~ t + X + Dummies + fourier.series(t,2,351.25)) # 2 weeks removed around xmas! 351.25 if leap year!
    ltsc$coefficients 
    #df = data.frame(cbind(t , X , Dummies, fourier.series(t,1,351)))
    #pre  = sum(as.numeric(c(1,df[N,]))*ltsc$coefficients) 
    
    result = list(res = ltsc$residuals)
    return(result)
}


season.cov = function(x, year){
    mat = as.matrix(x)
    mod_year = length(year)-length(x)%%length(year)
    if(mod_year==max(year)){
        mod_year = 0
    }
    mat = rbind(mat, matrix(NaN, nrow= mod_year, ncol=1) )
    YN1 = matrix(data=mat,nrow=length(year),byrow=FALSE)
    Y1 = rowMeans(YN1,na.rm = TRUE) #mean over years of hourly demand
    
    hs = h.select(year,Y1, method="cv")
    s = sm.regression(year,Y1,hs,eval.points=year)
    
    season1 = s$estimate
    season11 = rep(season1,dim(mat)[1]/length(year))
    resid = (mat-season11)[1:(length(mat)-mod_year)]
    return(resid)
}


function = insample(Load, Temp, Sun, m){
    # Load = Load_mat[1:702,-1]
    # Temp = Temperature[1:702,2]
    # Sun  = Sun[1:702]
    # m = insample period
    training      = Load
    training_temp = Temp
    training_sun  = Sun
    

    Load_list     = unlist(as.data.frame(t(training)))
    DT_Load = Load_list
    Season  = Load_list
    
    tempseq = as.numeric(rep(rep(c(1:7),each=1),length.out=end)) # seq with 1:7 
    Mo      = as.numeric(tempseq==3)
    Tu      = as.numeric(tempseq==4)
    We      = as.numeric(tempseq==5)
    Th      = as.numeric(tempseq==6)
    Fr      = as.numeric(tempseq==7)
    Sa      = as.numeric(tempseq==1)
    X       = cbind(Mo,Tu,We,Th,Fr,Sa) # matrix with explanatory variables
    
    PH_dummy =as.Date(c("2010-04-02","2010-04-05","2010-05-13","2010-05-24","2010-06-03","2010-11-01",
                        "2011-04-22","2011-04-25","2011-06-02","2011-06-13","2011-06-23","2011-10-03","2011-11-01",
                        "2012-04-06","2012-04-09","2012-05-01","2012-05-17","2012-05-28","2012-06-07","2012-10-03","2012-11-01"))

    PH = as.numeric(Load_mat[(1:end),1] %in% PH_dummy)# public holiday
    
    for(i in 1:96){
        index = as.numeric(seq(from=i, to=end*96, by=96))
        DT = detrend(Load=Load_list,X=X,Dummies=PH, index=index)
        DT_Load[index] = DT$res
    }
    
    # seasonality covariates
    
    resid_temp = season.cov(training_temp)
    resid_sun  = season.cov(training_sun)
    
    
    ######################################################################################
    resid_data_yearly = matrix(DT_RLoad,nrow=96)
    
    x   = seq(1/nrow(resid_data_yearly),1,length.out=nrow(resid_data_yearly)) # generate x
    fit = array(0,dim=c(ncol(resid_data_yearly),96,7))
    
    for(i in 1:ncol(resid_data_yearly)){
        y = expectreg.ls(resid_data_yearly[,i]~rb(x,"pspline"),estimate="sheet",expectiles=c(0.01,0.05,0.25,0.5,0.75,0.95,0.99),smooth="acv") # ci=TRUE
        fit[i,,] = y$fitted
    }
    
    fpca = function(num){
        dataResid   = Data2fd(x,t(fit[,,num]))
        PCA_Resid   = pca.fd(dataResid,nharm=4,centerfns=TRUE)
        PC_Resid    = PCA_Resid$harmonics
        mean_Resid  = PCA_Resid$meanfd
        a_Resid     = PCA_Resid$scores
        varprop     = PCA_Resid$varprop
        return(list(PC_Resid,a_Resid,mean_Resid))
    }
    
    #################################################################################
    
    # TS object
    exo = cbind( resid_temp,resid_sun)
    colnames(exo) = c("Temp","Sun_L")
    
    for(j in 1:7){
        pca = fpca(j)
        endo=pca[2][[1]]
        colnames(endo)<-c("s1","s2","s3","s4")
        model = VAR(endo,exogen=exo,lag.max=30,type="const",ic="HQ")
    }
    return(list(ExplainedVar, )
}