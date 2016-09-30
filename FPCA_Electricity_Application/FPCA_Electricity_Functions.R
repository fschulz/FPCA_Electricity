fourier.series = function(t,terms,period)
{
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

# dsc = function(Load_list, WD, PH, n, k, hours){
#   PH = PH[1:n,]
#   DT_Load = array(NA, dim = length(Load_list))
#   season_forecast = matrix(ncol=1,nrow=hours)
#   for(i in 1:hours){
#     index = as.numeric(seq(from=i, to=(n+k)*hours, by=hours))
#     temp = Load_list[index]
#     temp_date = Load_mat[index,1]
#     t = 1:length(temp)
#     ltsc = lm(temp ~ t + WD[-(n+k+1),] + PH[-(n+k+1),]  + fourier.series(t,4,365.25))
#     DT_Load[index] = ltsc$residuals
# 
#   }
#   return(DT_Load)
# }

dsc = function(Load_list, WD, PH, n, k, hours, p){
  DT_Load = array(NA, dim = length(Load_list))
  season_forecast = matrix(ncol = 1, nrow = hours)
  for(i in 1:hours){
    index = as.numeric(seq(from = i, to = (n + k) * hours, by = hours))
    temp = Load_list[index]
    temp_date = Load_mat[index,1]
    t = 1:length(temp)
    ltsc = lm(temp ~ t + WD[1:(n+k),] + PH[1:(n+k),]  + fourier.series(t,4,365.25))
    DT_Load[index] = ltsc$residuals
    new = as.data.frame(t(c(1, (max(t)+p), WD[(n+k+p),], PH[(n+k+p),], fourier.series((max(t)+p), 4, 365.25))))
    coeff = ltsc$coefficients
    season_forecast[i,1]=sum(coeff * new)
  }
  return(list(DT_Load,season_forecast))
}


fpca = function(num, fit, x){
  dataResid   = Data2fd(x,t(fit[,,num]))
  PCA_Resid   = pca.fd(dataResid,nharm=4,centerfns=TRUE)
  return(PCA_Resid)
}

plot.PC = function(ind, num, result){
  plot(result[1,][[num]][ind], ylab = paste("PC" ,ind), xlab = "Time of day", cex.lab = 2, cex.axis = 2,lwd = 3, ylim = c(-2,2), xaxt = 'n')
  abline(h = 0, lty = 2)
  axis(1,c(0,0.25,0.5,0.75,1), c("00:00","06:00","12:00","18:00","24:00"), cex.axis = 1.5)
}

plot.score = function(ind, num, result){
  plot(result[3,][[num]][,ind], ylab = paste(expression(alpha) ,ind), xlab = "Time", cex.lab = 2, cex.axis = 2,lwd = 3, type= "l")
}

season.cov = function(x, days){
  # deseasonalizes covariates
  year = 1:days
  mat  = as.matrix(x)
  mod_year = days-length(x)%%days
  if(mod_year==max(year)){
    mod_year = 0
  }
  mat = rbind(mat, matrix(NaN, nrow= mod_year, ncol=1) )
  YN1 = matrix(data=mat,nrow=days,byrow=FALSE)
  Y1  = rowMeans(YN1,na.rm = TRUE) #mean over years of hourly demand
  hs  = h.select(year,Y1, method="cv")
  s   = sm.regression(year,Y1,hs,eval.points=year, display='none')
  season1  = s$estimate
  season11 = rep(season1,dim(mat)[1]/days)
  resid    = (mat-season11)[1:(length(mat)-mod_year)]
  return(resid)
}

var.model = function(num, scores, exo){
  endo = scores[num][[1]]
  colnames(endo)<-c("s1","s2","s3","s4")
  model = VAR(endo, exogen = exo, lag.max = 30, type = "const", ic = "AIC")
  Temp  = sapply(X = 1:4, FUN = function(ind){coef(model)[[ind]]["Temperature",]})
  Sun   = sapply(X = 1:4, FUN = function(ind){coef(model)[[ind]]["Sunshine",]})
  return(list(Temp, Sun))
}

var.forecast = function(num, scores, exo, exo_fore, p){
  endo = scores[num][[1]]
  colnames(endo)<-c("s1","s2","s3","s4")
  model = VAR(endo, exogen = exo, lag.max = 30, type = "const", ic = "AIC")
  forecast = predict(model, n.ahead=p, dumvar=exo_fore)
  return(forecast)
}

forecast.curve = function(num, result, forecast.score, season_forecast, p){
  a_hat1 = forecast.score[[num]]$fcst$s1[p,1]
  a_hat2 = forecast.score[[num]]$fcst$s2[p,1]
  a_hat3 = forecast.score[[num]]$fcst$s3[p,1]
  a_hat4 = forecast.score[[num]]$fcst$s4[p,1]
  eval_points = seq(1/96,1,length=96)
  quant_fore = (eval.fd(eval_points,result[1,][[num]][1])*a_hat1+eval.fd(eval_points,result[1,][[num]][2])*
                  a_hat2+eval.fd(eval_points,result[1,][[num]][3])*a_hat3+eval.fd(eval_points,result[1,][[num]][4])*a_hat4)#+eval.fd(eval_points,pca[[3]])+
  forecast = quant_fore+season_forecast[k+p,]
  return(forecast)
}

EVAL = function(Fore, Load, h, n, p){
  MSE  = matrix(ncol = 96, nrow = (h))
  MAPE = matrix(ncol = 96, nrow = (h))
  for(k in 0:(h-p)){
    MSE[k+1,] = (Fore[(k+1),]-as.numeric(Load[(n+k+1),]))^2
    MAPE[k+1,] = t(abs(Fore[(k+1),]-as.numeric(Load[(n+k+1),]))/(as.numeric(Load[(n+k+1),])))
  }
  result = matrix(c(mean(sqrt(MSE),na.rm=TRUE), mean(MAPE,na.rm=TRUE)),ncol=2)
  colnames(result) = c("RMSE", "MAPE")
  return(result) # list(mean(sqrt(MSE),na.rm=TRUE), mean(MAPE,na.rm=TRUE))
}

MSE.FDA = function(num, Fore, Load, tau, n, h, p){
  Load_mat_reduced = as.matrix(Load[(n+1):(n+h),-1])
  Diff_Mat = Load_mat_reduced - as.matrix(Fore[,,num])
  MSE_FDA =  abs(tau[num]-(Diff_Mat<=0))*Diff_Mat^2
  mean = mean(sqrt(MSE_FDA),na.rm=TRUE)
  sd = sd(sqrt(rowMeans(MSE_FDA)),na.rm=TRUE)
  result = rbind(mean, sd)
  return(result)
}
