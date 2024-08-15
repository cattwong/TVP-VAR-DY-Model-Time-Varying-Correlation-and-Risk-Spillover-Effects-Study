###导入数据###

library(openxlsx)
data=read.xlsx('C:/Users/lenovo/Desktop/092003413/workdata.xlsx')
library(tidyverse)
library(ggthemes)
library(forecast)
library(tseries)
library(gridExtra)
library(rugarch)
library(FinTS)



#转为时间序列
data_ts=ts(data)
ssec=ts(data$ssec)
ine=ts(data$ine)
wti=ts(data$wti)


#绘制收益率时间序列图
#install.packages('zoo')
#install.packages('base')
library(zoo)
plot(ssec)
plot(ine)
plot(wti)

#计算相关系数矩阵
pcor=cor(data_ts)
pcor

#描述性统计（std.dev：标准差；Variance：方差；Skewness：偏度；Kurtosis：峰度）
#install.packages('moments')
library(moments)
summary(ine)
var(ine)
sd(ine)
skewness(ine)
kurtosis(ine)

summary(ssec)
var(ssec)
sd(ssec)
skewness(ssec)
kurtosis(ssec)

summary(wti)
var(wti)
sd(wti)
skewness(wti)
kurtosis(wti)

#shapiro正态检验，p值越小，越拒绝原假设，表明序列不服从正态分布。原假设H0：数据服从正态分布
#JB检验
shapiro.test(ine)
shapiro.test(ssec)
jarque.bera.test(ine)
jarque.bera.test(ssec)
jarque.bera.test(wti)

#ADF单位根检验，原假设序列存在单位根，当p值越小越拒绝原假设，可以得出该序列平稳。若不平稳，需在进行差分处理。
#install.packages('tseries')
#install.packages("quantmod")
library(tseries)
adf.test(ine)
adf.test(ssec)
adf.test(wti)


###ARMA定阶部分###
auto.arima(ssec)   
auto.arima(ine)  
auto.arima(wti)
arma_ssec=arima(ssec,order=c(4,0,0))    
arma_ine=arima(ine,order=c(3,0,1))
arma_wti=arima(ine,order=c(3,0,4))
arma_ssec_resd=arma_ssec$residuals     #获得残差数据，下同
arma_ine_resd=arma_ine$residuals   
arma_wti_resd=arma_wti$residuals  
resd_series=cbind(arma_ssec_resd,arma_ine_resd,arma_wti_resd)   #得到两个序列在ARMA模型下的残差序列
data_matrix=cbind(ssec,ine,wti)

#提取拟合ARIMA模型的残差项，对其进行LB检验来判断残差项是否独立不相关。原假设H0：独立不相关。p值越小越拒绝，当p值较大时，则认为序列不相关。
Box.test(residuals(arma_ssec),lag=4,type = "Ljung-Box")
Box.test(residuals(arma_wti),lag=4,type = "Ljung-Box")
Box.test(residuals(arma_ine),lag=4,type = "Ljung-Box")
#进一步用LB检验残差项的平方是否存在自相关性，检验p值很小拒绝原假设，表明存在自相关性，满足建立GARCH模型的前提条件。
Box.test(residuals(arma_ssec)^2,lag=4)
Box.test(residuals(arma_wti)^2,lag=4)
Box.test(residuals(arma_ine)^2,lag=4)

#收益率序列拟合后的ARMA模型残差会表现出异方差性。
#此时需要用ARCH检验来判断扰动项的条件方差对它前期方差依赖的程度，确保该序列可以建立GARCH 模型。
#install.packages('FinTS')
#install.packages('forecast')
library(FinTS)
ArchTest(residuals(arma_ssec),lag=4)
ArchTest(residuals(arma_wti),lag=4)
ArchTest(residuals(arma_ine),lag=4)

###构建单序列ARMAGARCH模型部分###并保存garch模型拟合得出的波动率用于tvp-var-dy模型分析
##SSEC##

model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) , 
                        mean.model = list(armaOrder = c(4 , 0)))

model.fit = ugarchfit(spec = model.spec , data = ssec , solver = 'solnp')

options(scipen = 999)
model.fit@fit$matcoef


module_ssec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                                garchOrder = c(1, 1), 
                                                submodel = NULL, 
external.regressors = NULL, 
                                                variance.targeting = FALSE), 
                          
                          mean.model     = list(armaOrder = c(4, 0), 
                                                external.regressors = NULL, 
                                                start.pars = list(), 
                                                fixed.pars = list()),
                          distribution.model = "sstd")   #选择偏t分布
fitgarch_ssec=ugarchfit(module_ssec,data=ssec,solver="nlminb")        #偏tGARCH（1,1）
fitgarch_ssec
ssec.garch <- garch(ssec)
ssec.garch$fitted.values
write.xlsx(ssec.garch$fitted.values, "C:/Users/lenovo/Desktop/092003413/ssec.xlsx")

##INE##
model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) , 
                        mean.model = list(armaOrder = c(3 , 1)))

model.fit = ugarchfit(spec = model.spec , data = ine , solver = 'solnp')

options(scipen = 999)
model.fit@fit$matcoef


module_ine<- ugarchspec(variance.model = list(model = "sGARCH", 
                                                  garchOrder = c(1, 1), 
                                                  submodel = NULL, 
                                                  external.regressors = NULL, 
                                                  variance.targeting = FALSE), 
                            
                            mean.model     = list(armaOrder = c(3, 1), 
                                                  external.regressors = NULL, 
                                                  start.pars = list(), 
                                                  fixed.pars = list()),
                            distribution.model = "sstd")  #选择偏t分布
fitgarch_ine=ugarchfit(module_ine,data=ine,solver="nlminb")          #偏tGARCH（1,1）
fitgarch_ine
ine.garch <- garch(ine)
ine.garch$fitted.values
write.xlsx(ine.garch$fitted.values, "C:/Users/lenovo/Desktop/092003413/ine.xlsx")

##WTI##

model.spec = ugarchspec(variance.model = list(model = 'sGARCH' , garchOrder = c(1 , 1)) , 
                        mean.model = list(armaOrder = c(3 , 4)))

model.fit = ugarchfit(spec = model.spec , data = wti , solver = 'solnp')

options(scipen = 999)
model.fit@fit$matcoef

module_wti<- ugarchspec(variance.model = list(model = "sGARCH", 
                                              garchOrder = c(1, 1), 
                                              submodel = NULL, 
                                              external.regressors = NULL, 
                                              variance.targeting = FALSE), 
                        
                        mean.model     = list(armaOrder = c(3, 4), 
                                              external.regressors = NULL, 
                                              start.pars = list(), 
                                              fixed.pars = list()),
                        distribution.model = "sstd")  #选择偏t分布
fitgarch_wti=ugarchfit(module_wti,data=wti,solver="nlminb")          #偏tGARCH（1,1）
fitgarch_wti
wti.garch <- garch(wti)
wti.garch$fitted.values
write.xlsx(wti.garch$fitted.values, "C:/Users/lenovo/Desktop/092003413/wti.xlsx")

