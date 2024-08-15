
### ANTONAKAKIS, N., CHATZIANTONIOU, I., AND GABAUER, D. (2020)
### REFINED MEASURES OF DYNAMIC CONNECTEDNESS BASED ON TIME-VARYING PARAMETERS VECTOR AUTREGRESSIONS
### Journal of Risk and Financial Management
### by David Gabauer (https://sites.google.com/view/davidgabauer/contact-details)

UninformativePrior = function(gamma, r, nlag, m){
  A_prior = cbind(0*diag(r), matrix(0, ncol=(nlag-1)*r, nrow=r))
  aprior = c(A_prior)
  V_i = matrix(0, nrow=(m/r), ncol=r)
  for (i in 1:r){
    for (j in 1:(m/r)) {
      V_i[j,i] = gamma/(ceiling(j/r)^2)
    }
  }
  # Now V (MINNESOTA VARIANCE) is a diagonal matrix with diagonal elements the V_i'  
  V_i_T = t(V_i)
  Vprior = diag(c(V_i_T))
  diag(Vprior)
  return = list(aprior=aprior, Vprior=Vprior)
}

########################TVPVAR模型######################
TVPVAR = function(Y, l, nlag, beta_0.mean, beta_0.var, Q_0){
  create_RHS_NI = function(templag, r, nlag, t){
    K = nlag*(r^2)
    x_t = matrix(0, (t-nlag)*r, K)
    for (i in 1:(t-nlag)){
      ztemp=NULL
      for (j in 1:nlag){
        xtemp = templag[i,((j-1)*r+1):(j*r)]
        xtemp = t(kronecker(diag(r),xtemp))
        ztemp = cbind(ztemp, xtemp)
      }
      x_t[((i-1)*r+1):(i*r),] = ztemp
    }
    return=list(x_t=x_t, K=K)
  }
  Y = scale(Y,T,T)
  y_true = 0
  FPC = Y
  YX = cbind(Y,Y)
  nfac = 0
  p = n = ncol(Y)
  r = nfac + p
  m = nlag*(r^2)
  k = nlag*r
  t = nrow(FPC)
  q = n + p
  Q_0 = Q_0
  
  # Initialize matrices
  beta_0_prmean = beta_0.mean
  beta_0_prvar = beta_0.var
  
  beta_pred = matrix(0,m,t)
  beta_update = matrix(0,m,t)
  
  Rb_t = array(0,c(m,m,t))
  Sb_t = array(0,c(m,m,t))
  
  beta_t = array(0, c(k,k,t))
  Q_t = array(0, c(r,r,t))
  
  # Decay and forgetting factors
  l_2 = l[1]
  l_4 = l[2]
  
  # Define lags of the factors to be used in the state (VAR) equation         
  yy = FPC[(nlag+1):t,]      
  xx = embed(FPC,nlag+1)[,-c(1:ncol(FPC))]
  templag = embed(FPC,nlag+1)[,-c(1:ncol(FPC))]
  RHS1 = create_RHS_NI(templag,r,nlag,t);  
  Flagtemp = RHS1$x_t
  m = RHS1$K
  Flag = rbind(matrix(0, k,m), Flagtemp)
  
  ###-----| 1. KALMAN FILTER
  for (irep in 1:t){
    #-----| Update the state covariances
    # 1. Get the variance of the factor
    
    # Update Q[t]
    if (irep==1){
      Q_t[,,irep] = Q_0
    } else if (irep > 1) {
      if (irep <= (nlag+1)) { 
        Gf_t = 0.1*(t(matrix(FPC[irep,],nrow=1))%*%(FPC[irep,]))
      } else {
        Gf_t = t(yy[(irep-nlag),]-xx[(irep-nlag),]%*%t(B[1:r,1:k])) %*% (yy[(irep-nlag),]-xx[(irep-nlag),]%*%t(B[1:r,1:k]))
      }
      Q_t[,,irep] = l_2*Q_t[,,(irep-1)] + (1-l_2)*Gf_t[1:r,1:r]
    }
    # -for beta
    if (irep <= (nlag+1)) {
      beta_pred[,irep] = beta_0_prmean
      beta_update[,irep] = beta_pred[,irep]
      Rb_t[,,irep] = beta_0_prvar
    } else if (irep > (nlag+1)) {
      beta_pred[,irep] = beta_update[,(irep-1)]
      Rb_t[,,irep] = (1/l_4)*Sb_t[,,(irep-1)]
    }
    
    # -for beta
    if (irep >= (nlag+1)) {
      # 2/ Update VAR coefficients conditional on Principal Componets estimates
      Rx = Rb_t[,,irep]%*%t(Flag[((irep-1)*r+1):(irep*r),])
      KV_b = Q_t[,,irep] + Flag[((irep-1)*r+1):(irep*r),]%*%Rx
      KG = Rx%*%MASS::ginv(KV_b)
      beta_update[,irep] = matrix(beta_pred[,irep], ncol=1) + (KG%*%(t(matrix(FPC[irep,], nrow=1))-Flag[((irep-1)*r+1):(irep*r),]%*%matrix(beta_pred[,irep], ncol=1)) )
      Sb_t[,,irep] = Rb_t[,,irep] - KG%*%(Flag[((irep-1)*r+1):(irep*r),]%*%Rb_t[,,irep])
    }
    
    # Assign coefficients
    bb = matrix(beta_update[,irep], ncol=1)
    splace = 0
    biga = matrix(0, r,r*nlag)
    for (ii in 1:nlag) {                                          
      for (iii in 1:r) {           
        biga[iii,((ii-1)*r+1):(ii*r)] = t(bb[(splace+1):((splace+r)),1])
        splace = splace + r
      }
    }
    
    B = rbind(biga, cbind(diag(r*(nlag-1)), matrix(0, nrow=r*(nlag-1), ncol=r)))
    
    if ((max(abs(eigen(B)$values))<=1)||(irep==1)){
      beta_t[,,irep] = B
    } else {
      beta_t[,,irep] = beta_t[,,(irep-1)]
      beta_update[,irep] = 0.99*beta_update[,(irep-1)]
    }
  }
  
  return = list(beta_t=beta_t[1:ncol(Y),,], Q_t=Q_t)
}

#######################TVPPhi#######################
tvp.Phi = function (x, nstep = 10, ...) {
  nstep = abs(as.integer(nstep))
  K=nrow(x)
  p=floor(ncol(x)/K)
  A = array(0, c(K,K,nstep))
  for (i in 1:p){
    A[,,i]=x[,((i-1)*K+1):(i*K)]
  }
  
  Phi = array(0, dim = c(K, K, nstep + 1))
  Phi[, , 1] = diag(K)
  Phi[, , 2] = Phi[, , 1] %*% A[, , 1]
  if (nstep > 1) {
    for (i in 3:(nstep + 1)) {
      tmp1 = Phi[, , 1] %*% A[, , i - 1]
      tmp2 = matrix(0, nrow = K, ncol = K)
      idx = (i - 2):1
      for (j in 1:(i - 2)) {
        tmp2 = tmp2 + Phi[, , j + 1] %*% A[, , idx[j]]
      }
      Phi[, , i] = tmp1 + tmp2
    }
  }
  return(Phi)
}
#################TVP VAR 方差分解##########################
tvp.gfevd = function(model, Sigma, n.ahead=10,normalize=TRUE,standardize=TRUE) {
  A = tvp.Phi(model, (n.ahead-1))
  Sigma = Sigma
  gi = array(0, dim(A))
  sigmas = sqrt(diag(Sigma))
  for (j in 1:dim(A)[3]) {
    gi[,,j] = t(A[,,j]%*%Sigma%*%MASS::ginv(diag(sqrt(diag(Sigma)))))
  }
  if (standardize==TRUE){
    girf=array(NA, c(dim(gi)[1],dim(gi)[2], (dim(gi)[3])))
    for (i in 1:dim(gi)[3]){
      girf[,,i]=((gi[,,i])%*%MASS::ginv(diag(diag(gi[,,1]))))
    }
    gi=girf
  }
  
  num = apply(gi^2,1:2,sum)
  den = c(apply(num,1,sum))
  fevd = t(num)/den
  nfevd = fevd
  if (normalize==TRUE) {
    fevd=(fevd/apply(fevd, 1, sum))
  } else {
    fevd=(fevd)
  }
  return = list(fevd=fevd, girf=gi, nfevd=nfevd)
}


####################DCA 溢出################################
DCA = function(CV){
  k = dim(CV)[1]
  SOFM = apply(CV,1:2,mean)*100 # spillover from others to one specific
  VSI = round(mean(100-diag(SOFM)),2)
  TO = colSums(SOFM-diag(diag(SOFM)))
  FROM = rowSums(SOFM-diag(diag(SOFM)))
  NET = TO-FROM
  NPSO = SOFM-t(SOFM)
  INC = rowSums(NPSO>0)
  ALL = rbind(format(round(cbind(SOFM,FROM),1),nsmall=1),c(format(round(TO,1),nsmall=1),format(round(sum(colSums(SOFM-diag(diag(SOFM)))),1),nsmall=1)),c(format(round(NET,1),nsmall=1),"TCI"),format(round(c(INC,VSI),1),nsmall=1))
  colnames(ALL) = c(rownames(CV),"FROM")
  rownames(ALL) = c(rownames(CV),"Contribution TO others","NET directional connectedness","NPDC transmitter")
  return = list(CT=SOFM,TCI=VSI,TO=TO,FROM=FROM,NET=NET,NPSO=NPSO,NPDC=INC,ALL=ALL)
}
########################################################################
########################################################################
path = file.path(file.choose()) # select dy2012.csv
DATA = read.csv(path)
date = as.Date(as.character(DATA[,1]))
Y = DATA[,-1]
k = ncol(Y)

### TVP-VAR
nlag = 3 # VAR(4)
nfore = 10 # 10-step ahead forecast
m = nlag*(k^2)
t = nrow(Y)
l_1 = 0.99
l_2 = 0.99

prior = UninformativePrior(0.1, k, nlag, m)
beta_0.mean = prior$aprior
beta_0.var = prior$Vprior

tvpvar = TVPVAR(Y,l=c(l_1, l_2),nlag,beta_0.mean,beta_0.var,cov(Y))
B_t = tvpvar$beta_t
Q_t = tvpvar$Q_t

### DYNAMIC CONNECTEDNESS APPROACH
to = matrix(NA, ncol=k, nrow=t)
from = matrix(NA, ncol=k, nrow=t)
net = matrix(NA, ncol=k, nrow=t)
ct = npso = array(NA, c(k, k, t))
total = matrix(NA, ncol=1, nrow=t)
colnames(npso)=rownames(npso)=colnames(ct)=rownames(ct)=colnames(Y)
for (i in 1:t){
  CV = tvp.gfevd(B_t[,,i], Q_t[,,i], n.ahead=nfore)$fevd
  colnames(CV)=rownames(CV)=colnames(Y)
  vd = DCA(CV)
  ct[,,i] = vd$CT
  to[i,] = vd$TO/k
  from[i,] = vd$FROM/k
  net[i,] = vd$NET/k
  npso[,,i] = vd$NPSO/k
  total[i,] = vd$TCI
}

nps = array(NA,c(t,k/2*(k-1)))
colnames(nps) = 1:ncol(nps)
jk = 1
for (i in 1:k) {
  for (j in 1:k) {
    if (j<=i) {
      next
    } else {
      nps[,jk] = npso[i,j,]
      colnames(nps)[jk] = paste0(colnames(Y)[j],"-",colnames(Y)[i])
      jk = jk + 1
    }
  }
}

### DYNAMIC TOTAL CONNECTEDNESS
par(mfrow = c(1,1), oma = c(0,1,0,0) + 0.05, mar = c(1,1,1,1) + .05, mgp = c(0, 0.1, 0))
plot(date,total, type="l",xaxs="i",col="grey20", las=1, main="",ylab="",ylim=c(floor(min(total)),ceiling(max(total))),yaxs="i",xlab="",tck=0.01)
grid(NA,NULL,lty=1)
polygon(c(date,rev(date)),c(c(rep(0,nrow(total))),rev(total)),col="grey20", border="grey20")
box()
print(DCA(ct/100)$ALL)
sink("a.txt") 
          
### TOTAL DIRECTIONAL CONNECTEDNESS TO OTHERS
par(mfrow = c(ceiling(k/2),2), oma = c(0,1,0,0) + 0.02, mar = c(1,1,1,1) + .02, mgp = c(0, 0.1, 0))
for (i in 1:k){
  plot(date,to[,i], xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1, main=paste(colnames(Y)[i],"TO all others"),ylim=c(floor(min(to)),ceiling(max(to))),tck=0.01,yaxs="i")
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(to))),rev(to[,i])),col="grey20", border="grey20")
  box()
}

### TOTAL DIRECTIONAL CONNECTEDNESS FROM OTHERS
par(mfrow = c(ceiling(k/2),2), oma = c(0,1,0,0) + 0.02, mar = c(1,1,1,1) + .02, mgp = c(0, 0.1, 0))
for (i in 1:k){
  plot(date,from[,i], xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1, main=paste(colnames(Y)[i],"FROM all others"),ylim=c(floor(min(from)),ceiling(max(from))),tck=0.01,yaxs="i")
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(from))),rev(from[,i])),col="grey20", border="grey20")
  box()
}

### NET TOTAL DIRECTIONAL CONNECTEDNESS
par(mfrow = c(ceiling(k/2),2), oma = c(0,1,0,0) + 0.05, mar = c(1,1,1,1) + .05, mgp = c(0, 0.1, 0))
for (i in 1:k){
  plot(date,net[,i], xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1, main=paste("NET",colnames(Y)[i]),ylim=c(floor(min(net)),ceiling(max(net))),tck=0.01,yaxs="i")
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(net))),rev(net[,i])),col="grey20", border="grey20")
  box()
}

### NET PAIRWISE DIRECTIONAL CONNECTEDNESS
par(mfrow = c(ceiling(ncol(nps)/2),2), oma = c(0,1,0,0) + 0.05, mar = c(1,1,1,1) + .05, mgp = c(0, 0.1, 0))
for (i in 1:ncol(nps)) {
  plot(date,nps[,i], xlab="",ylab="",type="l",xaxs="i",col="grey20", las=1, main=colnames(nps)[i],tck=0.02,yaxs="i",ylim=c(floor(min(nps)),ceiling(max(nps))))
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(nps))),rev(nps[,i])),col="grey20", border="grey20")
  box()
}

### AVERAGE DYNAMIC CONNECTEDNESS TABLE
print(DCA(ct/100)$ALL)

### END
write.csv(to,"C:/Users/lenovo/Desktop/最终版程序啊啊啊/1111/to.csv")
write.csv(from,"C:/Users/lenovo/Desktop/最终版程序啊啊啊/1111/from.csv")