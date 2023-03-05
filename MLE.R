#####################Mle of gamma alpha and beta unknown
mle=function(x){
  like=function(t){
    alpha=t[1]
    beta=t[2]
    Loglike<-sum(dgamma(x,alpha,beta,log = T))
    -Loglike
  }
  
}
x=c(22,23.9,20.9,23.8,25,24,21.7,23.8,22.8,23.1,23.1,23.5,23,23)
mle(x)
opt1=optim(c(.1,.1),mle(x))
MLE1=opt1$par
opt2=nlminb(c(.1,.1),mle(x))
MLE2=opt2$par
#####################Mle of gamma alpha  unknown

mle2=function(x){
  like=function(t){
    alpha=t[1]
    beta=sum(x)/(length(x)*alpha)
    Loglike<-sum(dgamma(x,alpha,beta,log = T))
    -Loglike
  }
  
}
x=c(22,23.9,20.9,23.8,25,24,21.7,23.8,22.8,23.1,23.1,23.5,23,23)
mle2(x)
opt=optim(c(1),mle2(x))
alpha=opt$par
beta=sum(x)/(length(x)*alpha)
opt.=nlminb(c(.1),mle2(x))
alpha2=opt.$par
beta2=sum(x)/(length(x)*alpha2)

