sjanse=rep(0,40)
sjanse[c(1,5,11,24,30,39,40)]=1/8
provlykken=rep(0,40)
provlykken[c(30,40)]=0.5

sjanseloc=c(7,22,36)
provlykkenloc=c(2,17,33)

brettet=matrix(rep(0,40*40),40,40)
prob=c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36)
tempbrett=c(1:40,1:12)
for(i in 1:40){
  brettet[i,tempbrett[(i+2):(i+12)]]=prob
}
#Fengsel
brettet[30,]=brettet[10,]*(5/6)^9+(1-(5/6)^9)*c(rep(0,11),1/6,0,1/6,0,1/6,0,1/6,0,1/6,0,1/6,rep(0,18))


for(i in provlykkenloc){
  temp=which(brettet[,i]!=0)
  for(j in temp){
    brettet[j,]=brettet[j,]+brettet[j,i]*provlykken*2/16
    brettet[j,i]=brettet[j,i]*14/16
  }
}

for(i in sjanseloc){
  temp=which(brettet[,i]!=0)
  for(j in temp){
    sjansetemp=sjanse
    sjansetemp[i-3]=sjansetemp[i-3]+1/8
    brettet[j,]=brettet[j,]+brettet[j,i]*sjansetemp*8/16
    brettet[j,i]=brettet[j,i]*8/16
  }
}