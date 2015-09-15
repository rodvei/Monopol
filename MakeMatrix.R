sporsmaal=0
proovlykken=0
brettet=matrix(rep(0,40*40),40,40)
prob=c(1/36,2/36,3/36,4/36,5/36,6/36,5/36,4/36,3/36,2/36,1/36)
tempbrett=c(1:40,1:12)
for(i in 1:40){
  brettet[i,tempbrett[(i+2):(i+12)]]=prob
}
#Fengsel
brettet[30,]=brettet[10,]*(5/6)^9+(1-(5/6)^9)*c(rep(0,11),1/6,0,1/6,0,1/6,0,1/6,0,1/6,0,1/6,rep(0,18))

