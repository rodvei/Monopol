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

#prov lykken kort
for(i in provlykkenloc){
  temp=which(brettet[,i]!=0)
  for(j in temp){
    brettet[j,]=brettet[j,]+brettet[j,i]*provlykken*2/16
    brettet[j,i]=brettet[j,i]*14/16
  }
}

#sjanse kort
for(i in sjanseloc){
  temp=which(brettet[,i]!=0)
  for(j in temp){
    sjansetemp=sjanse
    sjansetemp[i-3]=sjansetemp[i-3]+1/8
    brettet[j,]=brettet[j,]+brettet[j,i]*sjansetemp*8/16
    brettet[j,i]=brettet[j,i]*8/16
  }
}


brettetn=brettet%*%brettet #brettetn,n=2
brettetn=brettetn%*%brettetn #brettetn,n=4
brettetn=brettetn%*%brettetn #brettetn,n=8
brettetn=brettetn%*%brettetn #brettetn,n=16
brettetn=brettetn%*%brettetn #brettetn,n=32
brettetn=brettetn%*%brettetn #brettetn,n=64
brettetn=brettetn%*%brettetn #brettetn,n=128

plot(1:40, brettetn[1,],type='h',xaxt='n')
axis(1, at = 1:40, las=2)



#gate type
tomt=list(brun=c(1,3), lblaa=c(6,8,9), rosa=c(11,13,14), orange=c(16,18,19), rod=c(21,23,24), gul=c(26,27,29), gronn=c(31,32,34),mblaa=c(37,39))
name=c('brun','lblaa','rosa','orange','rod','gul','gronn','mblaa')
tomtint=rep(0,8)
for(i in 1:length(tomt)){
  tomtint[i]=sum(brettetn[1,tomt[[i]]])/length(tomt[[i]])
}
plot(1:8,tomtint,type='h',xaxt='n')
axis(1, 1:8, name)
