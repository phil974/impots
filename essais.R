# essais impot
rmax=250
tabimpot=numeric(rmax)
tabimpot2=numeric(rmax)
tabimpot4=numeric(rmax)
for (i in 1:rmax) tabimpot[i]=impotsaides(i*100,arrondi=F)
for (i in 1:rmax) tabimpot2[i]=impotsaides(i*100,i*100,nbpers=2,arrondi=F)
for (i in 1:rmax) tabimpot4[i]=impotsaides(i*100,i*100,nbpers=4,arrondi=F)
rav = 1:rmax * 100 -tabimpot
rav2 = 1:rmax * 200 -tabimpot2
rav4 = 1:rmax * 200 -tabimpot4
plot(rav,tabimpot,type="l",
     #     xlim=range(rav,rav2/1.5,rav4/2.1),
     col=1)
lines(rav2/1.5,tabimpot2/2,col=2)
lines(rav4/2.1,tabimpot4/2,col=3)
plot(rav[2:rmax],diff(tabimpot),type="l",ylim=c(0,80),
#     xlim=range(rav,rav2/1.5,rav4/2.1),
     col=1)
lines(rav2[2:rmax]/1.5,diff(tabimpot2)/2,col=2)
lines(rav4[2:rmax]/2.1,diff(tabimpot4)/2,col=3)
legend("topright",legend=c("1","2","4"),col=1:3,lty=1)
#boxplot(list(diff(tabimpot),diff(tabimpot2/2),diff(tabimpot4/2)))
