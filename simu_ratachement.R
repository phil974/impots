# simulation ratachement
revenus1 <- 70455   #revenus des parents
revenus2 <- 20174+2954
revenus3 <- 2519   #revenus de l'enfant
pension <- 5947   # pension alimentaire versée plafond 5947 ?
#pension <- 00
FF <- filldeclarant(revenusalaires = revenus1)
FF$AJ[2] <- revenus2
FF$AJ[3] <- revenus3
impotsavec <- impots(FF,nbparts = 3, DOM =TRUE)$impots

# 9042
# 
FF <- filldeclarant(revenusalaires = revenus1)
FF$AJ[2] <- revenus2
FF$EL <- pension # pension alimentaire versée
impotssans <- impots(FF,nbparts = 2.5, DOM = TRUE)$impots
#7113
FF <- filldeclarant(revenusalaires = revenus3)
FF$AO <- pension # pension alimentaire reçue
impotsseul <- impots(FF,nbparts = 1, DOM = FALSE)$impots
#0
impotsavec
impotssans
impotsseul

# allocation logement
# avril mai juin
diff2T <- 3*(alloclogement(revenus3*3)-alloclogement(revenus3*3+pension))
#juillet aout septembre
diff3T <- 3*(alloclogement(revenus3*6)-alloclogement(revenus3*6+pension))
diffT <- impotsavec-impotssans-impotsseul-diff2T-diff3T

# variation du salaire
pension <- 5947
FF <- filldeclarant(revenusalaires = revenus1)
FF$AJ[2] <- revenus2
FF$EL <- pension # pension alimentaire versée
impotssans <- impots(FF,nbparts = 2.5, DOM = TRUE)$impots
diffS <- numeric(300)
for (i in 1:300){
    revenus3 <- 10*i
    FF <- filldeclarant(revenusalaires = revenus1)
    FF$AJ[2] <- revenus2
    FF$AJ[3] <- revenus3
    impotsavec <- impots(FF,nbparts = 3, DOM =TRUE)$impots
    FF <- filldeclarant(revenusalaires = revenus3)
    FF$AO <- pension # pension alimentaire reçue
    impotsseul <- impots(FF,nbparts = 1, DOM = FALSE)$impots
    diff2T <- 3*(alloclogement(revenus3*3)-alloclogement(revenus3*3+pension))
    diff3T <- 3*(alloclogement(revenus3*6)-alloclogement(revenus3*6+pension))
    diff4T <- 3*(alloclogement(revenus3*9)-alloclogement(revenus3*9+pension))
    diffS[i] <- impotsavec-impotssans-impotsseul-diff2T-diff3T-diff4T
    cat(i,impotsavec,impotssans,impotsseul,diff2T,diff3T,diff4T,"\n")
}
plot(1:300/100,diffA,type="l",xlab="revenus mensuels (k€)")
# variation de la pension
revenus3 <- 2400
FF <- filldeclarant(revenusalaires = revenus1)
FF$AJ[2] <- revenus2
FF$AJ[3] <- revenus3
impotsavec <- impots(FF,nbparts = 3, DOM =TRUE)$impots
diffP <- numeric(595)
for (i in 1:595){
    pension <- 10*i
    FF <- filldeclarant(revenusalaires = revenus1)
    FF$AJ[2] <- revenus2
    FF$EL <- pension # pension alimentaire versée
    impotssans <- impots(FF,nbparts = 2.5, DOM = TRUE)$impots
    FF <- filldeclarant(revenusalaires = revenus3)
    FF$AO <- pension # pension alimentaire reçue
    impotsseul <- impots(FF,nbparts = 1, DOM = FALSE)$impots
    diff2T <- 3*(alloclogement(revenus3*3)-alloclogement(revenus3*3+pension))
    diff3T <- 3*(alloclogement(revenus3*6)-alloclogement(revenus3*6+pension))
    diff4T <- 3*(alloclogement(revenus3*9)-alloclogement(revenus3*9+pension))
    diffP[i] <- impotsavec-impotssans-impotsseul-diff2T-diff3T-diff4T
#    cat(i,impotsavec,impotssans,impotsseul,diff2T,diff3T,diff4T,"\n")
}
plot(1:595/100,diffP,type="l",xlab="pension (k€)")
