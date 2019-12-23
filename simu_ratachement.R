# simulation ratachement
revenus1 <- 70815   #revenus des parents
revenus2 <- 21290
revenus3 <- 600   #revenus de l'enfant
pensions <- 5947   # pension alimentaire versée
FF <- filldeclarant(revenusalaires = revenus1)
FF$AJ[2] <- revenus2
FF$AJ[3] <- revenus3
impotsavec <- impots(FF,nbparts = 3, DOM = TRUE)

# 9042
# 
FF <- filldeclarant(revenusalaires = revenus1)
FF$AJ[2] <- revenus2
FF$EL <- pensions # pension alimentaire versée
impotssans <- impots(FF,nbparts = 2.5, DOM = TRUE)
#7113
FF <- filldeclarant(revenusalaires = revenus3)
FF$AO <- pensions # pension alimentaire reçue
impotsseul <- impots(FF,nbparts = 1, DOM = FALSE)
#0
impotsavec$impots
impotssans$impots
impotsseul$impots
