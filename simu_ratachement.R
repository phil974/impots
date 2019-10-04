# simulation ratachement
revenus <- 100000   #revenus des parents
revenus1 <- 9000   #revenus de l'enfant
pensions <- 5800   # pension alimentaire versée
FF <- filldeclarant(revenusalaires = revenus+revenus1)
impotsavec <- impots(FF,nbparts = 3, DOM = TRUE)

# 9042
FF <- filldeclarant(revenusalaires = revenus-pensions)
impotssans <- impots(FF,nbparts = 2.5, DOM = TRUE)
#7113
FF <- filldeclarant(revenusalaires = revenus1+pensions)
impotsseul <- impots(FF,nbparts = 1, DOM = FALSE)
#0
impotsavec$impots
impotssans$impots
impotsseul$impots
