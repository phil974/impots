# calcul IRPP
# il faut charger données et barème
# par ex load("2014")
# ne tient pas compte de la contribution execptionnelle
# qui commence pour rfr >250 k€ pour célibataire
# et 500 k€ pour couple et famille
# FF foyer fiscal
#list(  
#    AJ = revenusalaires,
#    BDC = dividendes,
#    CVG = plusvalues,
#    AK = fraispro)
# min_per et arrondi peuvent être mis à FALSE pour augmenter la régularité de l'impots
# (mais le calcul est alors faux...)
impots <- function(FF,nbparts=1,DOM=FALSE,min_per=TRUE,arrondi = TRUE)
with(di,{
# calcul des frais pro : AK
for(i in 1:length(FF$AJ))
    # si pas frais réel faire le calcul
    if (is.na(FF$AK[i])){
        if(FF$AJ[i]>0) {
        FF$AK[i] <- min(plafondFraisPro,0.1*FF$AJ[i])
        FF$AK[i] <- max(FF$AK[i],plancherFraisPro)
        }
        else FF$AK[i] <- 0
    }
# abbatement de 40 % sur dividendes pour revenunet mais pas rfr     
revenunet <- sum(FF$AJ-FF$AK)+0.6*FF$BDC+FF$CVG
FF$rfr <- sum(FF$AJ-FF$AK)+FF$BDC+FF$CVG
#cat(revenunet,rfr,"\n")
# pas d'enfant
if (nbparts <= 2){
    impots <- nbparts*impotsparpart(revenunet/nbparts)
}
else
# présence d'enfants 2 calculs à cause du plafonnement de l'avantage du QF
{
    impots1 <-  nbparts*impotsparpart(revenunet/nbparts)
    impots2 <- 2*impotsparpart(revenunet/2)-2*(nbparts-2)*plafondDemiPart
    impots <- max(impots1,impots2)
}
# reduction DOM
if(DOM) impots <- impots - min(0.3*impots,plafondDom)

# celibataire ou couple ?
if (nbparts == 1)
    i = 1
else i = 2

# decote : changement de systeme entre 2014 et 2015
# encore un nouveau systeme en 2016...
if (annee == 2016){
    if (impots < decote[i]/0.75)
        impots <- max(1.75*impots - decote[i],0)   
    if(FF$rfr <i*20500 +(nbparts-i)*7400){
        
        if (FF$rfr <i*18500 +(nbparts-i)*7400)
            impots <- impots *0.8
        else{
            txreduc <- 0.2 * (i*20500 +(nbparts-i)*7400 - FF$rfr)/(2000*i)
            impots <- impots * (1-txreduc)
        }
    }
}
if (annee == 2015){
    if (impots < decote[i]/0.75)
    impots <- max(1.75*impots - decote[i],0)
}
if (annee == 2014){
    if (impots < decote[i])
        impots <- max(2*impots - decote[i],0) 
}
# arrondi ?
if (arrondi)
    impots = round(impots)
# minimum perception
if(min_per)
    if(impots < minPersception)
            impots = 0
FF$impots <- impots
FF
})

impotsparpart <- function(QF)
with(di,{
    impot = 0   
#    i=2
#    while(QF >= bareme[1,i-1]){
    for (i in 2:ncol(bareme)){
    ipt <- min(bareme[1,i],QF) - bareme[1,i-1]
        impot <- impot + max(ipt,0) * bareme[2,i]/100
#    impot <- impot + ipt * bareme[2,i]/100
#    i <- i+1
    }
    impot
})
#
filldeclarant <- function(revenusalaires = 0, dividendes = 0, plusvalues = 0,
                   fraispro = NA){
    list(  
    AJ = revenusalaires,
    BDC = dividendes,
    CVG = plusvalues,
    AK = fraispro)
}
# rsa bareme 2015
# pour 2014 rsa moyen facteur 
# rsasocle=502.63
# http://vosdroits.service-public.fr/particuliers/F502.xhtml
#données de 1 à 5 personnes (célibataire, couple, famille)
# ressources par an et rsa par mois 
# alloc : allocations  perçues
#
rsa <- function(ressources,nbpers=1,alloc=0,logement=0){
    ressources = ressources/12
    coef=switch(nbpers,1,1.5,1.8,2.1,2.5,2.9)
    montant = max(0,di$rsasocle*coef - 0.38*ressources)
    if(alloc >0) montant = montant - alloc
    if(logement>0) 
        montant <- montant 
            - min(logement,di$forfaitlogement[min(3,nbpers)])
    if(montant < 6) montant = 0
    montant
}
# allocations familliales
# entrées :
# enfants    : vecteur contenant l'age des enfants
# ressources : ressources annuelles du foyer fiscal
# sortie :
# montant par mois
allocf <- function(ressources = 0,enfants){
# abbatement de 10%    
r=0.9*ressources
nb = length(enfants)
# si moins de 2 enfants rien
if (nb < 2) return(0)
# nb enfants de plus de 14 ans
nb14 = sum(enfants >=14)
# calcul du montant (le tableau contient 2, 3 et par enfant supplémentaire)
montant = di$alloc[min(3,nb)]
if (nb>3) montant = montant + (nb-3)*di$alloc[4]
# majoration + de 14 ans
# si 2 enfants majoration pour plus de 14 ans uniquement pour le 2ieme
if (nb14 & nb==2) nb14 = nb14-1
montant = montant + nb14*di$allocsup
#reduction si dépacement du plafond
plafond4 = di$plafondalloc4+nb/10*di$plafondalloc2
plafond2 = di$plafondalloc2+nb/10*di$plafondalloc2
if (r>plafond4+3*montant) return(montant/4)
if (r>plafond4) return(montant/4 + (plafond4+3*montant-r)/12)
if (r>plafond2+6*montant) return(montant/2)
if (r>plafond2) return(montant/2 + (plafond2+6*montant-r)/12)

montant
}
# allocation logement uniquement secteur locatif
# donnees 2015
# resources par an allocation par mois
# DOM est la uniquement pour les ... dans fct impotsrsa
alloclogement <- function(ressources,nbpers=1,loyer=Inf,zone = 2,
    arrondi = TRUE,DOM){
# l    
    l2=di$plafondloyerzone[zone,min(3,nbpers)]
    if(nbpers>3) 
        l2 = l2 + di$plafondloyerzone[zone,4]*(nbpers-3)
    l = min(loyer,l2)
# c
    if(nbpers <=2)
        c=di$cc
    else
        c=di$cc + (nbpers-2)*di$ccsup
# Rp    
    if (arrondi) r = 100*ceiling(0.9*ressources/100)
    else r=0.9*ressources
    r0 = di$R0[nbpers]
    Rp = max(0,r-r0)
# Pp    
    p0 = min((l+c)*0.085,di$p0)
    tf = di$tf[nbpers]
    lr = di$lr[min(3,nbpers)]
    if(nbpers >3)
        lr = lr + di$lr[4]*(nbpers-3)
    rl =round((l/lr)*100,2)
    if(rl<45) tl = 0
    if(rl<75) tl = 0.45*(rl-45)
    else tl = 0.68*rl-37.5
    tl = tl/100
    Pp = round((tf+tl)/100*Rp+p0,2)
# montant
    montant = l+c-Pp
    if(montant<15) montant=0
    else montant = round(0.995*montant,2)
    montant
}
# bourse CROUS
# uniquement dans le cas de 6 points de charge pour l'instant
# entrée nombre d'étudiants
# resourses : ressources annuelles du foyer
# sortie montant bourse + montant droit inscription L si bourse
# 0 sinon
allocrous <- function(ressources,nbetu=0){
# abattement 10%
# normalement c'est le revenu brut global du foyer    
    r = 0.9*ressources
    montant = 0
    i=1
    while (r <=di$BoursePlafond4[i] & i<=9)
        i=i+1
    if (i>1)
        montant = nbetu*(di$droitsinscriptions + di$BourseMontant[i-1])
    montant
}
# revenusalaire par an
impotsaides <- function(revenusalaires1,revenusalaires2=0,nbpers=1,nbetu=0,...){
# montant annuel allocation logement    
mall <- 12*alloclogement(revenusalaires1+revenusalaires2,nbpers,...)
# montant annuel rsa
mrsa <- 12*rsa(revenusalaires1+revenusalaires2,nbpers,logement = mall/12)
# montant bourse crous
mcrous <- allocrous(revenusalaires1+revenusalaires2,nbetu)
FF=filldeclarant(revenu=revenusalaires1)
if(nbpers==1){ 
    mimpots=impots(FF,...)$impots
    malf =0
    }
else{
    FF$AJ[2] = revenusalaires2
    mimpots=impots(FF,nbparts = 2 + (nbpers-2)/2,...)$impots[1]
# allocations familliales    
# age 15 ans
    malf <- 12*allocf(revenusalaires1+revenusalaires2,rep(15,nbpers-2))
    }
#cat(revenusalaires1,mimpots,mrsa,mall,"\n")
mimpots-mrsa-mall-malf-mcrous
#niveau de vie 
#revenusalaires1+revenusalaires2+mrsa+mall-mimpots
}
# CMU-C www.cmu.fr
