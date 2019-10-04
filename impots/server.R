tabimpot = matrix(nrow=rmax,ncol=length(choix))
for (i in 1:rmax){
    tabimpot[i,1] = impots(filldeclarant(revenu=i*100))$impots
    FF = filldeclarant(revenu=i*50)
    FF$AJ[2]= 50*i
    tabimpot[i,2] = impots(FF,nbparts=2)$impots
    tabimpot[i,3] = impots(FF,nbparts=2.5)$impots
    tabimpot[i,4] = impots(FF,nbparts=3)$impots
    tabimpot[i,5] = impots(FF,nbparts=4)$impots
}
TMI=diff(tabimpot)
#TMI[,2:3] <- TMI[,2:3]/2

shinyServer(function(input, output) {
    
     
    r <- reactive(10*input$revenus[1]) 
    i <- reactive(as.numeric(input$typeff))
    d <- reactive(10*input$revenus[2])  # début pour les graphes
    output$Plot1 <- renderPlot({
        plot(d():r()/10,tabimpot[d():r(),i()],col="blue",type="l",
        xlab="revenus annuels (k€)",ylab="impôt",
        main=paste0("impôts d'",names(choix[i()])))
    })
    output$Plot2 <- renderPlot({  
        plot(d():(r()-1)/10,TMI[d():(r()-1),i()],col="blue",type="l",xlab="revenus annuels (k€)",
        ylab="TMI (%)",main=paste0("TMI d'",names(choix[i()])),ylim=c(0,50))
    })
})