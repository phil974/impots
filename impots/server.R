tabimpot = matrix(nrow=rmax,ncol=3)
for (i in 1:rmax){
    tabimpot[i,1] = impots(filldeclarant(revenu=i*100))$impots
    FF = filldeclarant(revenu=i*100)
    FF$AJ[2]= 100*i
    tabimpot[i,2] = impots(FF,nbparts=2)$impots
    tabimpot[i,3] = impots(FF,nbparts=3)$impots
}
TMI=diff(tabimpot)
TMI[,2:3] <- TMI[,2:3]/2

shinyServer(function(input, output) {
    
     
    r <- reactive(10*input$revenus) 
    i <- reactive(as.numeric(input$typeff))
    
    output$Plot1 <- renderPlot({
        plot(1:r()/10,tabimpot[1:r(),i()],col="blue",type="l",
        xlab="revenus annuels (k€)",ylab="impôt",
        main=paste0("impôts d'",names(choix[i()])))
    })
    output$Plot2 <- renderPlot({  
        plot(1:(r()-1)/10,TMI[1:(r()-1),i()],col="blue",type="l",xlab="revenus annuels (k€)",
        ylab="TMI (%)",main=paste0("TMI d'",names(choix[i()])),ylim=c(0,50))
    })
})