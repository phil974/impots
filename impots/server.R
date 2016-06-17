shinyServer(function(input, output) {
    
    tabimpot1 = numeric(rmax)
    for (i in 1:rmax)
        tabimpot1[i] = impots(filldeclarant(revenu=i*100))$impots
    TMI1=diff(tabimpot1)

    r <- reactive(10*input$revenus) 
    
    output$Plot1 <- renderPlot({
    plot(1:r()/10,tabimpot1[1:r()],col="blue",type="l",
         xlab="revenus annuels (k€)",ylab="impôt",
         main="impôt d'un célibataire")
    })
    output$Plot2 <- renderPlot({  
    plot(1:(r()-1)/10,TMI1[1:(r()-1)],col="blue",type="l",xlab="revenus annuels (k€)",
         ylab="TMI (%)",main="TMI d'un célibataire",ylim=c(0,50))
    })
})