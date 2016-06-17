shinyUI(fluidPage(
titlePanel("Quelques éléments sur le système fiscal français"),
mainPanel(
    
    h2("L'impôt sur le revenu (barème sur les revenus de ",di$annee,")"),
    p("Normalement, l'impôt est progressif.
Ce qui veut dire, que plus le revenu est important, plus le taux  d'imposition est important."),
    sliderInput("revenus","revenus par personne (k€)",
                             min = 10,
                             max = rmax/10, value = 50),
        plotOutput("Plot1"),
    p("revenu d'activité imposable avec frais professionnels de 10 % minimun", 
      di$plancherFraisPro," maximum ",di$plafondFraisPro),
    p("Le taux marginal d’impostion (TMI) c’est l’impôt en plus quand on augmente son revenu de 100 €."),
    
    p("C’est aussi la pente du graphique ci-dessus."),
    plotOutput("Plot2")
    )        
)
)
