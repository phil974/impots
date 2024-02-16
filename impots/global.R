load("~/rdata/impots/2021")
load("~/rdata/impots/2023-provisoire")
source('~/rdata/impots/calculimpot.R')
rmax <- 2600
choix=list("un célibataire"=1,"un couple"=2,"une famille (3)"=3,
           "une famille (4)"=4, "une famille (5)"=5)

