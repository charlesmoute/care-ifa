library(shiny)
library(shinythemes)
library(shinymaterial)
library(reshape)
library(gtable)
library(gridExtra)
library(plyr)
library(tidyverse)


# library(plotly) # Create interactive Web Graphics via

#library(grid)
#library(gridExtra)
# library(cowplot) #save_plot, plot_grid

# if(file.exists("appData.RData")){
#   load("appData.RData")
#   #rm(list = setdiff(c("dds.graphs","graphs"),ls()))
# }
# packageurl <- "http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_0.9.1.tar.gz"
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rgeos/rgeos_0.3-28.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")
# rm(packageurl)

# devtools::install_version("rgeos", version = "0.3-28", repos = "http://cran.us.r-project.org")
# devtools::install_version("rgeos", version = "0.3-28", repos = "https://cran.r-project.org")

source("helpers.R")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Variables utilitaires
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Base de données relative au dividende économique

countries <- read.csv2("data/country.csv")
countriesList <- as.list(as.character(countries$english))
names(countriesList) <- as.character(countries$french)
countriesList <- c(list("-- Sélectionnez un pays --"="All"),countriesList)

dde.database <- read.csv("data/DDE_Database.csv")
if("X" %in% names(dde.database)) dde.database <- dde.database %>% select(-X)

dds.database <- read.csv("data/DDS_Database.csv")
if("X" %in% names(dds.database)) dds.database <- dds.database %>% select(-X)

isf.database <- read.csv("data/ISF_Database.csv")
if("X" %in% names(isf.database)) isf.database <- isf.database %>% select(-X)

year.max <- as.numeric(format(Sys.time(),"%Y"))

dde.countries <- read.csv2("data/DDE_country.csv")
dde.countriesList <- as.character(dde.countries$english)

dds.countries <- read.csv2("data/DDS_country.csv")
dds.countriesList <- as.character(dds.countries$english)
# dde.countriesList <- as.list(as.character(dde.countries$english))
# names(dde.countriesList) <- as.character(dde.countries$french)
# dde.countriesList <- c(list("--- Tous les pays ---"="All"),dde.countriesList)

#Fichier de donnees relatif a la fenetre d'opportunite
txdpce <- read.csv2("data/txdpce.csv")
names(txdpce)[3:dim(txdpce)[2]] <- as.character(seq(from=1950,to=2100,by=5))
txdpce <- txdpce[which(txdpce$country %in% countries$english ),]

#Base de données relative à la chaîne de production du dividende économique
dde.freins <- read.csv2('data/DDE_obstacle.csv')

#Base de données relative à la chaîne de production du dividende scolaire
dds.freins <- read.csv2('data/DDS_obstacle.csv')

#Base de données relative à la source du changement du PIB/tête
dde.decomp <- read.csv2('data/DDE_decomposition.csv')

#Base de données relative à la source du changement du Taux brut de scolarisation au 
#niveau secondaire
dds.decomp <- read.csv2('data/DDS_decomposition.csv')

# Base de données relative aux indicateurs utilisés dans l'ouvrage du dividende économique
dde.indice <- read.csv2("data/DDE_indicateur.csv")

# Base de données relative aux indicateurs utilisés dans l'ouvrage le dividende scolaire
dds.indice <- read.csv2("data/DDS_indicateur.csv")

# Paramètres globale pour images
fig.width <- 400
fig.width02 <- 590
fig.height <- 400

dde.windowGraph <- dde.obstacleGraph <- dde.srceGraph <- NULL
dds.windowGraph <- dds.obstacleGraph <- dds.srceGraph <- NULL

# Chargement des graphiques 
# dde.graphs <- draw.worldMap(dde.freins)
# dds.graphs <- draw.worldMap(dds.freins,draw.dds=TRUE)

## Liste des variables
dde.varlist <- c("year","isf","popTotal","pop1564_","pibHbt","txchomage")
dds.varlist <- c("year","isf","gni","popTot","k","pop1564","youth.dpce","txdpce_age",
                 "txdpce_real","tbs")

# varlist <- c("english","french","code","year","isf","gni","popTot","k","pop1564","dpceYouth","dpceAge",
#              "dpceEco","tbs")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Interface utilisateurs
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

ui <- tagList(
  tags$head(
    HTML('
          <!-- Global site tag (gtag.js) - Google Analytics -->
          <script async src="https://www.googletagmanager.com/gtag/js?id=UA-149968367-1"></script>
          <script>
            window.dataLayer = window.dataLayer || [];
            function gtag(){dataLayer.push(arguments);}
            gtag("js", new Date());
          
            gtag("config", "UA-149968367-1");
          </script>
    '),
    # tags$script("async src"="https://www.googletagmanager.com/gtag/js?id=UA-149968367-1"),
    # includeScript("google-analytics.js"),
    tags$script("src"="func.js"),
    tags$style(
      type="text/css",
      "a{font-weight:bold;}"
    )
  ),
  navbarPage(
    title = "DD-cal",#tags$a("FraNet", onclick="customHref('acceuil')"),
    footer=tagList(hr(),p(align="center",style="font-size:11px;","Pour des ",strong("retours")," et/ou des ",strong("suggestions d'améliorations")," bien vouloir ",code("Nous conctacter"),
                          "aux adressses suivantes ",
                          strong(a(href="mailto:reseau.franet@gmail.com?subject=[FraNet] Feedback Application&cc=charlesmoute@gmail.com",
                            "reseau.franet@gmail.com / charlesmoute@gmail.com"))),
                   br()),
    # theme = "cerulean",united,flatly,sandstone
    tabPanel("Accueil",value="acceuil",icon = icon("home","fa-2x"),
             img(src="IUSSP_logo.png",width="8%"),br(),br(), # Insertion du logo de l'UIESP
             p(style="text-align:justify","L’application ici proposée se veut avant tout être une ",code("« calculatrice »")," pour la mesure du ",tags$a("dividende économique", onclick="customHref('economicDividend')"),
              "et du ",tags$a("dividende scolaire", onclick="customHref('schoolDividend')"),
              "en Afrique en utilisant une ",a(href="https://iussp.org/sites/default/files/Decomposition_2018.pdf","méthode de décomposition"),
              ". Cette approche a été développée par le réseau pour le renforcement de la formation démographique en Afrique francophone (FraNet)."),
               # a(href="https://www.iussp.org/fr/formation/renforcement-de-la-formation-demographique-en-afrique-francophone","(FraNet)"),"."),
             br(),
             p(style="text-align:justify",strong(em("Le réseau")),code("FraNet"),"a été créé en 2009 sous l’égide de l’UIESP et avec le soutien financier de la fondation Hewlett.",
              "Il se  propose d’améliorer la recherche démographique et la communication avec les décideurs politiques dans la sous-région."),
              p("[",a(href="https://www.iussp.org/fr/formation/renforcement-de-la-formation-demographique-en-afrique-francophone","Pour en savoir plus sur le réseau ..."),"]"),
             br(),
             p(align="justify","Les graphiques et tableaux présentés par défaut par l'application sont ceux disponibles dans", 
               a(href="https://iussp.org/sites/default/files/DDE_2018.pdf","l’ouvrage sur le dividende économique")," et l’ouvrage sur le dividende scolaire.",
               "L’utilisateur peut saisir ses propres données pour produire les tableaux et graphiques correspondants.",
               "Pour ce faire, après sélection du ",code("type de dividende")," et du ",code("pays")," allez à l’option ",code("paramètres avancés"),
               ". Vous pouvez également configurer automatiquement l'application avec vos données. Pour cela : "),
             p(align="justify",
               tags$ol(tags$li("D'abord, ",code("téléchargez")," un des templates ci-dessous."),
                       tags$li("Puis, ",code("complétez"), " les feuilles ",em("'ISF'")," et ",em("'Data'")," avec vos données. La feuille 'Readme' décrit les paramétres de chacune des feuilles 'ISF' et 'Data'."),
                       tags$li("Ensuite, après sélection du ",code("type de dividende")," et du ",code("pays")," sélectionnez ",code("Paramètres avancés"),"."),
                       tags$li("Enfin,",code("chargez"), " la feuille remplie à ",span(style="text-decoration:underline;",em("l'étape 2")),". Pour ce faire, sélectionnez ",code("Parcourir..."),
                               " puis sélectionner le fichier à charger."))),
             br(),
             p(align="justify",strong("Templates de configuration automatique"),
               tags$ul(tags$li(a(href="dde_template.xls","Template pour le dividende économique")),
                       tags$li(a(href="dds_template.xls","Template pour le dividende scolaire")))),
             br(),
             p(align="justify","Lorsque vous modifiez les périodes d'observation des indicateurs, les données, ",code("lorsqu'elles sont disponibles"),", sont automatiquement chargées dans les champs.",
               "Aussi, le cas échéant, vous serez invité à compléter les champs vides. Pour la complétion automatique des champs, il y'a lieu de préciser que : ",
               tags$ul(tags$li("les données relatives au nombre moyen d'enfants par femme (ISF) ainsi qu'aux paramètres mobilisés pour le dividende scolaire sont issues du ",
                               code("World Development Indicator 2018")),
                       tags$li("les données relatives au dividende économique sont issues du ",code("Penn World Table 9.0")),
                       tags$li("les données mobilisées pour l'estimation de la fenêtre d'opportunité sont issues du ",
                               code("World Population Prospects 2017"))))
             # ,br(),
             # p(style="text-align:justify",strong(em("Le Réseau pour le renforcement de la formation démographique en Afrique francophone")),", en abrégé ",
             #   code("FraNet"),"a été créé en 2009 sous l’égide de l’UIESP et avec le soutien financier de la fondation Hewlett.",
             #   "Il se  propose d’améliorer la recherche démographique et la communication avec les décideurs politiques dans la sous-région.",
             #   a(href="https://www.iussp.org/fr/formation/renforcement-de-la-formation-demographique-en-afrique-francophone","Pour en savoir plus ..."))
             # ,hr(),
             # h5(strong("Comité de pilotage du réseau")),
             # h5(em("Président :")),
             # p(tags$ul(tags$li("Parfait M. Eloundou Enyegue (Cornell University)"))),
             # h5(em("Membres :")),
             # p(tags$ul(tags$li("Jean François Kobiane (Université de Ouagadougou)"),
             #           tags$li("Gervais Beninguisse (Institut de formation et de Recherche Démographiques (IFORD))"),
             #           tags$li("Valérie Delaunay (Institut de Recherche pour le Développement (IRD))"),
             #           tags$li("Richard Marcoux (Université de Laval)"),
             #           tags$li("Philippe Bocquier (Université Catholique de Louvain)"))),
             # h5(em("Conseillers :")),
             # p(tags$ul(tags$li("Cheikh MBACKE (Consultant Indépendant, Sénégal)"),
             #           tags$li("William MOLMY (INED, France)"))),
             # h5(em("Appui administratif et logistique :")),
             # p(tags$ul(tags$li("Mary Ellen ZUPPAN (Directrice Exécutive, UIESP)"),
             #           tags$li("Paul Monet (International Union for the Scientific Study of Population (IUSSP))")))
    ),
    tabPanel("Dividende économique",value="economicDividend",icon = icon("bar-chart-o","fa-2x"),
             fluidPage(theme = shinytheme("sandstone"),fluidRow(
               column(width=4,wellPanel(
                 h4("Paramètres"),
                 selectInput("countriesList",NULL,choices = countriesList),
                 conditionalPanel("input.countriesList!='All'",
                                  hr(),
                                  sliderInput("rangeWindow","Période de la fenêtre d'opportunité",sep="",min=1950,max=2100,value = c(1975,2035),step=5),
                                  sliderInput("tresholdWindow","Seuil de la fenêtre d'opportunité",value=80,min=55,max=95,step=0.1),
                                  hr(),
                                  radioButtons("dataSrc","Paramètres avancés",c("Non","Oui"),inline = TRUE),
                                  conditionalPanel("input.dataSrc=='Oui'",
                                                   hr(),
                                                   fileInput('fileDDE', '1. Sélectionnez le fichier de données à charger',
                                                             accept = c("application/vnd.ms-excel",
                                                                        "pplication/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                                                             buttonLabel = "Parcourir..."),
                                                   div(align="center",h3(code("OU"))),p(strong("2. Saisir/Modifier les données")),
                                                   sliderInput("isf.year","Indice synthétique de fécondité (ISF) : période d'observation du changement",value = c(1975,1990),min=1950,max=year.max,step=1,sep=""),
                                                   numericInput("isf01",label="Valeur de l'ISF en 1975",value = NA,min=0,max=20,step =0.01),
                                                   numericInput("isf02",label="Valeur de l'ISF en 1990",value = NA,min=0,max=20,step =0.01),
                                                   hr(),
                                                   sliderInput("indice.year","Autres indicateurs: période d'observation du changement",value = c(1990,2010),min = 1950,max=year.max,step=1,sep=""),
                                                   helpText("Note: la période d'observation du changement des indicateurs doit être postérieure à celle de l'ISF"),
                                                   hr(),
                                                   numericInput("popTotal01",label="Population total en 1990",value = NA,min=0,step =1),
                                                   numericInput("popTotal02",label="Population total en 2010",value = NA,min=0,step =1),
                                                   hr(),
                                                   numericInput("pop1564_01",label="Population 15 à 64 ans (%) en 1990",value = NA,min=0,max=100,step=0.01),
                                                   numericInput("pop1564_02",label="Population 15 à 64 ans (%) en 2010",value = NA,min=0,max=100,step=0.01),
                                                   hr(),
                                                   numericInput("pibHbt01",label="PIB/habitant en 1990",value = NA,min=0,step=0.01),
                                                   numericInput("pibHbt02",label = "PIB/habitant en 2010",value=NA,min=0,step=0.01),
                                                   hr(),
                                                   numericInput("txchomage01",label="Taux de chômage (%) en 1990",value = NA,min=0,step=0.01),
                                                   numericInput("txchomage02",label="Taux de chômage (%) en 2010",value = NA,min=0,step=0.01),
                                                   div(align="right",actionButton("resetButton","Reset",icon=icon("window-restore","regular"))),
                                                   selectize=FALSE)
                                  )
                    )),
               column(width=8,
                      conditionalPanel("input.countriesList=='All'",
                                       br(),
                                       p(style="text-align:justify",code("Le dividende économique"),
                                         " est la contribution de la dynamique de la population sur la dynamique de la richesse nationale par habitant (PIB / par habitant).",
                                         "La baisse de la fécondité dans un pays modifie la structure par âge de la population et notamment la part de la population en âge de travailler.  Cette modification a des implications ",code("séquentielles"),
                                         tags$a("(voire méthodologie)", onclick="customHref('method');customHref('methodEconomicDividend');"),
                                         " sur le PIB par habitant et l’objet de cette page est de les quantifier. ",
                                         "Au départ, la baisse de la fécondité, avec un décalage dans le temps, modifie le nombre de personnes en âge de travailler et donc le ratio de dépendance démographique. ",
                                         "En fonction du taux d’activité et de la productivité des personnes qui travaillent, le ratio de dépendance démographique se convertit en une dépendance économique. ",
                                         "La capacité des personnes qui travaillent à supporter la charge de la population affecte la richesse par habitant. "),
                                       p(style="text-align:justify",
                                         "Pour un pays donné, les graphiques montrent successivement, la fenêtre d’opportunité du dividende démographique, les corrélations successives entre la baisse de la fécondité et le changement observé du PIB/habitant et enfin les contributions de trois sources du changement de la richesse nationale par habitant sur la période. ",
                                         strong("L’utilisateur peut saisir ses propres données")," pour produire les tableaux et graphiques correspondants. ",
                                         "Pour ce faire, ",code("sélectionner le pays")," puis ",code("paramètres avancés"),"."),
                                       br(),
                                       h4("Chaîne de production du dividende économique en Afrique entre 1990 et 2010"),
                                       hr(),
                                       fluidRow(
                                         column(width=6,
                                                h5("Transition : Baisse de la fécondité >> Structure par âge"),
                                                helpText("Etape 1 de la chaîne de production du dividende économique"),
                                                #plotOutput("plotApps_01"),
                                                div(style="position:relative",
                                                    div(img(src="Loader.gif",height=100,width=100),
                                                        style="position:absolute;z-index:1;top:25%;left:25%"),
                                                    div(imageOutput("plotApps_01"),
                                                        style="position:relative;top:0%;z-index:2")),
                                                uiOutput("plotApps_btt01",align="right")),
                                         column(width=6,
                                                h5("Transition : Structure par âge >> Emploi"),
                                                helpText("Etape 2 de la chaîne de production du dividende économique"),
                                                # plotOutput("plotApps_02").
                                                div(style="position:relative",
                                                    div(img(src="Loader.gif",height=100,width=100),
                                                        style="position:absolute;z-index:1;top:25%;left:25%"),
                                                    div(imageOutput("plotApps_02"),
                                                        style="position:relative;top:0%;z-index:2")),
                                                uiOutput("plotApps_btt02",align="right"))
                                       ),
                                       fluidRow(
                                         column(width=6,
                                                h5("Transition : Emploi >> Productivité des occupés"),
                                                helpText("Etape 3 de la chaîne de production du dividende économique"),
                                                # plotOutput("plotApps_03"),
                                                div(style="position:relative",
                                                    div(img(src="Loader.gif",height=100,width=100),
                                                        style="position:absolute;z-index:1;top:25%;left:25%"),
                                                    div(imageOutput("plotApps_03"),
                                                        style="position:relative;top:0%;z-index:2")),
                                                uiOutput("plotApps_btt03",align="right"))
                                         ,
                                         column(width=6,
                                                h5("Transition : Productivité des occupés >> PIB/Tête"),
                                                helpText("Etape 4 de la chaîne de production du dividende économique"),
                                                # plotOutput("plotApps_04"),
                                                div(style="position:relative",
                                                    div(img(src="Loader.gif",height=100,width=100),
                                                        style="position:absolute;z-index:1;top:25%;left:25%"),
                                                    div(imageOutput("plotApps_04"),
                                                        style="position:relative;top:0%;z-index:2")),
                                                uiOutput("plotApps_btt04",align="right"))
                                       )),
                      conditionalPanel("input.countriesList!='All'",
                                       br(),
                                       h5(strong("Fenêtre d'opportunité")),
                                       # helpText("A propos de la fenêtre d'opportunité ..."),
                                       hr(),
                                       # plotOutput("dde_windowCountry"),
                                       div(style="position:relative",align="center",
                                           div(img(src="Loader.gif",height=100,width=100),
                                               style="position:absolute;z-index:1;top:25%;left:45%"),
                                           div(imageOutput("dde_windowCountry"),
                                               style="position:relative;top:0%;z-index:2")),
                                       uiOutput("dde_windowCountry_btt",align="right"),
                                       br(),
                                       h5(strong("Chaîne de production du dividende économique")),
                                       # helpText("A propos de la chaîne de production..."),
                                       hr(),
                                       # plotOutput("dde_obstacle"),
                                       div(style="position:relative",align="center",
                                           div(img(src="Loader.gif",height=100,width=100),
                                               style="position:absolute;z-index:1;top:25%;left:45%"),
                                           div(imageOutput("dde_obstacle"),
                                               style="position:relative;top:0%;z-index:2")),
                                       uiOutput("dde_obstacle_btt",align="right"),
                                       br(), 
                                       h5(strong(textOutput("dde.titleSource", container = span))) ,
                                       # helpText("A propos des sources de changement..."),
                                       hr(),
                                       # plotOutput("dde_srce")
                                       div(style="position:relative",align="center",
                                           div(img(src="Loader.gif",height=100,width=100),
                                               style="position:absolute;z-index:1;top:25%;left:45%"),
                                           div(imageOutput("dde_srce"),
                                               style="position:relative;top:0%;z-index:2")),
                                       uiOutput("dde_srce_btt",align="right")
                      )      
               )
    ))),
    tabPanel("Dividende Scolaire",value="schoolDividend",icon = icon("graduation-cap","fa-2x"),
             fluidPage(fluidRow(
               column(width=4,wellPanel(
                 h4("Paramètres"),
                 selectInput("ddsCountries",NULL,choices = countriesList),
                 conditionalPanel("input.ddsCountries!='All'",
                                  hr(),
                                  sliderInput("ddsRangeWindow","Période de la fenêtre d'opportunité",sep="",min=1950,max=2100,value = c(1980,2035),step=5),
                                  sliderInput("ddsTresholdWindow","Seuil de la fenêtre d'opportunité",value=80,min=55,max=95,step=0.1),
                                  hr(),
                                  radioButtons("ddsDataSource","Paramètres avancés",c("Non","Oui"),inline = TRUE),
                                  conditionalPanel("input.ddsDataSource=='Oui'",
                                                   fileInput('fileDDS', 'Sélectionnez le fichier de données à charger',
                                                             accept = c("application/vnd.ms-excel",
                                                                        "pplication/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
                                                             buttonLabel = "Parcourir..."),
                                                   div(align="center",h3(code("OU"))),p(strong("2. Saisir/Modifier les données")),
                                                   sliderInput("dds.isfYear","Indice synthétique de fécondité (ISF) : période d'observation du changement",value = c(1980,1995),min=1950,max=year.max,step=1,sep=""),
                                                   numericInput("dds.isf01",label="Valeur de l'ISF en 1980",value = NA,min=0,max=20,step =0.01),
                                                   numericInput("dds.isf02",label="Valeur de l'ISF en 1995",value = NA,min=0,max=20,step =0.01),
                                                   hr(),
                                                   sliderInput("dds.indiceYear","Autres indicateurs: période d'observation du changement",value = c(1995,2010),min = 1950,max=year.max,step=1,sep=""),
                                                   helpText("Note: la période d'observation du changement des indicateurs doit être postérieure à celle de l'ISF"),
                                                   hr(),
                                                   numericInput("ddsPopTotal01",label="Population total en 1995",value = NA,min=0,step =1),
                                                   numericInput("ddsPopTotal02",label="Population total en 2010",value = NA,min=0,step =1),
                                                   hr(),
                                                   numericInput("ddsPop1564_01",label="Population 15 à 64 ans (%) en 1995",value = NA,min=0,max=100,step=0.01),
                                                   numericInput("ddsPop1564_02",label="Population 15 à 64 ans (%) en 2010",value = NA,min=0,max=100,step=0.01),
                                                   hr(),
                                                   numericInput("dpceAge01",label="Taux de dépendance démographique total en 1995 (%)",value = NA,min=0,step=0.01),
                                                   numericInput("dpceAge02",label = "Taux de dépendance démographique total en 2010 (%)",value=NA,min=0,step=0.01),
                                                   hr(),
                                                   numericInput("dpceEco01",label="Taux de dépendance économique en 1995 (%)",value = NA,min=0,step=0.01),
                                                   numericInput("dpceEco02",label = "Taux de dépendance économique en 2010 (%)",value=NA,min=0,step=0.01),
                                                   hr(),
                                                   numericInput("dpceYouth01",label="Taux de dépendance des jeunes en 1995 (%)",value = NA,min=0,step=0.01),
                                                   numericInput("dpceYouth02",label = "Taux de dépendance des jeunes en 2010 (%)",value=NA,min=0,step=0.01),
                                                   hr(),
                                                   numericInput("gni01",label="Produit national brut en 1995",value = NA,min=0,step=0.01),
                                                   numericInput("gni02",label = "Produit national brut en 2010",value=NA,min=0,step=0.01),
                                                   hr(),
                                                   numericInput("k01",label="Part du budget alloué à l'éducation en 1995 (%)",value = NA,min=0,step=0.01),
                                                   numericInput("k02",label = "Part du budget alloué à l'éducation en 2010 (%)",value=NA,min=0,step=0.01),
                                                   hr(),
                                                   numericInput("tbs01",label="Taux brut de scolarisation en 1995 (%)",value = NA,min=0,step=0.01),
                                                   numericInput("tbs02",label = "Taux brut de scolarisation en 2010 (%)",value=NA,min=0,step=0.01),
                                                   div(align="right",actionButton("ddsResetButton","Reset",icon=icon("window-restore","regular"))),
                                                   selectize=FALSE)
                 )
               )),
               column(width=8,
                      conditionalPanel("input.ddsCountries=='All'",
                                       br(),
                                       p(style="text-align:justify",code("Le dividende scolaire")," est la contribution de la dynamique de la population à la dynamique des dotations scolaire.",
                                         "La baisse de la fécondité dans un pays modifie la structure par âge de la population et notamment la part de la population en âge d’être scolarisée.",
                                         "Lorsque la part de la population scolarisable se modifie, cela a des implications sur les dotations scolaires par enfant et l’objet de cette section est de monter une quantification de ces implications qui sont ",
                                         code("séquentielles"), 
                                         tags$a("(voire méthodologie)", onclick="customHref('method');customHref('methodSchoolDividend');"),
                                         "On distingue successivement l’influence de la baisse de la fécondité sur la modification de la structure par âge de la population, sur la dépendance économique, sur les ressources allouées à chaque élève."),
                                       p(style="text-align:justify","Pour un pays donné, les graphiques montrent successivement, la fenêtre d’opportunité du dividende démographique, les corrélations successives entre la baisse de la fécondité et le changement observé dans la dotation par enfant et enfin les contributions de trois sources du changement de la dotation par enfant sur la période.",
                                         strong("L’utilisateur peut saisir ses propres données")," pour produire les tableaux et graphiques correspondants.",
                                         "Pour ce faire, ",code("sélectionner le pays")," puis ",code("paramètres avancés"),"."),
                                       br(),
                                       h4("Chaîne de production du dividende scolaire en Afrique entre 1995 et 2010"),
                                       hr(),
                                       fluidRow(
                                         column(width=6,
                                                h5("Transition : Baisse de la fécondité >> Structure par âge"),
                                                helpText("Etape 1 de la chaîne de production du dividende scolaire"),
                                                div(style="position:relative",
                                                    div(img(src="Loader.gif",height=100,width=100),
                                                        style="position:absolute;z-index:1;top:25%;left:25%"),
                                                    div(imageOutput("ddsPlotApps_01"),
                                                        style="position:relative;top:0%;z-index:2")),
                                                uiOutput("ddsPlotApps_btt01",align="right")),
                                         column(width=6,
                                                h5("Transition : Structure par âge >> Dépendance économique"),
                                                helpText("Etape 2 de la chaîne de production du dividende scolaire"),
                                                # plotOutput("ddsPlotApps_02"),
                                                div(style="position:relative",
                                                    div(img(src="Loader.gif",height=100,width=100),
                                                        style="position:absolute;z-index:1;top:25%;left:25%"),
                                                    div(imageOutput("ddsPlotApps_02"),
                                                        style="position:relative;top:0%;z-index:2")),
                                                uiOutput("ddsPlotApps_btt02",align="right"))
                                       ),
                                       fluidRow(
                                         column(width=6,
                                                h5("Transition : Dépendance économique >> Ressources par élève"),
                                                helpText("Etape 3 de la chaîne de production du dividende scolaire"),
                                                # plotOutput("ddsPlotApps_03"),
                                                div(style="position:relative",
                                                    div(img(src="Loader.gif",height=100,width=100),
                                                        style="position:absolute;z-index:1;top:25%;left:25%"),
                                                    div(imageOutput("ddsPlotApps_03"),
                                                        style="position:relative;top:0%;z-index:2")),
                                                uiOutput("ddsPlotApps_btt03",align="right"))
                                         ,
                                         column(width=6,
                                                h5("Transition : Ressources par élève >> Education"),
                                                helpText("Etape 4 de la chaîne de production du dividende scolaire"),
                                                # plotOutput("ddsPlotApps_04"),
                                                div(style="position:relative",
                                                    div(img(src="Loader.gif",height=100,width=100),
                                                        style="position:absolute;z-index:1;top:25%;left:25%"),
                                                    div(imageOutput("ddsPlotApps_04"),
                                                        style="position:relative;top:0%;z-index:2")),
                                                uiOutput("ddsPlotApps_btt04",align="right"))
                                       )),
                      conditionalPanel("input.ddsCountries!='All'",
                                       br(),
                                       h5(strong("Fenêtre d'opportunité")),
                                       hr(),
                                       # plotOutput("dds_windowCountry"),
                                       div(style="position:relative",align="center",
                                           div(img(src="Loader.gif",height=100,width=100),
                                               style="position:absolute;z-index:1;top:25%;left:45%"),
                                           div(imageOutput("dds_windowCountry"),
                                               style="position:relative;top:0%;z-index:2")),
                                       uiOutput("dds_windowCountry_btt",align="right"),
                                       br(),
                                       h5(strong("Chaîne de production du dividende scolaire")),
                                       hr(),
                                       # plotOutput("dds_obstacle"),
                                       div(style="position:relative",align="center",
                                           div(img(src="Loader.gif",height=100,width=100),
                                               style="position:absolute;z-index:1;top:25%;left:45%"),
                                           div(imageOutput("dds_obstacle"),
                                               style="position:relative;top:0%;z-index:2")),
                                       uiOutput("dds_obstacle_btt",align="right"),
                                       br(), 
                                       h5(strong(textOutput("dds.titleSource", container = span))) ,
                                       hr(),
                                       # plotOutput("dds_srce")
                                       div(style="position:relative",align="center",
                                           div(img(src="Loader.gif",height=100,width=100),
                                               style="position:absolute;z-index:1;top:25%;left:45%"),
                                           div(imageOutput("dds_srce"),
                                               style="position:relative;top:0%;z-index:2")),
                                       uiOutput("dds_srce_btt",align="right")
                      )      
               )
             ))),
    tabPanel("Aspects méthodologiques",icon = icon("briefcase","fa-2x"), value="method",tabsetPanel( #
      # tabPanel("FraNet",conditionalPanel("input.about=='FraNet'",includeMarkdown("About_FraNet.md"))),
      tabPanel("Dividende économique",value="methodEconomicDividend",
               includeMarkdown("About_DDE.md"),br()
               # conditionalPanel("input.about=='Dividende économique'",includeMarkdown("About_DDE.md"))
               ),
      tabPanel("Dividende scolaire",value="methodSchoolDividend",
               includeMarkdown("About_DDS.md"), br()
               # conditionalPanel("input.about=='Dividende scolaire'",includeMarkdown("About_DDS.md"))
               ),
      id="about"
    ))
  ),
  tags$style(type="text/css",
             '.navbar-default .navbar-brand {color: #93c54b;}',
             '.navbar-default:hover .navbar-brand {color: #93c54b;}',
             '.myButton{color: #3e3f3a;background-color:transparent;}',
             '.btn.myButton, btn-default.myButton{all:unset}',
             '.myButton:hover{color: #93c54b;background-color:transparent;}'
             )
             # '.shiny-output-error { visibility: hidden; }',
             # '.shiny-output-error:before { visibility: hidden; }')
)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Serveur pour le traitement des données
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

server <- function(input,output,session){
    
output$dde.titleSource <- renderText({"Source du changement du PIB/Tête entre 1990 et 2010"})
  
  output$plotApps_01 <- renderImage({
      validate(
        need(input$countriesList=='All',message = FALSE)
      )
      if(input$countriesList=='All'){
        # dde.graphs <- NULL
        # dde.graphs <- get("dde.graphs",envir = .GlobalEnv)
        # dde.graphs[[1]]
        
        outfile <- tempfile(fileext = ".png")
        # outfile <- "./www/plotApps_01.png"
        pixelratio <- session$clientData$pixelratio
        
        width <- session$clientData$output_plotApps_01_width
        height <- session$clientData$output_plotApps_01_height 
        png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
        plot(dde.graphs[[1]])
        dev.off()
        
        list(src = outfile,
             contentType = 'image/png',
             width = ifelse(width>get("fig.width",envir = .GlobalEnv),
                            get("fig.width",envir = .GlobalEnv),width),
             height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                            get("fig.height",envir = .GlobalEnv),height),
             alt = "Loading ...")
        
        # plot(dde.graphs[[1]])
        
        # withProgress(message = 'Creating map', value = 0.1,{
        #   dde.graphs <- draw.worldMap(dde.freins)
        #   incProgress(0.5,detail="Generating data")
        #   # grid.arrange(graphs[[1]],dde.graphs[[2]],ncol=2)
        #   Sys.sleep(0.1)
        #   # graphs <- get("dde.graphs",envir = .GlobalEnv)
        #   plot(graphs[[1]])
        #   assign("dde.graphs",dde.graphs,envir = .GlobalEnv)
        #   incProgress(0.4,detail="Plotting data")
        # })
      }else{ return(NULL)} 
    },deleteFile = TRUE)
  output$plotApps_btt01 <- renderUI({
    downloadButton("ddeFullImage01", label = "Download", class = "myButton")
  })
  output$ddeFullImage01 <- downloadHandler(
    filename = "DDE_Afrique_01.png",
    content = function(file){
      ggsave(file, plot = dde.graphs[[1]], device = png())
    }
  )
  
  output$plotApps_02 <- renderImage({
    
    validate(
      need(input$countriesList=='All',message = FALSE)
    )
    
    if(input$countriesList=='All'){
      # if(exists("dde.graphs",envir = .GlobalEnv)){
      #   dde.graphs <- get("dde.graphs",envir = .GlobalEnv)
      #   # grid.arrange(dde.graphs[[3]],dde.graphs[[4]],ncol=2)
      #   # dde.graphs[[2]]
      #   # rm("dde.graphs",envir = .GlobalEnv)
        outfile <- tempfile(fileext = ".png")
        pixelratio <- session$clientData$pixelratio
        
        width <- session$clientData$output_plotApps_02_width
        height <- session$clientData$output_plotApps_02_height 
        png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
        plot(dde.graphs[[2]])
        dev.off()
        
        list(src = outfile,
             contentType = 'image/png',
             width = ifelse(width>get("fig.width",envir = .GlobalEnv),
                            get("fig.width",envir = .GlobalEnv),width),
             height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                            get("fig.height",envir = .GlobalEnv),height),
             alt = "Loading ...")
      # }
    } 
  },deleteFile = TRUE)
  output$plotApps_btt02 <- renderUI({
    downloadButton("ddeFullImage02", label = "Download", class = "myButton")
  })
  output$ddeFullImage02 <- downloadHandler(
    filename = "DDE_Afrique_02.png",
    content = function(file){
      ggsave(file, plot = dde.graphs[[2]], device = png())
    }
  )
  
  output$plotApps_03 <- renderImage({
    
    validate(
      need(input$countriesList=='All',message = FALSE)
    )
    
    if(input$countriesList=='All'){
      # if(exists("dde.graphs",envir = .GlobalEnv)){
      #   dde.graphs <- get("dde.graphs",envir = .GlobalEnv)
      #   # dde.graphs[[3]]
      #   # rm("dde.graphs",envir = .GlobalEnv)
        outfile <- tempfile(fileext = ".png")
        pixelratio <- session$clientData$pixelratio
        
        width <- session$clientData$output_plotApps_03_width
        height <- session$clientData$output_plotApps_03_height 
        png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
        plot(dde.graphs[[3]])
        dev.off()
        
        list(src = outfile,
             contentType = 'image/png',
             width = ifelse(width>get("fig.width",envir = .GlobalEnv),
                            get("fig.width",envir = .GlobalEnv),width),
             height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                            get("fig.height",envir = .GlobalEnv),height),
             alt = "Loading ...")
      # }
    } 
  },deleteFile = TRUE)
  output$plotApps_btt03 <- renderUI({
    downloadButton("ddeFullImage03", label = "Download", class = "myButton")
  })
  output$ddeFullImage03 <- downloadHandler(
    filename = "DDE_Afrique_03.png",
    content = function(file){
      ggsave(file, plot = dde.graphs[[3]], device = png())
    }
  )
  
  output$plotApps_04 <- renderImage({
    
    validate(
      need(input$countriesList=='All',message = FALSE)
    )
    
    if(input$countriesList=='All'){
      # if(exists("dde.graphs",envir = .GlobalEnv)){
      #   dde.graphs <- get("dde.graphs",envir = .GlobalEnv)
      #   # dde.graphs[[4]]
      #   # rm("dde.graphs",envir = .GlobalEnv)
        
        outfile <- tempfile(fileext = ".png")
        pixelratio <- session$clientData$pixelratio
        
        width <- session$clientData$output_plotApps_04_width
        height <- session$clientData$output_plotApps_04_height 
        png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
        plot(dde.graphs[[4]])
        dev.off()
        
        list(src = outfile,
             contentType = 'image/png',
             width = ifelse(width>get("fig.width",envir = .GlobalEnv),
                            get("fig.width",envir = .GlobalEnv),width),
             height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                            get("fig.height",envir = .GlobalEnv),height),
             alt = "Loading ...")
      # }
    } 
  },deleteFile = TRUE)
  output$plotApps_btt04 <- renderUI({
    downloadButton("ddeFullImage04", label = "Download", class = "myButton")
  })
  output$ddeFullImage04 <- downloadHandler(
    filename = "DDE_Afrique_04.png",
    content = function(file){
      ggsave(file, plot = dde.graphs[[4]], device = png())
    }
  )
  
  # Graphique sur la fenêtre d'opportunité
  output$dde_windowCountry <- renderImage({
    
    assign("dde.windowGraph",NULL,envir = .GlobalEnv)
    
    validate(
      need(input$countriesList!='All',message = FALSE)
    )
    
    if(input$countriesList!='All'){
      db <- txdpce[which(txdpce$country==input$countriesList),]
      
      validate(
        need(!is.null(db),message=FALSE)
      )
      dde.g1 <- suppressWarnings(draw.windowCountry(db,graph.title=NULL,
                                                x.start=input$rangeWindow[1],x.end=input$rangeWindow[2],
                                         seuil=input$tresholdWindow))
      # plot(dde.g1)
      # Sauvegarde temporaire du graphique
      assign("dde.windowGraph",dde.g1,envir = .GlobalEnv)
      
      outfile <- tempfile(fileext = ".png")
      pixelratio <- session$clientData$pixelratio
      
      width <- session$clientData$output_dde_windowCountry_width
      height <- session$clientData$output_dde_windowCountry_height 
      png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
      plot(dde.g1)
      dev.off()
      
      list(src = outfile,
           contentType = 'image/png',
           width = ifelse(width>get("fig.width02",envir = .GlobalEnv),
                          get("fig.width02",envir = .GlobalEnv),width),
           height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                          get("fig.height",envir = .GlobalEnv),height),
           alt = "Loading ...")
      
    }
    
  },deleteFile = TRUE)
  output$dde_windowCountry_btt <- renderUI({
    validate(need(!is.null( get("dde.windowGraph",envir = .GlobalEnv)),message = "dde.windoGraph not null"))
    downloadButton("dde_Window", label = "Download", class = "myButton")
  })
  output$dde_Window <- downloadHandler(
    filename = "DDE_Window.png",
    content = function(file){
      plotValue <- get("dde.windowGraph",envir = .GlobalEnv)
      ggsave(file, plot = plotValue, device = png())
    }
  )
  
  # Graphique sur la chaîne de production du DDE
  output$dde_obstacle <- renderImage({
    
    assign("dde.obstacleGraph",NULL,envir = .GlobalEnv) 
    validate(
      need(input$countriesList!='All',message = FALSE)
    )
    
    if(input$countriesList!='All'){
      
      dde.g2 <- NULL
      
      if(input$dataSrc=='Non' & input$countriesList %in% dde.countriesList){
        db <- dde.freins[which(dde.freins$pays==input$countriesList),]
        # db <- dde.freins[which(dde.freins$pays=="Algeria"),] #Cameroon
        dde.g2 <- draw.escalator(db,graph.title=NULL)
        # plot(dde.g2) 
        
      }else{
        msgvalue <- dde.checkDataUser(input$isf.year[1],input$isf.year[2],
                                      input$isf01,input$isf02,
                                      input$indice.year[1],input$indice.year[2],
                                      input$popTotal01,input$popTotal02,
                                      input$pop1564_01,input$pop1564_02,
                                      input$txchomage01,input$txchomage02,
                                      input$pibHbt01,input$pibHbt02)
        # cat("\ndde_obstacle::msgvalue = ",is.null(msgvalue),"; valeur = ",msgvalue,"\n")
        if(is.null(msgvalue)){
          result <- compute.dde.indice(input$isf.year[1],input$isf.year[2],
                                       input$isf01,input$isf02,
                                       input$indice.year[1],input$indice.year[2],
                                       input$popTotal01,input$popTotal02,
                                       input$pop1564_01,input$pop1564_02,
                                       input$txchomage01,input$txchomage02,
                                       input$pibHbt01,input$pibHbt02)
          # assign("dde.result",result,envir = .GlobalEnv)
          db <- compute.dde.Escalator(input$isf.year[1],input$isf.year[2],
                                      result$isf[1], result$isf[2],
                                      input$indice.year[1],input$indice.year[2],
                                      result$txdpce[1], result$txdpce[2],
                                      result$emploi[1], result$emploi[2],
                                      result$prodOccp[1],result$prodOccp[2],
                                      result$pib[1],result$pib[2])
          
          dde.g2 <- draw.escalator(db,graph.title = NULL,
                                  input$isf.year[1],input$isf.year[2],
                                  input$indice.year[1],input$indice.year[2])
          # plot(dde.g2)
          
        }else{
          showNotification(msgvalue,closeButton=TRUE,type="error")
          tmp <- data.frame(x=seq(1,10,1),y=seq(1,10,1),valeur=NA)
          #tmp <- new_data_frame(list(x=seq(1,10,1),y=seq(1,10,1),valeur=NA))
          dde.g2 <- tmp %>% ggplot(aes(x,y)) +
            geom_label(aes(x=5,y=5,label="\nDonnées préchargées indisponibles...\nSélectionnez « Paramètres avancés » et renseignez tous les champs.\n"),
                       colour="black",fill="lightblue",size=5,fontface="bold",alpha=0.5) +
            theme_void() +
            theme(text=element_text(face="bold"),
                  panel.border=element_rect(linetype="dashed",fill = NA))
          rm(tmp)
          # plot(dde.g2)
        }
      }
      
      # Sauvegarde temporaire du graphique
      assign("dde.obstacleGraph",dde.g2,envir = .GlobalEnv)
      
      outfile <- tempfile(fileext = ".png")
      pixelratio <- session$clientData$pixelratio
      
      width <- session$clientData$output_dde_obstacle_width
      height <- session$clientData$output_dde_obstacle_height 
      png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
      plot(dde.g2)
      dev.off()
      
      list(src = outfile,
           contentType = 'image/png',
           width = ifelse(width>get("fig.width02",envir = .GlobalEnv),
                          get("fig.width02",envir = .GlobalEnv),width),
           height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                          get("fig.height",envir = .GlobalEnv),height),
           alt = "Loading ...")
    }
  },deleteFile = TRUE)
  output$dde_obstacle_btt <- renderUI({
    validate(need(!is.null( get("dde.obstacleGraph",envir = .GlobalEnv)),message = "dde.obstacleGraph not null"))
    downloadButton("dde_obstacleGraph", label = "Download", class = "myButton")
  })
  output$dde_obstacleGraph <- downloadHandler(
    filename = "DDE_Obstacle.png",
    content = function(file){
      plotValue <- get("dde.obstacleGraph",envir = .GlobalEnv)
      ggsave(file, plot = plotValue, device = png())
    }
  )
  
  # Graphique sur les sources de changement du PIB/Tête
  output$dde_srce <- renderImage({
    assign("dde.srceGraph",NULL,envir = .GlobalEnv)
    validate(
      need(input$countriesList!='All',message = FALSE)
    )
    
    if(input$countriesList!='All'){
      
      dde.g3 <- NULL
      
      if(input$dataSrc=='Non' & input$countriesList %in% dde.countriesList){
        db <- dde.decomp[which(dde.decomp$pays==input$countriesList),]
        dde.g3 <- draw.sourceOfChange(db,NULL)
        # plot(dde.g3)      
      }else{
        msgvalue <- dde.checkDataUser(input$isf.year[1],input$isf.year[2],
                                      input$isf01,input$isf02,
                                      input$indice.year[1],input$indice.year[2],
                                      input$popTotal01,input$popTotal02,
                                      input$pop1564_01,input$pop1564_02,
                                      input$txchomage01,input$txchomage02,
                                      input$pibHbt01,input$pibHbt02)
        # cat("\ndde_srce::msgvalue = ",is.null(msgvalue),"; valeur = ",msgvalue,"\n")
        if(is.null(msgvalue)){
          result <- compute.dde.indice(input$isf.year[1],input$isf.year[2],
                                       input$isf01,input$isf02,
                                       input$indice.year[1],input$indice.year[2],
                                       input$popTotal01,input$popTotal02,
                                       input$pop1564_01,input$pop1564_02,
                                       input$txchomage01,input$txchomage02,
                                       input$pibHbt01,input$pibHbt02)
          db <- compute.dde.sourceOfChange(input$indice.year[1],input$indice.year[2],
                                           result$txdpce[1], result$txdpce[2],
                                           result$emploi[1], result$emploi[2],
                                           result$prodOccp[1],result$prodOccp[2],
                                           result$pib[1],result$pib[2])
          dde.g3 <- draw.sourceOfChange(db,graph.title=NULL,data.user=TRUE)
          # plot(dde.g3)
        }else{
          tmp <- data.frame(x=seq(1,10,1),y=seq(1,10,1),valeur=NA)
          #tmp <- new_data_frame(list(x=seq(1,10,1),y=seq(1,10,1),valeur=NA))
          dde.g3 <- tmp %>% ggplot(aes(x,y)) + 
            geom_label(aes(x=5,y=5,label="\nDonnées préchargées indisponibles...\nSélectionnez « Paramètres avancés » et renseignez tous les champs.\n"),
                       colour="black",fill="lightblue",size=5,fontface="bold",alpha=0.5) +
            theme_void() + 
            theme(text=element_text(face="bold"),
                  panel.border=element_rect(linetype="dashed",fill = NA))
          rm(tmp)
          # plot(dde.g3)
        }
      }
      
      # Sauvegarde temporaire du graphique
      assign("dde.srceGraph",dde.g3,envir = .GlobalEnv)
      
      outfile <- tempfile(fileext = ".png")
      pixelratio <- session$clientData$pixelratio
      
      width <- session$clientData$output_dde_srce_width
      height <- session$clientData$output_dde_srce_height 
      png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
      plot(dde.g3)
      dev.off()
      
      list(src = outfile,
           contentType = 'image/png',
           width = ifelse(width>get("fig.width02",envir = .GlobalEnv),
                          get("fig.width02",envir = .GlobalEnv),width),
           height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                          get("fig.height",envir = .GlobalEnv),height),
           alt = "Loading ...")
      
    }
  },deleteFile = TRUE)
  output$dde_srce_btt <- renderUI({
    validate(need(!is.null( get("dde.srceGraph",envir = .GlobalEnv)),message = "dde.srceGraph not null"))
    downloadButton("dde_srceGraph", label = "Download", class = "myButton")
  })
  output$dde_srceGraph <- downloadHandler(
    filename = "DDE_Source.png",
    content = function(file){
      plotValue <- get("dde.srceGraph",envir = .GlobalEnv)
      ggsave(file, plot = plotValue, device = png())
    }
  )
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # Observer d'événements
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  observeEvent(input$dataSrc,{
    
    if(input$dataSrc=="Non"){
      # updateNumericInput(session,"year01",value = NA)
      # updateNumericInput(session,"year02",value = NA)
      updateSliderInput(session,"isf.year",value = c(1975,1990))
      updateNumericInput(session,"isf01",value = NA)
      updateNumericInput(session,"isf02",value = NA)
      updateSliderInput(session,"indice.year",value = c(1990,2010))
      updateNumericInput(session,"popTotal01",value = NA)
      updateNumericInput(session,"popTotal02",value = NA)
      updateNumericInput(session,"pop1564_01",value = NA)
      updateNumericInput(session,"pop1564_02",value = NA)
      updateNumericInput(session,"pibHbt01",value = NA)
      updateNumericInput(session,"pibHbt02",value = NA)
      updateNumericInput(session,"txchomage01",value = NA)
      updateNumericInput(session,"txchomage02",value = NA)
    }else{
      db <- dde.indice[dde.indice$country==input$countriesList,]
      if(dim(db)[1]){
        # isolate({
          updateNumericInput(session,"isf01",value = db$isf01)
          updateNumericInput(session,"isf02",value = db$isf02)
          updateNumericInput(session,"popTotal01",value = db$popTotal01)
          updateNumericInput(session,"popTotal02",value = db$popTotal02)
          updateNumericInput(session,"pop1564_01",value = db$pop1564_01)
          updateNumericInput(session,"pop1564_02",value = db$pop1564_02)
          updateNumericInput(session,"pibHbt01",value = db$pibHbt01)
          updateNumericInput(session,"pibHbt02",value = db$pibHbt02)
          updateNumericInput(session,"txchomage01",value = db$txchomage01)
          updateNumericInput(session,"txchomage02",value = db$txchomage02)
        # }) 
      }
    }
  })
  
  observeEvent(input$fileDDE,{
    
    validate(
      need(input$fileDDE,message = "fileDDE not assign")
    )
    
    inFile <- input$fileDDE
    if(!is.null(inFile)){
      db.isf <- tryCatch(readxl::read_excel(inFile$datapath,sheet = "ISF"),
                         error=function(e) {readxl::read_excel(inFile$datapath,sheet = 1)})
      db.indice <- tryCatch(readxl::read_excel(inFile$datapath,sheet = "Data"),
                     error=function(e) {readxl::read_excel(inFile$datapath,sheet = 2)})
      varlist <- get("dde.varlist",envir = .GlobalEnv)
      if( dim(db.isf)[1]>=2 & all(varlist[1:2] %in% names(db.isf)) &
          dim(db.indice)[1]>=2 & all(varlist[-2] %in% names(db.indice))){
        db.indice <- db.indice[,varlist[-2]] %>% arrange(year)
        db.isf <- db.isf[,varlist[1:2]] %>% arrange(year)
        
        updateSliderInput(session,"isf.year",value = c(db.isf$year[1],db.isf$year[2]))
        updateNumericInput(session,"isf01",value = db.isf$isf[1])
        updateNumericInput(session,"isf02",value = db.isf$isf[2])
        
        # updateNumericInput(session,"year01",value = db.indice$year[1])
        # updateNumericInput(session,"year02",value = db.indice$year[1])
        updateSliderInput(session,"indice.year",value = c(db.indice$year[1],db.indice$year[2]))
        updateNumericInput(session,"popTotal01",value = db.indice$popTotal[1])
        updateNumericInput(session,"popTotal02",value = db.indice$popTotal[2])
        updateNumericInput(session,"pop1564_01",value = db.indice$pop1564_[1])
        updateNumericInput(session,"pop1564_02",value = db.indice$pop1564_[2])
        updateNumericInput(session,"pibHbt01",value = db.indice$pibHbt[1])
        updateNumericInput(session,"pibHbt02",value = db.indice$pibHbt[2])
        updateNumericInput(session,"txchomage01",value = db.indice$txchomage[1])
        updateNumericInput(session,"txchomage02",value = db.indice$txchomage[2])
        
      }else{
        showNotification("[DD-Cal] Erreur chargement des données. Structures de fichiers incohérentes",closeButton=TRUE,type="error")
      }
      
    }else{
      showNotification("[DD-Cal] Erreur chargement des données. Données absentes.",closeButton=TRUE,type="error")
    }
  })
  
  observeEvent(input$countriesList,{
    updateSliderInput(session,"rangeWindow",value = c(1975,2035))
    updateSliderInput(session,"tresholdWindow",value=80)
    updateRadioButtons(session,"dataSrc",selected ="Non")
    assign("dde.windowGraph",NULL,envir = .GlobalEnv)
    assign("dde.obstacleGraph",NULL,envir = .GlobalEnv)
    assign("dde.srceGraph",NULL,envir = .GlobalEnv)
  })
  
  observeEvent(input$isf.year,{
    
    updateNumericInput(session,"isf01",label = paste("Valeur de l'ISF en ", input$isf.year[1]),
                       value = NA)
    updateNumericInput(session,"isf02",label = paste("Valeur de l'ISF en ", input$isf.year[2]),
                       value = NA)
    
    if(input$countriesList!="All" &input$dataSrc=="Oui"){
        db <- get("isf.database",envir = .GlobalEnv) %>% 
          filter(english==input$countriesList)
        
        if(dim(db)[1]){
          isf.year01 <- (db %>% filter(year==input$isf.year[1]))$isf
          isf.year02 <- (db %>% filter(year==input$isf.year[2]))$isf
          if(!length(isf.year01))  isf.year01 <- NA
          if(!length(isf.year02))  isf.year02 <- NA
          
          # cat("[",input$countriesList,"]\n")
          # cat("year01=",input$isf.year[1]," - year02=",input$isf.year[2],"\n")  
          # cat("isf.year01=",isf.year01," - isf.year02=",isf.year02,"\n")  
          
          updateNumericInput(session,"isf01",value = round(isf.year01,2))
          updateNumericInput(session,"isf02",value = round(isf.year02,2))
        }
    }
  })
  
  observeEvent(input$indice.year,{
    output$dde.titleSource <- renderText({paste0("Source du changement du PIB/Tête entre ",input$indice.year[1],
                                                 " et ",input$indice.year[2])})
    updateNumericInput(session,"popTotal01",label = paste("Population totale en ", input$indice.year[1]),
                       value=NA)
    updateNumericInput(session,"popTotal02",label = paste("Population totale en ", input$indice.year[2]),
                       value=NA)
    updateNumericInput(session,"pop1564_01",label = paste("Population 15 à 64 ans (%) en ", input$indice.year[1]),
                       value=NA)
    updateNumericInput(session,"pop1564_02",label = paste("Population 15 à 64 ans (%) en ", input$indice.year[2]),
                       value=NA)
    updateNumericInput(session,"pibHbt01",label = paste("PIB/habitant en ", input$indice.year[1]),
                       value=NA)
    updateNumericInput(session,"pibHbt02",label = paste("PIB/habitant en ", input$indice.year[2]),
                       value=NA)
    updateNumericInput(session,"txchomage01",label = paste("Taux de chômage (%) en ", input$indice.year[1]),
                       value=NA)
    updateNumericInput(session,"txchomage02",label = paste("Taux de chômage (%) en ", input$indice.year[2]),
                       value=NA)
    
    if(input$countriesList!="All" &input$dataSrc=="Oui"){
     
      db <- get("dde.database",envir = .GlobalEnv) %>% 
        filter(english==input$countriesList & year==input$indice.year[1])
      if(dim(db)[1]){
        updateNumericInput(session,"popTotal01",value = db$popTotal[1])
        updateNumericInput(session,"pop1564_01",value = db$pop1564_[1])
        updateNumericInput(session,"pibHbt01",value = db$pibHbt[1])
        updateNumericInput(session,"txchomage01",value = db$txchomage[1])
      }
      
      db <- get("dde.database",envir = .GlobalEnv) %>% 
        filter(english==input$countriesList & year==input$indice.year[2])
      if(dim(db)[1]){
        updateNumericInput(session,"popTotal02",value = db$popTotal[1])
        updateNumericInput(session,"pop1564_02",value = db$pop1564_[1])
        updateNumericInput(session,"pibHbt02",value = db$pibHbt[1])
        updateNumericInput(session,"txchomage02",value = db$txchomage[1])
      }
       
    }
    
  })
  
  observeEvent(input$resetButton,{
    # Reinitialiser tous les champs à vide...
    # updateNumericInput(session,"year01",value = NA)
    # updateNumericInput(session,"year02",value = NA)
    updateSliderInput(session,"isf.year",value = c(1975,1990))
    updateNumericInput(session,"isf01",value = NA)
    updateNumericInput(session,"isf02",value = NA)
    updateSliderInput(session,"indice.year",value = c(1990,2010))
    updateNumericInput(session,"popTotal01",value = NA)
    updateNumericInput(session,"popTotal02",value = NA)
    updateNumericInput(session,"pop1564_01",value = NA)
    updateNumericInput(session,"pop1564_02",value = NA)
    updateNumericInput(session,"pibHbt01",value = NA)
    updateNumericInput(session,"pibHbt02",value = NA)
    updateNumericInput(session,"txchomage01",value = NA)
    updateNumericInput(session,"txchomage02",value = NA)
  })
  
  #####################################################################################################
  # Dividende scolaire
  #####################################################################################################
  
  output$dds.titleSource <- renderText({"Source du changement des dotations publiques entre 1995 et 2010"})
  
  
  output$ddsPlotApps_01 <- renderImage({ #renderPlot({
    
    validate(
      need(input$ddsCountries=='All',message = FALSE)
    )
    
    if(input$ddsCountries=='All'){
      
      dds.graphs <- get("dds.graphs",envir = .GlobalEnv)
      # outfile <- tempfile(fileext = ".png")
      outfile <- file.path(getwd(),"www","ddsPlotApps_01.png")
      
      # outfile <- normalizePath(file.path('www',"ddsPlotApps_01.png"))
      
      pixelratio <- session$clientData$pixelratio
      
      width <- session$clientData$output_ddsPlotApps_01_width
      height <- session$clientData$output_ddsPlotApps_01_height
      
      png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
      plot(dds.graphs[[1]])
      dev.off()
            list(src = outfile,
           contentType = 'image/png',
           width = ifelse(width>get("fig.width",envir = .GlobalEnv),
                          get("fig.width",envir = .GlobalEnv),width),
           height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                          get("fig.height",envir = .GlobalEnv),height),
           alt = "Loading ...")
      
      # withProgress(message = 'Creating map', value = 0.1,{
      #   dds.graphs <- draw.worldMap(dds.freins,draw.dds=TRUE)
      #   incProgress(0.5,detail="Generating data")
      #   Sys.sleep(0.1)
      #   plot(dds.graphs[[1]])
      #   assign("dds.graphs",graphs,envir = .GlobalEnv)
      #   incProgress(0.4,detail="Plotting data")
      # })
      
    } 
  },deleteFile = TRUE)
  output$ddsPlotApps_btt01 <- renderUI({
    downloadButton("ddsFullImage01", label = "Download", class = "myButton")
  })
  
  output$ddsFullImage01 <- downloadHandler(
    filename = "DDS_Afrique_01.png",
    content = function(file){
      ggsave(file, plot = dds.graphs[[1]], device = png())
    }
  )
  
  output$ddsPlotApps_02 <- renderImage({
    
    validate(
      need(input$ddsCountries=='All',message = FALSE)
    )
    
    if(input$ddsCountries=='All'){
      if(exists("dds.graphs",envir = .GlobalEnv)){
        dds.graphs <- get("dds.graphs",envir = .GlobalEnv)
        # dds.graphs[[2]]
        
        outfile <- tempfile(fileext = ".png")
        pixelratio <- session$clientData$pixelratio
        
        width <- session$clientData$output_ddsPlotApps_02_width
        height <- session$clientData$output_ddsPlotApps_02_height 
        png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
        plot(dds.graphs[[2]])
        dev.off()
        
        list(src = outfile,
             contentType = 'image/png',
             width = ifelse(width>get("fig.width",envir = .GlobalEnv),
                            get("fig.width",envir = .GlobalEnv),width),
             height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                            get("fig.height",envir = .GlobalEnv),height),
             alt = "Loading ...")
      }
    } 
  },deleteFile = TRUE)
  output$ddsPlotApps_btt02 <- renderUI({
    downloadButton("ddsFullImage02", label = "Download", class = "myButton")
  })
  output$ddsFullImage02 <- downloadHandler(
    filename = "DDS_Afrique_02.png",
    content = function(file){
      ggsave(file, plot = dds.graphs[[2]], device = png())
    }
  )
  
  output$ddsPlotApps_03 <- renderImage({
    
    validate(
      need(input$ddsCountries=='All',message = FALSE)
    )
    
    if(input$ddsCountries=='All'){
      if(exists("dds.graphs",envir = .GlobalEnv)){
        dds.graphs <- get("dds.graphs",envir = .GlobalEnv)
        # dds.graphs[[3]]
        # rm("dds.graphs",envir = .GlobalEnv)
        
        outfile <- tempfile(fileext = ".png")
        pixelratio <- session$clientData$pixelratio
        
        width <- session$clientData$output_ddsPlotApps_03_width
        height <- session$clientData$output_ddsPlotApps_03_height 
        png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
        plot(dds.graphs[[3]])
        dev.off()
        
        list(src = outfile,
             contentType = 'image/png',
             width = ifelse(width>get("fig.width",envir = .GlobalEnv),
                            get("fig.width",envir = .GlobalEnv),width),
             height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                            get("fig.height",envir = .GlobalEnv),height),
             alt = "Loading ...")
        
      }
    } 
  },deleteFile = TRUE)
  output$ddsPlotApps_btt03 <- renderUI({
    downloadButton("ddsFullImage03", label = "Download", class = "myButton")
  })
  output$ddsFullImage03 <- downloadHandler(
    filename = "DDS_Afrique_03.png",
    content = function(file){
      ggsave(file, plot = dds.graphs[[3]], device = png())
    }
  )
  
  output$ddsPlotApps_04 <- renderImage({
    
    validate(
      need(input$ddsCountries=='All',message = FALSE)
    )
    
    if(input$ddsCountries=='All'){
      if(exists("dds.graphs",envir = .GlobalEnv)){
        dds.graphs <- get("dds.graphs",envir = .GlobalEnv)
        # dds.graphs[[4]]
        # rm("graphs",envir = .GlobalEnv)
        
        outfile <- tempfile(fileext = ".png")
        pixelratio <- session$clientData$pixelratio
        
        width <- session$clientData$output_ddsPlotApps_04_width
        height <- session$clientData$output_ddsPlotApps_04_height 
        png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
        plot(dds.graphs[[4]])
        dev.off()
        
        list(src = outfile,
             contentType = 'image/png',
             width = ifelse(width>get("fig.width",envir = .GlobalEnv),
                            get("fig.width",envir = .GlobalEnv),width),
             height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                            get("fig.height",envir = .GlobalEnv),height),
             alt = "Loading ...")
        
      }
    } 
  },deleteFile = TRUE)
  output$ddsPlotApps_btt04 <- renderUI({
    downloadButton("ddsFullImage04", label = "Download", class = "myButton")
  })
  output$ddsFullImage04 <- downloadHandler(
    filename = "DDS_Afrique_04.png",
    content = function(file){
      ggsave(file, plot = dds.graphs[[4]], device = png())
    }
  )
  
  # Graphique sur la fenêtre d'opportunité du DDS
  output$dds_windowCountry <- renderImage({
    
    assign("dds.windowGraph",NULL,envir = .GlobalEnv)
    
    validate(
      need(input$ddsCountries!='All',message = FALSE)
    )
    
    if(input$ddsCountries!='All'){
      db <- txdpce[which(txdpce$country==input$ddsCountries),]
      dds.g1 <- suppressWarnings(draw.windowCountry(db,graph.title=NULL,
                                                x.start=input$ddsRangeWindow[1],x.end=input$ddsRangeWindow[2],
                                                seuil=input$ddsTresholdWindow))
      # plot(dds.g1)
      
      # Sauvegarde temporaire du graphique
      assign("dds.windowGraph",dds.g1,envir = .GlobalEnv)
      
      outfile <- tempfile(fileext = ".png")
      pixelratio <- session$clientData$pixelratio
      
      width <- session$clientData$output_dds_windowCountry_width
      height <- session$clientData$output_dds_windowCountry_height 
      png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
      plot(dds.g1)
      dev.off()
      
      list(src = outfile,
           contentType = 'image/png',
           width = ifelse(width>get("fig.width02",envir = .GlobalEnv),
                          get("fig.width02",envir = .GlobalEnv),width),
           height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                          get("fig.height",envir = .GlobalEnv),height),
           alt = "Loading ...")
      
    }
    
  },deleteFile = TRUE)
  output$dds_windowCountry_btt <- renderUI({
    validate(need(!is.null( get("dds.windowGraph",envir = .GlobalEnv)),message = "dds.windowGraph not null"))
    downloadButton("dds_Window", label = "Download", class = "myButton")
  })
  output$dds_Window <- downloadHandler(
    filename = "DDS_Window.png",
    content = function(file){
      ggsave(file, plot = dds.windowGraph, device = png())
    }
  )
  
  # Graphique sur la chaîne de production du DDS
  output$dds_obstacle <- renderImage({
    
    assign("dds.obstacleGraph",NULL,envir = .GlobalEnv)
    
    validate(
      need(input$ddsCountries!='All',message = FALSE)
    )
    
    if(input$ddsCountries!='All'){
      
      dds.g2 <- NULL
      
      if(input$ddsDataSource=='Non' & input$ddsCountries %in% dds.countriesList){
        db <- dds.freins[which(dds.freins$pays==input$ddsCountries),]
        dds.g2 <- draw.escalator(db,graph.title=NULL,draw.dds=TRUE,
                                 isf.ystart=1980,isf.yend=1995,ystart=1995,yend=2010)
        # plot(dds.g2) 
      }else{
        msgvalue <- dds.checkDataUser(input$dds.isfYear[1],input$dds.isfYear[2],
                                      input$dds.isf01,input$dds.isf02,
                                      input$dds.indiceYear[1],input$dds.indiceYear[2],
                                      input$ddsPopTotal01,input$ddsPopTotal02,
                                      input$dpceAge01,input$dpceAge02,
                                      input$dpceEco01,input$dpceEco02,
                                      input$dpceYouth01,input$dpceYouth02,
                                      input$ddsPop1564_01,input$ddsPop1564_02,
                                      input$gni01,input$gni02,input$k01,input$k02,
                                      input$tbs01,input$tbs02)
        # cat("\ndde_obstacle::msgvalue = ",is.null(msgvalue),"; valeur = ",msgvalue,"\n")
        if(is.null(msgvalue)){
          result <- compute.dds.indice(input$ddsPopTotal01,input$ddsPopTotal02,
                                       input$dpceAge01,input$dpceAge02,
                                       input$dpceEco01,input$dpceEco02,
                                       input$dpceYouth01,input$dpceYouth02,
                                       input$ddsPop1564_01,input$ddsPop1564_02,
                                       input$gni01,input$gni02,input$k01,input$k02)
          # assign("ddsresult",result,envir = .GlobalEnv)
          db <- compute.dds.Escalator(input$dds.isfYear[1],input$dds.isfYear[2],
                                      input$dds.isf01,input$dds.isf02,
                                      input$dds.indiceYear[1],input$dds.indiceYear[2],
                                      input$dpceAge01,input$dpceAge02,
                                      input$dpceEco01,input$dpceEco02,
                                      input$tbs01,input$tbs02,result)
          
          dds.g2 <- draw.escalator(db,graph.title = NULL,
                                   input$dds.isfYear[1],input$dds.isfYear[2],
                                   input$dds.indiceYear[1],input$dds.indiceYear[2],
                                   draw.dds=TRUE)
          # plot(dds.g2)
          
        }else{
          showNotification(msgvalue,closeButton=TRUE,type="error")
          tmp <- data.frame(x=seq(1,10,1),y=seq(1,10,1),valeur=NA)
          #tmp <- new_data_frame(list(x=seq(1,10,1),y=seq(1,10,1),valeur=NA))
          dds.g2 <- tmp %>% ggplot(aes(x,y)) +
            geom_label(aes(x=5,y=5,label="\nDonnées préchargées indisponibles...\nSélectionnez « Paramètres avancés » et renseignez tous les champs.\n"),
                       colour="black",fill="lightblue",size=5,fontface="bold",alpha=0.5) +
            theme_void() +
            theme(text=element_text(face="bold"),
                  panel.border=element_rect(linetype="dashed",fill = NA))
          rm(tmp)
          # plot(dds.g2)
        }
      }
      
      # Sauvegarde temporaire du graphique
      assign("dds.obstacleGraph",dds.g2,envir = .GlobalEnv)
      
      outfile <- tempfile(fileext = ".png")
      pixelratio <- session$clientData$pixelratio
      
      width <- session$clientData$output_dds_obstacle_width
      height <- session$clientData$output_dds_obstacle_height 
      png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
      plot(dds.g2)
      dev.off()
      
      list(src = outfile,
           contentType = 'image/png',
           width = ifelse(width>get("fig.width02",envir = .GlobalEnv),
                          get("fig.width02",envir = .GlobalEnv),width),
           height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                          get("fig.height",envir = .GlobalEnv),height),
           alt = "Loading ...")
      
    }
  },deleteFile = TRUE)
  output$dds_obstacle_btt <- renderUI({
    validate(need(!is.null( get("dds.obstacleGraph",envir = .GlobalEnv)),message = "dds.obstacleGraph not null"))
    downloadButton("dds_obstacleGraph", label = "Download", class = "myButton")
  })
  output$dds_obstacleGraph <- downloadHandler(
    filename = "DDS_Obstacle.png",
    content = function(file){
      plotValue <- get("dds.obstacleGraph",envir = .GlobalEnv)
      ggsave(file, plot = plotValue, device = png())
    }
  )
  
  # Graphique sur les sources de changement des dotations publiques
  output$dds_srce <- renderImage({
    
    assign("dds.srceGraph",NULL,envir = .GlobalEnv)
    validate(
      need(input$ddsCountries!='All',message = FALSE)
    )
    
    if(input$ddsCountries!='All'){
      
      dds.g3 <- NULL
      
      if(input$ddsDataSource=='Non' & input$ddsCountries %in% dds.countriesList){
        db <- dds.decomp[which(dds.decomp$pays==input$ddsCountries),]
        dds.g3 <- draw.sourceOfChange(db,NULL,draw.dds=TRUE)
        # plot(dds.g3)      
      }else{
        msgvalue <- dds.checkDataUser(input$dds.isfYear[1],input$dds.isfYear[2],
                                      input$dds.isf01,input$dds.isf02,
                                      input$dds.indiceYear[1],input$dds.indiceYear[2],
                                      input$ddsPopTotal01,input$ddsPopTotal02,
                                      input$dpceAge01,input$dpceAge02,
                                      input$dpceEco01,input$dpceEco02,
                                      input$dpceYouth01,input$dpceYouth02,
                                      input$ddsPop1564_01,input$ddsPop1564_02,
                                      input$gni01,input$gni02,input$k01,input$k02,
                                      input$tbs01,input$tbs02)
        # cat("\ndde_srce::msgvalue = ",is.null(msgvalue),"; valeur = ",msgvalue,"\n")
        if(is.null(msgvalue)){
          result <- compute.dds.indice(input$ddsPopTotal01,input$ddsPopTotal02,
                                       input$dpceAge01,input$dpceAge02,
                                       input$dpceEco01,input$dpceEco02,
                                       input$dpceYouth01,input$dpceYouth02,
                                       input$ddsPop1564_01,input$ddsPop1564_02,
                                       input$gni01,input$gni02,input$k01,input$k02)
          db <- compute.dds.sourceOfChange(result,input$dds.indiceYear[1],input$dds.indiceYear[2])
          dds.g3 <- draw.sourceOfChange(db,graph.title=NULL,data.user=TRUE,draw.dds=TRUE)
          # plot(dds.g3)
        }else{
          tmp <- data.frame(x=seq(1,10,1),y=seq(1,10,1),valeur=NA)
          # tmp <- new_data_frame(list(x=seq(1,10,1),y=seq(1,10,1),valeur=NA))
          dds.g3 <- tmp %>% ggplot(aes(x,y)) + 
            geom_label(aes(x=5,y=5,label="\nDonnées préchargées indisponibles...\nSélectionnez « Paramètres avancés » et renseignez tous les champs.\n"),
                       colour="black",fill="lightblue",size=5,fontface="bold",alpha=0.5) +
            theme_void() + 
            theme(text=element_text(face="bold"),
                  panel.border=element_rect(linetype="dashed",fill = NA))
          rm(tmp)
          # plot(dds.g3)
        }
      }
      
      # Sauvegarde temporaire du graphique
      assign("dds.srceGraph",dds.g3,envir = .GlobalEnv)
      
      outfile <- tempfile(fileext = ".png")
      pixelratio <- session$clientData$pixelratio
      
      width <- session$clientData$output_dds_srce_width
      height <- session$clientData$output_dds_srce_height 
      png(outfile,width = width*pixelratio,height = height*pixelratio,res=72*pixelratio)
      plot(dds.g3)
      dev.off()
      
      list(src = outfile,
           contentType = 'image/png',
           width = ifelse(width>get("fig.width02",envir = .GlobalEnv),
                          get("fig.width02",envir = .GlobalEnv),width),
           height =ifelse(height>get("fig.height",envir = .GlobalEnv),
                          get("fig.height",envir = .GlobalEnv),height),
           alt = "Loading ...")
      
    }
  },deleteFile = TRUE)
  output$dds_srce_btt <- renderUI({
    validate(need(!is.null( get("dds.srceGraph",envir = .GlobalEnv)),message = "dds.srceGraph not null"))
    downloadButton("dds_srceGraph", label = "Download", class = "myButton")
  })
  output$dds_srceGraph <- downloadHandler(
    filename = "DDS_Source.png",
    content = function(file){
      plotValue <- get("dds.srceGraph",envir = .GlobalEnv)
      ggsave(file, plot = plotValue, device = png())
    }
  )
  
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  # Observer d'événements
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  observeEvent(input$ddsDataSource,{
    
    if(input$ddsDataSource=="Non"){
      updateSliderInput(session,"dds.isfYear",value = c(1980,1995))
      updateNumericInput(session,"dds.isf01",value = NA)
      updateNumericInput(session,"dds.isf02",value = NA)
      updateSliderInput(session,"dds.indiceYear",value = c(1995,2010))
      updateNumericInput(session,"ddsPopTotal01",value = NA)
      updateNumericInput(session,"ddsPopTotal02",value = NA)
      updateNumericInput(session,"ddsPop1564_01",value = NA)
      updateNumericInput(session,"ddsPop1564_02",value = NA)
      updateNumericInput(session,"dpceAge01",value = NA)
      updateNumericInput(session,"dpceAge02",value = NA)
      updateNumericInput(session,"dpceEco01",value = NA)
      updateNumericInput(session,"dpceEco02",value = NA)
      updateNumericInput(session,"dpceYouth01",value = NA)
      updateNumericInput(session,"dpceYouth02",value = NA)
      updateNumericInput(session,"gni01",value = NA)
      updateNumericInput(session,"gni02",value = NA)
      updateNumericInput(session,"k01",value = NA)
      updateNumericInput(session,"k02",value = NA)
      updateNumericInput(session,"tbs01",value = NA)
      updateNumericInput(session,"tbs02",value = NA)
    }else{
      db <- dds.indice[dds.indice$country==input$ddsCountries,]
      if(dim(db)[1]){
        
        # cat("\n--------------------------------------------\n")
        # cat("[Observe.ddsDataSource]\n")
        # cat(paste(names(db),"=",t(db),collapse = "\n"))
        # cat("\n--------------------------------------------\n")
        
        updateSliderInput(session,"dds.isfYear",value = c(1980,1995))
        updateNumericInput(session,"dds.isf01",value = db$isf_1995)
        updateNumericInput(session,"dds.isf02",value = db$isf_2010)
        
        updateSliderInput(session,"dds.indiceYear",value = c(1995,2010))
        updateNumericInput(session,"ddsPopTotal01",value = db$popTot_1995)
        updateNumericInput(session,"ddsPopTotal02",value = db$popTot_2010)
        updateNumericInput(session,"ddsPop1564_01",value = db$pop1564_1995)
        updateNumericInput(session,"ddsPop1564_02",value = db$pop1564_2010)
        updateNumericInput(session,"dpceAge01",value = db$txdpce_age_1995)
        updateNumericInput(session,"dpceAge02",value = db$txdpce_age_2010)
        updateNumericInput(session,"dpceEco01",value = db$txdpce_real_1995)
        updateNumericInput(session,"dpceEco02",value = db$txdpce_real_2010)
        updateNumericInput(session,"dpceYouth01",value = db$youth.dpce_1995)
        updateNumericInput(session,"dpceYouth02",value = db$youth.dpce_2010)
        updateNumericInput(session,"gni01",value = db$gni_1995)
        updateNumericInput(session,"gni02",value = db$gni_2010)
        updateNumericInput(session,"k01",value = db$k_1995)
        updateNumericInput(session,"k02",value = db$k_2010)
        updateNumericInput(session,"tbs01",value = db$tbs_1995)
        updateNumericInput(session,"tbs02",value = db$tbs_2010)
      }
    }
  })
  
  observeEvent(input$fileDDS,{
    
    validate(
      need(input$fileDDS,message = "fileDDS not assign")
    )
    
    inFile <- input$fileDDS
    if(!is.null(inFile)){
      db.isf <- tryCatch(readxl::read_excel(inFile$datapath,sheet = "ISF"),
                         error=function(e) {readxl::read_excel(inFile$datapath,sheet = 1)})
      db.indice <- tryCatch(readxl::read_excel(inFile$datapath,sheet = "Data"),
                            error=function(e) {readxl::read_excel(inFile$datapath,sheet = 2)})
      varlist <- get("dds.varlist",envir = .GlobalEnv)
      if( dim(db.isf)[1]>=2 & all(varlist[1:2] %in% names(db.isf)) &
          dim(db.indice)[1]>=2 & all(varlist[-2] %in% names(db.indice))){
        db.indice <- db.indice[,varlist[-2]] %>% arrange(year)
        db.isf <- db.isf[,varlist[1:2]] %>% arrange(year)
        
        updateSliderInput(session,"dds.isfYear",value = c(db.isf$year[1],db.isf$year[2]))
        updateNumericInput(session,"dds.isf01",value = db.isf$isf[1])
        updateNumericInput(session,"dds.isf02",value = db.isf$isf[2])
        
        updateSliderInput(session,"dds.indiceYear",value = c(db.indice$year[1],db.indice$year[2]))
        updateNumericInput(session,"ddsPopTotal01",value = db.indice$popTot[1])
        updateNumericInput(session,"ddsPopTotal02",value = db.indice$popTot[2])
        updateNumericInput(session,"ddsPop1564_01",value = db.indice$pop1564[1])
        updateNumericInput(session,"ddsPop1564_02",value = db.indice$pop1564[2])
        updateNumericInput(session,"dpceAge01",value = db.indice$txdpce_age[1])
        updateNumericInput(session,"dpceAge02",value = db.indice$txdpce_age[2])
        updateNumericInput(session,"dpceEco01",value = db.indice$txdpce_real[1])
        updateNumericInput(session,"dpceEco02",value = db.indice$txdpce_real[2])
        updateNumericInput(session,"dpceYouth01",value = db.indice$youth.dpce[1])
        updateNumericInput(session,"dpceYouth02",value = db.indice$youth.dpce[2])
        updateNumericInput(session,"gni01",value = db.indice$gni[1])
        updateNumericInput(session,"gni02",value = db.indice$gni[2])
        updateNumericInput(session,"k01",value = db.indice$k[1])
        updateNumericInput(session,"k02",value = db.indice$k[2])
        updateNumericInput(session,"tbs01",value = db.indice$tbs[1])
        updateNumericInput(session,"tbs02",value = db.indice$tbs[2])
        
      }else{
        showNotification("[DD-Cal] Erreur chargement des données. Structures de fichiers incohérentes",closeButton=TRUE,type="error")
      }
      
    }else{
      showNotification("[DD-Cal] Erreur chargement des données. Données absentes.",closeButton=TRUE,type="error")
    }
  })
  
  observeEvent(input$ddsCountries,{
    updateSliderInput(session,"ddsRangeWindow",value = c(1980,2035))
    updateSliderInput(session,"ddsTresholdWindow",value=80)
    updateRadioButtons(session,"ddsDataSource",selected ="Non")
    assign("dds.windowGraph",NULL,envir = .GlobalEnv)
    assign("dds.obstacleGraph",NULL,envir = .GlobalEnv)
    assign("dds.srceGraph",NULL,envir = .GlobalEnv)
  })
  
  observeEvent(input$dds.isfYear,{
    updateNumericInput(session,"dds.isf01",label = paste("Valeur de l'ISF en ", input$dds.isfYear[1]),
                       value = NA)
    updateNumericInput(session,"dds.isf02",label = paste("Valeur de l'ISF en ", input$dds.isfYear[2]),
                       value=NA)
    
    if(input$ddsCountries!="All" &input$ddsDataSource=="Oui"){
      db <- get("isf.database",envir = .GlobalEnv) %>% 
        filter(english==input$ddsCountries)
      
      if(dim(db)[1]){
        isf.year01 <- (db %>% filter(year==input$dds.isfYear[1]))$isf
        isf.year02 <- (db %>% filter(year==input$dds.isfYear[2]))$isf
        if(!length(isf.year01))  isf.year01 <- NA
        if(!length(isf.year02))  isf.year02 <- NA
        
        # cat("[",input$ddsCountries,"]\n")
        # cat("year01=",input$dds.isfYear[1]," - year02=",input$dds.isfYear[2],"\n")  
        # cat("isf.year01=",isf.year01," - isf.year02=",isf.year02,"\n")  
        
        updateNumericInput(session,"dds.isf01",value = round(isf.year01,2))
        updateNumericInput(session,"dds.isf02",value = round(isf.year02,2))
      }
    }
    
  })
  
  observeEvent(input$dds.indiceYear,{
    output$dds.titleSource <- renderText({paste0("Source du changement des dotations publiques entre ",input$dds.indiceYear[1],
                                                 " et ",input$dds.indiceYear[2])})
    updateNumericInput(session,"ddsPopTotal01",label = paste("Population totale en", input$dds.indiceYear[1]),
                       value=NA)
    updateNumericInput(session,"ddsPopTotal02",label = paste("Population totale en", input$dds.indiceYear[2]),
                       value=NA)
    updateNumericInput(session,"ddsPop1564_01",label = paste("Population 15 à 64 ans en", input$dds.indiceYear[1],"(%)"),
                       value=NA)
    updateNumericInput(session,"ddsPop1564_02",label = paste("Population 15 à 64 ans en", input$dds.indiceYear[2],"(%)"),
                       value=NA)
    updateNumericInput(session,"dpceAge01",label = paste("Taux de dépendance démographique total", input$dds.indiceYear[1],"(%)"),
                       value=NA)
    updateNumericInput(session,"dpceAge02",label = paste("Taux de dépendance démographique total", input$indice.year[2],"(%)"),
                       value=NA)
    updateNumericInput(session,"dpceEco01",label = paste("Taux de dépendance économique en", input$dds.indiceYear[1],"(%)"),
                       value=NA)
    updateNumericInput(session,"dpceEco02",label = paste("Taux de dépendance économique en", input$dds.indiceYear[2],"(%)"),
                       value=NA)
    updateNumericInput(session,"dpceYouth01",label = paste("Taux de dépendance des jeunes en ", input$dds.indiceYear[1],"(%)"),
                       value=NA)
    updateNumericInput(session,"dpceYouth02",label = paste("Taux de dépendance des jeunes en ", input$dds.indiceYear[2],"(%)"),
                       value=NA)
    updateNumericInput(session,"gni01",label = paste("Produit national brut en", input$dds.indiceYear[1],"(%)"),
                       value=NA)
    updateNumericInput(session,"gni02",label = paste("Produit national brut en", input$dds.indiceYear[2],"(%)"),
                       value=NA)
    updateNumericInput(session,"k01",label = paste("Part du budget alloué à l'éducation en", input$dds.indiceYear[1],"(%)"),
                       value=NA)
    updateNumericInput(session,"k02",label = paste("Part du budget alloué à l'éducation en", input$dds.indiceYear[2],"(%)"),
                       value=NA)
    updateNumericInput(session,"tbs01",label = paste("Taux brut de scolarisation en", input$dds.indiceYear[1],"(%)"),
                       value=NA)
    updateNumericInput(session,"tbs02",label = paste("Taux brut de scolarisation en", input$dds.indiceYear[2],"(%)"),
                       value=NA)
    
    if(input$ddsCountries!="All" &input$ddsDataSource=="Oui"){
      
      db <- get("dds.database",envir = .GlobalEnv) %>% 
        filter(english==input$ddsCountries & year==input$dds.indiceYear[1])
      if(dim(db)[1]){
        updateNumericInput(session,"ddsPopTotal01",value=db$popTotal[1])
        updateNumericInput(session,"ddsPop1564_01",value=db$pop1564[1])
        updateNumericInput(session,"dpceAge01",value=db$dpceAge[1])
        updateNumericInput(session,"dpceEco01",value=db$dpceEco[1])
        updateNumericInput(session,"dpceYouth01",value=db$dpceYouth[1])
        updateNumericInput(session,"gni01",value=db$gni[1])
        updateNumericInput(session,"k01",value=db$k[1])
        updateNumericInput(session,"tbs01",value=db$tbs[1])
      }
      
      db <- get("dds.database",envir = .GlobalEnv) %>% 
        filter(english==input$ddsCountries & year==input$dds.indiceYear[2])
      if(dim(db)[1]){
        updateNumericInput(session,"ddsPopTotal02",value=db$popTotal[1])
        updateNumericInput(session,"ddsPop1564_02",value=db$pop1564[1])
        updateNumericInput(session,"dpceAge02",value=db$dpceAge[1])
        updateNumericInput(session,"dpceEco02",value=db$dpceEco[1])
        updateNumericInput(session,"dpceYouth02",value=db$dpceYouth[1])
        updateNumericInput(session,"gni02",value=db$gni[1])
        updateNumericInput(session,"k02",value=db$k[1])
        updateNumericInput(session,"tbs02",value=db$tbs[1])
      }
      
    }
    
  })
  
  observeEvent(input$ddsResetButton,{
    # Reinitialiser tous les champs à vide...
    updateSliderInput(session,"dds.isfYear",value = c(1980,1995))
    updateNumericInput(session,"dds.isf01",value = NA)
    updateNumericInput(session,"dds.isf02",value = NA)
    updateSliderInput(session,"dds.indiceYear",value = c(1995,2010))
    updateNumericInput(session,"ddsPopTotal01",value = NA)
    updateNumericInput(session,"ddsPopTotal02",value = NA)
    updateNumericInput(session,"ddsPop1564_01",value = NA)
    updateNumericInput(session,"ddsPop1564_02",value = NA)
    updateNumericInput(session,"dpceAge01",value = NA)
    updateNumericInput(session,"dpceAge02",value = NA)
    updateNumericInput(session,"dpceEco01",value = NA)
    updateNumericInput(session,"dpceEco02",value = NA)
    updateNumericInput(session,"dpceYouth01",value = NA)
    updateNumericInput(session,"dpceYouth02",value = NA)
    updateNumericInput(session,"gni01",value = NA)
    updateNumericInput(session,"gni02",value = NA)
    updateNumericInput(session,"k01",value = NA)
    updateNumericInput(session,"k02",value = NA)
    updateNumericInput(session,"tbs01",value = NA)
    updateNumericInput(session,"tbs02",value = NA)
  })
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Construction de l'application
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
shinyApp(ui=ui,server = server)

# Lancement de l'application : shiny::runApp('DDApp')
# save.image("")


