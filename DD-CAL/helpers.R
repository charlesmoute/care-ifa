
# Tracer des cartes
# library(maptools)
# gpclibPermit()
# gpclibPermitStatus()

library(rgeos)
library(rworldmap)
library(rworldxtra)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Fonctions utilitaires
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Enable gpclib
# library(gpclib)
# library(maptools)
# gpclibPermit()

#Réencodage des chaines de caractéres en UTF-8
#sapply(iconvlist(),function(x,data=tmp)iconv(data,'UTF-8',x))
encoding <- function(dbase){
  
  for(variable in names(dbase)) {
    if(!is.null(levels(dbase[,variable]))){
      tmp <- as.character(dbase[,variable])
      tmp <- iconv(tmp,"latin1",'UTF-8') #réencodage
      dbase[,variable] <- factor(tmp)
    }
  }
  
  return(dbase)
}

# Verification de la cohérence des données saisies pour le DDE
dde.checkDataUser <- function(isf.ystart,isf.yend,isf.val01,isf.val02,
                           ystart,yend,popTot.val01,popTot.val02,
                           pop1564.val01,pop1564.val02,
                           emp.val01,emp.val02,
                           pib.val01,pib.val02){
  errmsg <- NULL
  
  # cat("isf.ystart= ",isf.ystart,"isf.yend=",isf.yend,
  #     "isf.val01=",isf.val01,"isf.val02=",isf.val02,
  #     "ystart=",ystart,"yend=",yend,"popTot.val01=",popTot.val01,
  #     "popTot.val02=",popTot.val02,"pop1564.val01=",pop1564.val01,
  #     "pop1564.val02=",pop1564.val02,"emp.val01=",emp.val01,
  #     "emp.val02=",emp.val02,"pib.val01=",pib.val01,
  #     "pib.val02= ",pib.val02)
  
  if(is.na(isf.ystart) | is.na(isf.yend) | is.na(isf.val01) | is.na(isf.val02) |
     is.na(ystart) | is.na(yend) | is.na(popTot.val01) | is.na(popTot.val02) |
     is.na(pop1564.val01) | is.na(pop1564.val02) | is.na(emp.val01) | 
     is.na(emp.val02) | is.na(pib.val01) | is.na(pib.val02)){
    return("Bien vouloir,  renseignez tous les champs sous « Paramètres avancés ».")
  }
  
  if(isf.ystart==isf.yend) 
    errmsg <- c(errmsg,
                sprintf("Les années de début et de fin d'observation du changement de l'ISF doivent être différente d'au moins une année",ystart,yend))
  
  
  if((isf.ystart!=isf.yend) & ystart==yend) 
    errmsg <- c(errmsg,
                sprintf("Les années de début et de fin d'observation du changement des indicateurs doivent être différentes d'au moins une année",ystart,yend))
  
  if(ystart<isf.yend | yend<=isf.yend)
    errmsg <- c(errmsg,
                "La période d'observation des indicateurs doit être postérieure à celle de l'ISF")
  
  return(errmsg)
}

# Calcul des indicateurs utilisés pour l'estimation du DDE
compute.dde.indice <- function(isf.ystart,isf.yend,isf.val01,isf.val02,
                               ystart,yend,popTot.val01,popTot.val02,
                               pop1564.val01,pop1564.val02,
                               emp.val01,emp.val02,
                               pib.val01,pib.val02){
  
  # isf.ystart=1970
  # isf.yend=1985
  # isf.val01=6.264
  # isf.val02=4.712
  # ystart=1985
  # yend=1995
  # popTot.val01=54323648
  # popTot.val02=69835715
  # pop1564.val01=54.97112786
  # pop1564.val02=57.15320306
  # emp.val01=6.070000172
  # emp.val02=3.808000088
  # pib.val01=1380.681898
  # pib.val02=1506.5843
  
  # Compute
  pib <- c(start=as.numeric(pib.val01)*as.numeric(popTot.val01),
           end=as.numeric(pib.val02)*as.numeric(popTot.val02))
  
  pop1564 <- c(start=as.numeric(pop1564.val01)*as.numeric(popTot.val01),
               end=as.numeric(pop1564.val02)*as.numeric(popTot.val02))/100
  
  popOccp <- pop1564 * (c(start=(100-as.numeric(emp.val01)),end=(100-as.numeric(emp.val02)))/100)
  
  effet.age <- pop1564/c(start=as.numeric(popTot.val01),end=as.numeric(popTot.val02)) # txdpce=c(pop1564.val01,pop1564.val02)
  effet.empl <- popOccp/pop1564 # Proportion des actifs effectivement occupés
  effet.prod <- pib/popOccp # Revenu par individu
  
  result <- list(isf=c(start=as.numeric(isf.val01),end=as.numeric(isf.val02)),
                 txdpce=effet.age,emploi=effet.empl,
                 prodOccp=effet.prod,
                 pib=c(start=as.numeric(pib.val01),end=as.numeric(pib.val02)))
  
  return(result)
}

# Calcul des indicateurs permettant l'identification des sources de changement 
# du PIB/Tête
compute.dde.sourceOfChange <- function (startYear,endYear,txdpce.start,txdpce.end,emp.start,
                                        emp.end,prodOccp.start,prodOccp.end,pib.start,pib.end){
  
  diff.pib <- pib.end - pib.start
  dpce.age <- (txdpce.end-txdpce.start) * mean(c(emp.start,emp.end)) * mean(c(prodOccp.start,prodOccp.end))
  productivite <- (prodOccp.end-prodOccp.start) * mean(c(emp.start,emp.end)) * mean(c(txdpce.start,txdpce.end))
  emploi <- (emp.end-emp.start) * mean(c(prodOccp.start,prodOccp.end)) * mean(c(txdpce.start,txdpce.end))
  
  # dpce.age <- round(100*dpce.age/diff.pib,1)
  # productivite <- round(100*productivite/diff.pib,1)
  # emploi <- round(100*emploi/diff.pib,1)
  
  db <- data.frame(pays="User",periode=sprintf("%d-%d",startYear,endYear),
                   diff.pib,dpce.age,productivite,emploi,effet="absolu")
  return(db)
}

# Calcul des indicateurs permettant le tracé de la chaîne de production du dividende 
# économique
compute.dde.Escalator <- function(isf.startYear, isf.endYear,isf.start,isf.end,
                                  startYear,endYear,txdpce.start,txdpce.end,
                                  emp.start,emp.end,prodOccp.start,prodOccp.end,
                                  pib.start,pib.end){
  
  # isf.startYear <- dde.result$isf[1] 
  # isf.endYear <- dde.result$isf[2]
  # isf.start <- db$isf01 
  # isf.end <- db$isf02
  # startYear <- 1990
  # endYear <- 2010
  # txdpce.start <- dde.result$txdpce[1]
  # txdpce.end <- dde.result$txdpce[2]
  # emp.start <-dde.result$emploi[1]
  # emp.end <- dde.result$emploi[2]
  # prodOccp.start <- dde.result$prodOccp[1]
  # prodOccp.end <- dde.result$prodOccp[2]
  # pib.start <- dde.result$pib[1]
  # pib.end <- dde.result$pib[2]
  
  isf.interval <- isf.endYear-isf.startYear+1
  idx.interval <- endYear-startYear+1
  
  tx.fecondite <- 1-((isf.end/isf.start)^(1/isf.interval))
  dpce.age <- 1 - ((txdpce.end/txdpce.start)^(1/idx.interval))
  tx.emploi <- ((emp.end/emp.start)^(1/idx.interval)) -1
  productivite <- ((prodOccp.end/prodOccp.start)^(1/idx.interval))-1
  pib <- ((pib.end/pib.start)^(1/idx.interval))-1
  
  c1 <- round(dpce.age/tx.fecondite,1)
  c2 <- round(tx.emploi/dpce.age,1)
  c3 <- round(productivite/tx.emploi,1)
  c4 <- round(pib/productivite,1)
  
  tx.fecondite <- round(100*tx.fecondite,1) #tx.fecondite
  dpce.age <- round(100*dpce.age,1)
  tx.emploi <- round(100*tx.emploi,1)
  productivite <- round(100*productivite,1)
  pib <- round(100*pib,1)
    
  db <- data.frame(pays=NA,code=NA,tx.fecondite,c1,dpce.age,c2,tx.emploi,c3,productivite,c4,pib)
  return(db)
}

draw.worldMap <- function(db,draw.dds=FALSE){
  
  labs <- c("(1) Baisse de la fécondité >> Struture par âge\n",
            "(2) Structure par âge >> Emploi\n",
            "(3) Emploi >> Productivité des occupés\n",
            "(4) Productivité des occupés >> PIB/Tête \n")
  # labs <- sprintf("Transitions : %s",labs)
  names(labs) <- c("c1","c2","c3","c4")
  
  varlist <- list(c1=c("tx.fecondite","dpce.age"),
                  c2=c("dpce.age","tx.emploi"),
                  c3=c("tx.emploi","productivite"),
                  c4=c("productivite","pib"))
  
  if(draw.dds){
    labs <- c("(1) Baisse de la fécondité >> Struture par âge\n",
              "(2) Structure par âge >> Dépendance économique\n",
              "(3) Dépendance économique >> Ressources par élève\n",
              "(4) Ressources par élève >> Education\n")
    # labs <- sprintf("Transitions : %s",labs)
    names(labs) <- c("c1","c2","c3","c4")
    
    varlist <- list(c1=c("txfec","dpce.demo"),
                    c2=c("dpce.demo","dpce.reel"),
                    c3=c("dpce.reel","resource"),
                    c4=c("resource","tbs"))
  }
  
  for(coef in names(varlist)){
    varname <- varlist[[coef]]
    x1 <- db[,varname[1]]
    x2 <- db[,varname[2]]
    new.coef01 <- sprintf("%s.lab",coef)
    new.coef02 <- sprintf("%s.val",coef)
    new.coef03 <- sprintf("%s.color",coef)
    
    db[,new.coef01] <- "Données non disponibles"
    db[,new.coef02] <- 4
    db[,new.coef03] <- "darkgrey"
    
    rowid <- which(x1<=0 & x2<=0)
    db[rowid,new.coef01] <- "Transition freinée"
    db[rowid,new.coef02] <- 3
    db[rowid,new.coef03] <- "darkred"
    
    rowid <- which(x1>0 & x2>0)
    db[rowid,new.coef01] <- "Transition effectuée"
    db[rowid,new.coef02] <- 1
    db[rowid,new.coef03] <- "darkgreen"
    
    rowid <- which((x1<=0 & x2>0)|(x1>0 & x2<=0))
    db[rowid,new.coef01] <- "Transition mitigée"
    db[rowid,new.coef02] <- 2
    db[rowid,new.coef03] <- "darkorange"
    
  }
  
  rm(rowid,new.coef01,new.coef02,new.coef03,x1,x2,varname,coef,varlist)
  
  worldmap <- getMap(resolution = 'high')
  # olmar <- c(5.1,4.1,4.1,2.1)
  africa <- worldmap[which(worldmap$REGION=="Africa"),]
  # Ajout des données à la carte
  africa <- merge(africa,db,by.x="ISO3",by.y="code",all.x=TRUE)
  rm(db)
  
  # Ajout de la couleur Gris pour les données manquantes
  varlist <- names(labs)
  for(varname in varlist){
    rowid <- which(is.na(africa@data[,varname]))
    new.coef01 <- sprintf("%s.lab",varname)
    new.coef02 <- sprintf("%s.val",varname)
    new.coef03 <- sprintf("%s.color",varname)
    
    africa@data[rowid,new.coef01] <- "Données non disponibles"
    africa@data[rowid,new.coef02] <- 4
    africa@data[rowid,new.coef03] <- "darkgrey"
  }
  rm(varname,varlist,rowid,new.coef01,new.coef02,new.coef03)
  
  legend.data <- unique(africa@data[,c("c1.lab","c1.val","c1.color")])
  names(legend.data) <- c("lab","val","color")
  legend.data <- as.data.frame(legend.data %>% arrange(val))
  
  asp.value <- 1
  mar.value <- c(0,1,3,1)
  x.limit <- c(5,25) # c(3,22)
  y.limit <- c(-36,38)
  
  # La carte de l'Afrique sans les coloris
  # par(mar=c(0,0,0,0))
  # plot(africa,asp=asp.value,xlim=x.limit,ylim=y.limit)
  
  # Production de la carte avec les couleurs pour chaque inddicateur (Methode 1)
  # layout(matrix(c(1,2,3,4),2,2,byrow=TRUE),
  #        widths=5,heights=5,respect = TRUE) #rep.int(10,2) #
  # for(varname in names(labs)){
  #   par(mar=mar.value)
  #   plot(africa,asp=asp.value,xlim=x.limit,ylim=y.limit,
  #        col=africa@data[,sprintf("%s.color",varname)])
  #   title(main=list(labs[varname],cex=.95,font=3))
  #   legend("bottomleft",legend=legend.data$lab,fill=legend.data$color,
  #          bty="n",cex=.7)
  # }
  # layout(matrix(c(1,1),1,1,byrow=TRUE),respect = TRUE)
  # rm(varname,asp.value,mar.value,x.limit,y.limit)
  
  db_01 <- fortify(africa,region = "ISO3")
  db_02 <- merge(db_01,africa@data,by.x="id",by.y="ISO3")
  
  graphList <- list()
  graphTitles <- gsub("\\([1-4]\\)","Transition :",labs)
  graphSubtitles <- paste0("Etape ",1:4," de la chaîne de production du dividende économique")
  if(draw.dds)
    graphSubtitles <- paste0("Etape ",1:4," de la chaîne de production du dividende démographique relatif à l'éducation")
  names(graphTitles) <- names(graphSubtitles) <- names(labs)
  
  for(varname in names(labs)){
    var.fill <- sprintf("%s.lab",varname)
    graphList[[varname]] <- ggplot(data=db_02,aes(x=long,y=lat,group=group,fill=!!as.name(var.fill))) +
      # labs(title = graphTitles[varname],subtitle=graphSubtitles[varname],caption=paste0("Source: CARE-IFA © ",format(Sys.time(),"%Y"))) +
      labs(caption=paste0("Source: CARE-IFA © ",format(Sys.time(),"%Y"))) +
      geom_polygon() + geom_path(color="black") +
      scale_fill_manual(values=c("darkgrey","darkgreen","darkred","darkorange")) +
      theme(axis.title = element_blank(),axis.line = element_blank(),
            axis.text = element_blank(),legend.title=element_blank(),
            panel.background=element_rect(fill='transparent'),
            panel.grid=element_blank(),
            plot.background=element_rect(fill='transparent'),
            legend.position=c(0.20,0.25),legend.text = element_text(size=9),
            plot.title = element_text(hjust=0, size=12,face="bold"),
            plot.subtitle = element_text(hjust=0, size=10,vjust=1.5),
            plot.caption =element_text(size=8)) 
  }
  
  return(graphList)
  
  # g1 <- ggplot(data=db_02,aes(x=long,y=lat,group=group,fill=c1.lab)) +
  #   labs(title = "Transition : Baisse de la fécondité >> Structure d'âge",
  #        subtitle="Etape 1 de la chaîne de production du dividende économique",
  #        caption=paste0("Source: CARE-IFA © ",format(Sys.time(),"%Y"))) +
  #   geom_polygon() + geom_path(color="black") +
  #   scale_fill_manual(values=c("darkgrey","darkgreen","darkred","darkorange")) +
  #   theme(axis.title = element_blank(),axis.line = element_blank(),
  #         axis.text = element_blank(),legend.title=element_blank(),
  #         legend.position=c(0,0.25),legend.text = element_text(size=9),
  #         plot.title = element_text(hjust=0, size=10),
  #         plot.subtitle = element_text(hjust=0, size=8),
  #         plot.caption =element_text(size=8)) #,face="bold"
}

# Fenêtre d'opportunité pour un pays donné
draw.windowCountry <- function(db,graph.title="Fenêtre d'opportunité",
                               x.start=1980,x.middle=2015, x.end=2035,
                               y.min=30, y.max=125,seuil=80){
  if(is.null(db)){
    cat("\n*** Données absents. Bien vouloir entrer une base valide ***\n")
    return(NULL)
  }
  
  tmp <- length(unique(as.character(db$country)))
  if(tmp!=1){
    cat("\n*** Aucun ou plus d'un pays présent dans la base de données ***\n")
    return(NULL)
  }
  
  db <- db[,c('hypo',seq(x.start,x.end,by=5))]
  x.values <- seq(from=x.start,to=x.end,by=5)
  
  dbTmp <- data.frame(year=as.numeric(rownames(t(db[,-1]))),t(db[,-1])) 
  names(dbTmp) <- c('year',as.character(t(db)[1,])) 
  rownames(dbTmp) <- 1:dim(dbTmp)[1]
  
  dbTmp <- melt(dbTmp,id='year')
  .rowid <- which(dbTmp$year>=x.start & dbTmp$year<=x.middle & dbTmp$variable=='medium')
  
  #cat('\nEnvironnement courant\n') #print(ls(envir=as.environment(-1L)))
  #Definition de certaines variables comme globales pour resoudre un
  #Pb ponctuel
  assign(".rowid",.rowid,envir=globalenv())
  old.dbTmp <- NULL
  if(exists('dbTmp',envir=globalenv())) 
    old.dbTmp <- get('dbTmp',envir = globalenv())
  assign("dbTmp",dbTmp,envir=globalenv())
  
  #Construction du graphique piece par piece
  p1 <- ggplot(dbTmp,aes(x=year,y=value,group=variable,colour=variable)) +
    # ggtitle(graph.title) +
    labs(title = graph.title, subtitle=NULL,caption=paste0("Source: CARE-IFA © ",format(Sys.time(),"%Y"))) +
    scale_y_continuous(limits=c(y.min,y.max),
                       breaks=seq(from = y.min,to = y.max,by = 10)) + 
    scale_x_continuous(limits=c(x.start,x.end),breaks=x.values) + 
    labs(colour=" ",x='Année',y='Taux de dépendance (%)')+
    scale_color_manual(limits=c('high','medium','low'),
                       labels=c('Fécondité élevée','Fécondité moyenne',
                                'Fécondité faible'),
                       values=c(high='red',medium='black',low='green'))+
    theme(axis.title.x=element_text(face="bold"),
          axis.title.y=element_text(face="bold"),
          axis.line = element_line(colour="black"),
          # panel.background=element_blank(),
          # panel.grid=element_blank(),
          # plot.background=element_blank(),
          legend.position="bottom",
          # plot.title=element_text(face="bold",size=12,vjust=1.5),
          plot.title = element_text(hjust=0,face="bold",size=10),
          plot.subtitle = element_text(hjust=0, size=8),
          plot.caption =element_text(size=8))+
    geom_line(linetype='dashed',size=.7) +
    geom_line(aes(y=c(value[.rowid],rep(NA,dim(dbTmp)[1]-length(.rowid)))),
              linetype='solid',colour="black",size=1) +
    geom_hline(yintercept=seuil,colour='darkred') 
  
  #Obtenons l'année d'entrée pour la premiere fois dans la 
  # fenêtre d'opportunité
  tmp <- dbTmp[which(dbTmp$year>=x.start & dbTmp$year<=x.end & dbTmp$variable=="low"),-2]
  rowid.1 <- which(tmp$value<seuil+1)[1] 
  result <- NULL
  if(!is.na(rowid.1) & length(rowid.1)>0){
    if(abs(tmp$value[rowid.1]-seuil)<=1) result <- tmp[rowid.1, ]
    if(rowid.1>1 & tmp$value[rowid.1]<seuil & abs(tmp$value[rowid.1]-seuil)>1){
      result <- tmp[rowid.1,]
      rowid.2 <- tail(which(abs(tmp$value[1:(rowid.1-1)]-seuil)>1),1) #which(tmp$value[1:(rowid.1-1)]>80)
      if(length(rowid.2)==0) rowid.2 <- 1 
      result <- rbind(tmp[rowid.2,],result)
    }
    assign(".yearseuil",result$year,envir=globalenv())
    if(length(.yearseuil)==1){
      p1 <- p1 + geom_segment(aes(x=.yearseuil,y=seuil-3,xend=.yearseuil,yend=seuil+3),size=.25,
                              arrow = arrow(length = unit(0.15,"cm"),ends="both",type="closed")) +
        annotate("text",x=.yearseuil,y=seuil-5,label=.yearseuil,size=3,colour='darkred',
                 fontface="bold") +
        annotate("text",x=.yearseuil,y=seuil+5,label=.yearseuil,size=3,colour='darkred',
                 fontface="bold") 
    }
    if(length(.yearseuil)>1){
      p1 <- p1 + annotate('rect',xmin=.yearseuil[1],xmax=.yearseuil[2],
                          ymin=y.min,ymax=y.max,fill="blue",alpha=.1)
    }
  }
  
  # Construction du second axe
  p2 <- ggplot(dbTmp,aes(x=year)) + geom_blank() +
    scale_y_continuous(limits=c(y.min,y.max),
                       breaks=seq(from = y.min,to = y.max,by = 10)) + 
    scale_x_continuous(limits=c(x.start,x.end),breaks=x.values) + 
    theme(panel.background=element_rect(fill='transparent'),
          panel.grid=element_blank(),
          plot.background=element_rect(fill='transparent'),
          axis.title.x=element_blank())
  
  # extract gtable
  #Evaluer d'abor p1 et p2 avant de les passer dans les fonctions
  #Evaluation paresseuse peut etre a l'origine
  g1 <- ggplot_gtable(ggplot_build(p1)) 
  g2 <- ggplot_gtable(ggplot_build(p2)) 
  
  #   #Nettoyage de la memoire
  varlist <- c('.rowid','.yearseuil')
  if(is.null(old.dbTmp)){ 
    varlist <- c(varlist,'dbTmp')
  }else{
    assign("dbTmp",old.dbTmp,envir=globalenv())
  }
  rm(list = varlist,envir = globalenv())
  
  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                       pp$l, pp$b, pp$l) 
  # axis tweaks
  ia <- which(g2$layout$name == "axis-l") 
  ga <- g2$grobs[[ia]] 
  ax <- ga$children[[2]] 
  ax$widths <- rev(ax$widths) 
  ax$grobs <- rev(ax$grobs) 
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm") 
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  return(g)
}

# Tracé, pour un pays donné, de la chaîne de production du dividende économique
draw.escalator <- function(db,graph.title="Freins au dividende démographique",
                          isf.ystart=1975,isf.yend=1990,ystart=isf.yend,yend=2010,
                          draw.dds=FALSE){
  
  if(is.null(db)){
    cat("\n*** Données absents. Bien vouloir entrer une base valide ***\n")
    return(NULL)
  }
  
  tmp <- dim(db)[1]#length(unique(as.character(db$pays)))
  if(tmp!=1){
    cat("\n*** Aucun ou plus d'un pays présent dans la base de données ***\n")
    return(NULL)
  }
  
  img.white <- png::readPNG("www/white.png")
  img.green <- png::readPNG("www/green.png")
  img.orange <- png::readPNG("www/yellow.png")
  img.red <- png::readPNG("www/red.png")
  
  db <- db[,setdiff(names(db),"code")]
  
  legend.value <- paste("ß = Changement entre",
                        paste0("      ",isf.ystart," & ",isf.yend,"  "),
                        " ",
                        "∆ = Changement entre",
                        paste0("      ",ystart," & ",yend,"  "),
                        sep="\n");
  
  labels <- c('  Baisse de la fécondité  \n',
              '     Structure par âge    \n',
              '              Emploi              \n',
              ' Productivité des occupés \n',
              '                 PIB/tête                 \n')
  
  if(draw.dds)
    labels <- c('  Baisse de la fécondité  \n',
                '     Structure par âge    \n',
                '  Dépendance économique  \n',
                '  Ressources par élève  \n',
                '               Education               \n')
  
  
  varlist <- setdiff(names(db),c('pays',paste('c',1:4,sep="")));
  labels <- paste(labels,gsub('NA%','X',paste(' ∆ = ',db[,varlist],"%",sep="")))
  labels[1] <- gsub('∆','ß',labels[1]) 
  
  font.size <- 4#2.4
  
  tmp.x0 <- seq(from = 0,to =24,by=6) 
  tmp.y0 <- seq(from = 0,to =16,by=4) 
  incr.x <- 6
  incr.y <- 2
  escalator <- data.frame(x0=tmp.x0,y0=tmp.y0,
                          x1=tmp.x0+incr.x,y1=tmp.y0+incr.y,
                          lab.x=tmp.x0+(incr.x/2),lab.y=tmp.y0+(incr.y/2),
                          lab=labels)
  
  escalator$arrow.xmin <- c(NA,head(escalator$x0,-1)+1)
  escalator$arrow.ymin <- c(NA,head(escalator$y0,-1)+2)
  escalator$arrow.xmax <- c(NA,head(escalator$x1,-1))
  escalator$arrow.ymax <- c(NA,head(escalator$y1,-1)+6)
  
  tmp <- t(db[,paste('c',1:4,sep="")])
  colnames(tmp) <- "value"
  
  escalator$coef.lab <- c(' ',gsub('NA','x',paste(paste('c',1:4,sep=""), ' = ',
                                                  db[,paste('c',1:4,sep="")],
                                                  sep="")))
  escalator$coef.lab.x <- c(NA,tail(escalator$x0,-1) - (incr.x/2))# -.25
  escalator$coef.lab.y <-  c(NA,tail(escalator$y0,-1) +(incr.y/2))#+.25
  
  x.min<-0 ; x.max<-25
  y.min<-0 ; y.max<-20
  
  x1 <- c(NA,(db[,varlist])[-5]);
  x2 <- c(NA,(db[,varlist])[-1]);
  escalator$coef.colour <- rep('img.white',5)
  rowid <- which(x1<=0 & x2<=0)
  escalator$coef.colour[rowid] <- 'img.red'
  rowid <- which(x1>0 & x2>0)
  escalator$coef.colour[rowid] <- 'img.green'
  rowid <- which((x1<=0 & x2>0)|(x1>0 & x2<=0))
  escalator$coef.colour[rowid] <- 'img.orange'
  
  
  g <- ggplot(escalator) + 
    # labs(title=NULL,subtitle=NULL,caption=NULL) +
    labs(title=graph.title,subtitle=NULL,caption=paste0("Source: CARE-IFA © ",format(Sys.time(),"%Y"))) +
    scale_y_continuous(limits=c(0,20),breaks=seq(from = 0,to = 20,by = 1)) + 
    scale_x_continuous(limits=c(0,30),breaks=seq(from = 0,to = 30,by = 1)) +
    # geom_rect(aes(xmin=x0,xmax=x1,ymin=y0,ymax=y1,col="slategrey"),#lightblue
    #           color="black",lty="solid",alpha=0.6) +
    # annotate('text',x = escalator$x0[1],y = escalator$y1[5],
    #          label=legend.value,size=font.size,hjust=0,fontface='bold') +
    geom_label(aes(x = escalator$x0[1],y = escalator$y1[5],label=legend.value),
               size=font.size,hjust=0,fontface='bold',label.size = 0) +
    geom_label(aes(x=lab.x,y=lab.y,label=lab),
               colour="black",fill="lightgrey",size=font.size,fontface="bold") +
    geom_label(aes(x=coef.lab.x,y=coef.lab.y,label=coef.lab),
              size=font.size-.5,label.size=0,hjust=0.5,angle=0,na.rm=TRUE,fontface="bold") +
    annotation_raster(get(escalator$coef.colour[2]), xmin=escalator$arrow.xmin[2], 
                      ymin = escalator$arrow.ymin[2],xmax =escalator$arrow.xmax[2],
                      ymax= escalator$arrow.ymax[2]) +
    annotation_raster(get(escalator$coef.colour[3]), xmin=escalator$arrow.xmin[3], 
                      ymin = escalator$arrow.ymin[3],xmax =escalator$arrow.xmax[3],
                      ymax= escalator$arrow.ymax[3]) +
    annotation_raster(get(escalator$coef.colour[4]), xmin=escalator$arrow.xmin[4], 
                      ymin = escalator$arrow.ymin[4],xmax=escalator$arrow.xmax[4],
                      ymax= escalator$arrow.ymax[4]) +
    annotation_raster(get(escalator$coef.colour[5]), xmin=escalator$arrow.xmin[5], 
                      ymin = escalator$arrow.ymin[5],xmax = escalator$arrow.xmax[5],
                      ymax= escalator$arrow.ymax[5]) +
    theme(axis.title=element_blank(),
          axis.ticks=element_blank(),
          axis.text=element_blank(),
          axis.line = element_blank(),
          plot.title=element_text(face="bold",size=12,vjust=1.5),
          panel.background=element_rect(fill='transparent'),
          panel.grid=element_blank(),legend.text = element_blank(),
          plot.background=element_rect(fill='transparent'))
  return(g)
}


# Source de changement des dotations publiques par élève d'un pays
draw.sourceOfChange <- function(db,graph.title="Source du changement du PIB/Tête entre 1990 et 2010 \n",
                               data.user=FALSE,draw.dds=FALSE){
  
  # require(plyr)
  
  if(is.null(db)){
    cat("\n*** Données absents. Bien vouloir entrer une base valide ***\n")
    return(NULL)
  }
  
  tmp <- length(unique(as.character(db$pays)))
  if(tmp!=1){
    cat("\n*** Aucun ou plus d'un pays présent dans la base de données ***\n")
    return(NULL)
  }
  
  dbTmp <- NULL
  rowid <- NULL
  graph.subelt <- NULL
  sousTitre <- NULL
  
  if(!draw.dds){
    
    # Conversion en pourcentage
    # db <- dde.decomp[which(dde.decomp$pays=="Angola"),]
    db$dpce.age <- round(100*db$dpce.age/db$diff.pib,1)
    db$productivite <- round(100*db$productivite/db$diff.pib,1)
    db$emploi <- round(100*db$emploi/db$diff.pib,1)
    
    dbTmp <- melt(db[,setdiff(names(db),c('pays','effet'))],
                  id.vars = c('periode','diff.pib'))
    dbTmp <- ddply(dbTmp,c("periode","diff.pib","variable"),
                   transform,lab.y=cumsum(value)-.5*value)
    
    rowid <- 1:dim(dbTmp)[1]
    if(!data.user) rowid <- which(dbTmp$period=="1990-2010")
    sousTitre <- sprintf("∆PIB/hbt = $%.1f ",dbTmp$diff.pib[rowid[1]])
    
    graph.subelt <-  ggplot(dbTmp[rowid,],
                            aes(x=variable,y=value,fill=variable)) + #coord_fixed(ratio=.035) +
      # ggtitle(paste(graph.title,subtitle,sep="\n"))+
      labs(title=graph.title,subtitle=sousTitre,caption=paste0("Source: CARE-IFA © ",format(Sys.time(),"%Y"))) +
      geom_bar(stat="identity", width=.5,colour="black") +
      geom_text(aes(label=paste(format(value,nsmall=1),"%")), vjust=-.45,size=4,
                fontface="bold",colour="black") +
      #     geom_text(aes(y=lab.y,label=paste(format(value,nsmall=1),"%")),
      #               size=4,fontface="bold",colour="black") +
      scale_fill_hue(limits=c('dpce.age','productivite','emploi'),
                                    labels=c('Structure par âge',
                                             'Productivité des occupés',
                                             'Emploi'),
                                    guide=FALSE) +
      scale_x_discrete(limits=c('dpce.age','productivite','emploi'),
                       labels=c('Structure par âge',
                                'Productivité des occupés','Emploi'))
    
  }else{
    
    # Conversion en pourcentage
    db$dpce.age <- round(100*db$dpce.age/db$diff.resource,1)
    db$revenu <- round(100*db$revenu/db$diff.resource,1)
    db$social <- round(100*db$social/db$diff.resource,1)
    
    total <- db$dpce.age + db$revenu + db$social
    rowid <- which(total!=100)
    if(length(rowid)){
      coef <- 100/total[rowid] #1/total[rowid]
      db$dpce.age[rowid] <-  round(db$dpce.age[rowid] * coef,1)
      db$revenu[rowid] <-  round(db$revenu[rowid] * coef,1)
      db$social[rowid] <-  round(db$social[rowid] * coef,1)
    }
    
    dbTmp <- melt(db[,setdiff(names(db),c('pays','effet'))],
                  id.vars = c('periode','diff.resource'))
    dbTmp <- ddply(dbTmp,c("periode","diff.resource"),
                   transform,lab.y=cumsum(value)-.5*value)
    rowid <- 1:dim(dbTmp)[1]
    if(!data.user) rowid <- which(dbTmp$periode=="1995-2010");
    sousTitre <- sprintf("∆r = $%.1f ",dbTmp$diff.resource[rowid[1]])
    
    graph.subelt <- ggplot(dbTmp[rowid,],
                           aes(x=variable,y=value,fill=variable)) + #coord_fixed(ratio=.035) +
      # ggtitle(paste(graph.title,subtitle,sep="\n"))+
      labs(title=graph.title,subtitle=sousTitre,caption=paste0("Source: CARE-IFA © ",format(Sys.time(),"%Y"))) +
      geom_bar(stat="identity", width=.5,colour="black") +
      geom_text(aes(label=paste(format(value,nsmall=1),"%")), vjust=-.45,size=4,
                fontface="bold",colour="black") +
      #     geom_text(aes(y=lab.y,label=paste(format(value,nsmall=1),"%")),
      #               size=4,fontface="bold",colour="black") +
      scale_fill_hue(limits=c('dpce.age','revenu','social'),
                                   labels=c("Dépendance de l'âge",
                                            'Revenu',
                                            'Engagement politique'),
                                   guide=FALSE) +
      scale_x_discrete(limits=c('dpce.age','revenu','social'),
                       labels=c("Dépendance de l'âge",
                                'Revenu','Engagement politique'))
    
  }
  
  g <- graph.subelt +
    geom_hline(yintercept=0,colour="black") +
    theme(axis.title=element_blank(),
          axis.ticks=element_blank(),
          axis.text.y=element_blank(),
          axis.text.x=element_text(face="bold",size=12,family="Times",colour="black"),
          axis.line = element_blank(),
          #plot.title=element_text(face="bold",size=12,vjust=1.5),
          panel.background=element_rect(fill='transparent'),
          panel.grid=element_blank(),
          plot.background=element_rect(fill='transparent'),
          #plot.title = element_text(hjust=0,face="bold",size=10),
          plot.subtitle = element_text(face="bold",size=12,hjust=0.5,vjust=1.5),
          plot.caption =element_text(size=8)) 
  return(g)
}

# Vérification de la saisie des données utilisateurs
dds.checkDataUser <- function(dds.isfYear01,dds.isfYear02,
                              dds.isf01,dds.isf02,dds.year01,dds.year02,
                              popTot01,popTot02,
                              txdpce.age01,txdpce.age02,txdpce.real01,txdpce.real02,
                              dpce.youth01,dpce.youth02,pop1564_01,pop1564_02,
                              gni01,gni02,k01,k02,tbs01,tbs02){
  
  errmsg <- NULL
  # cat("\n[dds.checkDataUser]\n")
  # cat("dds.isfYear01=",dds.isfYear01,"\n")
  # cat("dds.isfYear02=",dds.isfYear02,"\n")
  # cat("dds.isf01=",dds.isf01,"\n")
  # cat("dds.isf02=",dds.isf02,"\n")
  # cat("dds.year01=",dds.year01,"\n")
  # cat("dds.year02=",dds.year02,"\n")
  # cat("popTot01=",popTot01,"\n")
  # cat("popTot02=",popTot02,"\n")
  # cat("txdpce.age01=",txdpce.age01,"\n")
  # cat("txdpce.age02=",txdpce.age02,"\n")
  # cat("txdpce.real01=",txdpce.real01,"\n")
  # cat("txdpce.real02=",txdpce.real02,"\n")
  # cat("dpce.youth01=",dpce.youth01,"\n")
  # cat("dpce.youth02=",dpce.youth02,"\n")
  # cat("pop1564_01=",pop1564_01,"\n")
  # cat("pop1564_02=",pop1564_02,"\n")
  # cat("gni01=",gni01,"\n")
  # cat("gni02=",gni02,"\n")
  # cat("k01=",k01,"\n")
  # cat("k02=",k02,"\n")
  # cat("tbs01=",tbs01,"\n")
  # cat("tbs02=",tbs02,"\n")
  
  if(is.na(dds.isfYear01) | is.na(dds.isfYear02) | is.na(dds.isf01) | is.na(dds.isf02) |
     is.na(dds.year01) | is.na(dds.year02) | is.na(popTot01) | is.na(popTot02) | 
     is.na(txdpce.age01) | is.na(txdpce.age02) | is.na(txdpce.real01) | is.na(txdpce.real02) | 
     is.na(dpce.youth01) | is.na(dpce.youth02) | is.na(pop1564_01) | is.na(pop1564_02) | 
     is.na(gni01) | is.na(gni02) | is.na(k01)  | is.na(k02) | is.na(tbs01)  | is.na(tbs02)){
    return("Bien vouloir,  renseignez tous les champs sous « Paramètres avancés ».")
  }
  
  if(dds.isfYear01==dds.isfYear02) 
    errmsg <- c(errmsg,
                sprintf("Les années de début et de fin d'observation du changement de l'ISF doivent être différente d'au moins une année",
                        dds.year01,dds.year02))
  
  if((dds.isfYear01!=dds.isfYear02) & dds.year01==dds.year02) 
    errmsg <- c(errmsg,
                sprintf("Les années de début et de fin d'observation du changement des indicateurs doivent être différentes d'au moins une année",
                        dds.year01,dds.year02))
  
  if(dds.year01<dds.isfYear02 | dds.year02<=dds.isfYear02)
    errmsg <- c(errmsg,
                "La période d'observation des indicateurs doit être postérieure à celle de l'ISF")
  
  return(errmsg)
  
}

# Calcul des indices à utiliser pour le dividende scolaire
compute.dds.indice <- function(popTot01,popTot02,
                               txdpce.age01,txdpce.age02,txdpce.real01,txdpce.real02,
                               dpce.youth01,dpce.youth02,pop1564_01,pop1564_02,
                               gni01,gni02,k01,k02){
  
  # dds.isfYear01,dds.isfYear02,tbs01,tbs02
  # dds.isf01,dds.isf02,dds.year01,dds.year02,
  
  g <- 100*c(start=as.numeric(gni01)/(as.numeric(popTot01)*as.numeric(pop1564_01)),
             end=as.numeric(gni02)/(as.numeric(popTot02)*as.numeric(pop1564_02)))
  
  k <- c(start=as.numeric(k01),end=as.numeric(k02))/100
  
  p <- c(start=as.numeric(dpce.youth01),end=as.numeric(dpce.youth02))/100
  
  effet.revenu <- g*k/p
  
  result <- list(resource=effet.revenu,g=g,k=k,p=p)
  return(result)
}

# Calcul des indices mobilisés dans le tracé de la chaîne de production du dividende scolaire
compute.dds.Escalator <- function(dds.isfYear01,dds.isfYear02,dds.isf01,dds.isf02,
                                  dds.year01,dds.year02,txdpce.age01,txdpce.age02,
                                  txdpce.real01,txdpce.real02,tbs01,tbs02,db){
 
  isf.interval <- as.numeric(dds.isfYear02)-as.numeric(dds.isfYear01)+1
  idx.interval <- as.numeric(dds.year02)-as.numeric(dds.year01)+1
  
  txfec <- 1-((as.numeric(dds.isf02)/as.numeric(dds.isf01))^(1/isf.interval))
  dpce.demo <- 1 - ((as.numeric(txdpce.age02)/as.numeric(txdpce.age01))^(1/idx.interval))
  dpce.reel <- 1 - ((as.numeric(txdpce.real02)/as.numeric(txdpce.real01))^(1/idx.interval))
  resource <- ((db$resource[2]/db$resource[1])^(1/idx.interval)) - 1
  tbs <- ((as.numeric(tbs02)/as.numeric(tbs01))^(1/idx.interval)) - 1
  
  c1 <- round(dpce.demo/txfec,1)
  c2 <- round(dpce.reel/dpce.demo,1)
  c3 <- round(resource/dpce.reel,1)
  c4 <- round(tbs/resource,1)
  
  txfec <- round(100*txfec,1)
  dpce.demo <- round(100*dpce.demo,1)
  dpce.reel <- round(100*dpce.reel,1)
  resource <- round(100*resource,1)
  tbs <- round(100*tbs,1)
    
  result <- data.frame(pays=NA,txfec,c1,dpce.demo,c2,dpce.reel,c3,resource,c4,tbs)
  return(result)
}

# Calcul des indices mobilisés dans le tracé des sources de changement
compute.dds.sourceOfChange <- function(db,dds.year01,dds.year02){
  
  # input <- dds.indice <- read.csv2("data/DDS_indicateur.csv")
  # input <- input[input$country=="Algeria",]
  # 
  # db <- compute.dds.indice(input$popTot_1995,input$popTot_2010,
  #                          input$txdpce_age_1995,input$txdpce_age_2010,
  #                          input$txdpce_real_1995,input$txdpce_real_2010,
  #                          input$youth.dpce_1995,input$youth.dpce_2010,
  #                          input$pop1564_1995,input$pop1564_2010,
  #                          input$gni_1995,input$gni_2010,input$k_1995,input$k_2010)
  # 
  # dds.year01 <- 1995
  # dds.year02 <- 2010
  
  diff.resource <- db$resource[2] - db$resource[1]
  
  dpce.age <- (db$p[2]-db$p[1])*mean((db$k[1]*db$g[1])/(db$p[1]^2),(db$k[2]*db$g[2])/(db$p[2]^2))
  revenu <- (db$g[2]-db$g[1])*mean(db$k[1]/db$p[1],db$k[2]/db$p[2])
  social <- (db$k[2]-db$k[1])*mean(db$g[1]/db$p[1],db$g[2]/db$p[2])
  
  # dpce.age <- round(100*dpce.age/diff.resource,1)
  # revenu <- round(100*revenu/diff.resource,1)
  # social <- round(100*social/diff.resource,1)
  
  result <- data.frame(pays=NA,periode=sprintf("%d-%d",dds.year01,dds.year02),dpce.age,revenu,social,
                       diff.resource,effet="absolu")
  return(result)
}


