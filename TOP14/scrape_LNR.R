library(chromote)

source("scrape_annex.R")

##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##

findMatchLNR <- function(union){
  if(!is.element(union, c("prod2","top14"))) stop("Union not in the list.")
  url <- paste0("https://",union,".lnr.fr/calendrier-et-resultats/")
  b <- ChromoteSession$new()
  navigate_safe(b,url,"input--select",retries = 2)
  tmp <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.input--select')).map(el => el.innerText))")$result$value)
  return(list("season"=strsplit(tolower(tmp[1]),"\n")[[1]][-1],"round"=strsplit(tolower(tmp[2]),"\n")[[1]][-1]))
}

# union <- c("top14") #c("prod2")  c("top14")
scrapeLNR<- function(union,addMat=NULL,whichSeasons=NULL,whichRounds=NULL){
  if(!is.element(union, c("prod2","top14"))) stop("Union not in the list.")
  if(!is.null(addMat)) if(any(colnames(addMat)!=c("minute","type","home","away","where","score","round","player","date"))) stop("Cannot add to the given matrix. Wrong colnames.")
  tmp <- findMatchLNR(union)
  listSeason <- tmp$season
  listRound <- tmp$round
  if(is.null(whichRounds) || any(!is.element(whichRounds,listRound)) ){ stop("Wrong seasons. Check findMatchLNR().")
  } else { listSeason <- whichSeasons }
  if(is.null(whichSeasons) || any(!is.element(whichSeasons,listSeason)) ){ stop("Wrong rounds. Check findMatchLNR().")
  } else { listRound <- whichRounds }
  
  id <- array(dim=c(10,length(listRound),length(listSeason)))  # 1: number of matches, 2: number of rounds, 3: number of seasons 
  dimnames(id)[[3]] <- listSeason
  dimnames(id)[[2]] <- listRound
  for(s in listSeason){
    url <- paste0("https://",union,".lnr.fr/calendrier-et-resultats/",s,"/")
    for (j in listRound) {
      download.file(paste0(url,j,collapse=""),"lnr.html")
      lines <- readLines("lnr.html")
      pat <- paste0("https://",union,".lnr.fr/feuille-de-match/",s,"/",j,"/")
      link <- findlinerow(lines,pat)
      if(is.null(link)) next
      else{
        link <- link[rep(c(FALSE,TRUE),nrow(link)/2),]
        for(i in 1:nrow(link)) id[i,j,s] <- strsplit(substr(lines[link[i,1]],link[i,2],link[i,2]+10000),"'")[[1]]
      }
    }
  }
  # id <- id[!apply(id,1,function(vec) all(is.na(vec))),!apply(id,2,function(vec) all(is.na(vec))),!apply(id,3,function(vec) all(is.na(vec)))]
  
  data <- data.frame("minute"=NA,"type"=NA,"home"=NA,"away"=NA,"where"=NA,"score"=NA,"round"=NA,"player"=NA,"date"=NA)
  for(s in listSeason){
    for(jou in listRound){
      for(idm in id[,jou,s][!is.na(id[,jou,s])]){
        tmp <- strsplit(idm,"-")[[1]]
        taway <- strsplit(idm,"-")[[1]][length(tmp)]
        thome <- strsplit(idm,"-")[[1]][2]
        round <- substr(jou,2,100)
        print(paste(s,jou,thome,taway))
        url <- paste0("https://",union,".lnr.fr/feuille-de-match/",s,"/",jou,"/",idm)
        
        b <- ChromoteSession$new()
        navigate_safe(b,url,"title",retries = 2)
        tmp <- strsplit(jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.title.title--large.title--textured.title--centered')).map(el => el.innerText))")$result$value)," - ")     
        score <- list("home"=tmp[[1]][1],"away"=tmp[[1]][2])
        navigate_safe(b,url,"match-header__season-day",retries = 2)
        tmp <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.match-header__season-day')).map(el => el.innerText))")$result$value)
        date <- strsplit(tmp," ")[[1]][6]
        
        flag <- navigate_safe(b,url,"vertical-timeline-item__time-label",retries = 2)
        if(!flag) next
        
        for(where in c("home","away")){
          action <- jsonlite::fromJSON(b$Runtime$evaluate(paste0("
              JSON.stringify(
                Array.from(document.querySelectorAll('.vertical-timeline-item--",where," .vertical-timeline-item__wrapper')).map(el => el.innerText)
              )"))$result$value)
          actime <- jsonlite::fromJSON(b$Runtime$evaluate(paste0("
              JSON.stringify(
                Array.from(document.querySelectorAll('.vertical-timeline-item--",where," .vertical-timeline-item__time-label')).map(el => el.innerText)
              )"))$result$value)
          for(i in 1:length(action)){
            tmp <- strsplit(action[i],"\n")[[1]]
            if(length(tmp)==3) data <- rbind(data,c(actime[i],tmp[2],thome,taway,where,score[[where]],round,tolower(tmp[3]),date))
            else if(length(tmp)==4){
              tmpbis <- strsplit(tmp[4]," ")[[1]]
              data <- rbind(data,c(actime[i],"conversion",thome,taway,where,score[[where]],round,tolower(paste(tmpbis[3:length(tmp)],collapse=" ")),date))
            }
          } 
        }
        b$close()
      }
    }
  }
  data <- data[-1,]
  if(union=="top14"){
    tradteamgene <- c("bayonne","bordeaux-bègles","castres","clermont","la rochelle","lyon","montauban","montpellier","pau","perpignan","racing 92","stade français","stade toulousain","toulon")
    tradteamlnr <- c("bayonne","bordeaux","castres","clermont","la","lyon","montauban","montpellier","pau","perpignan","racing","paris","toulouse","toulon")
  }
  if(union=="prod2"){
    tradteamgene <- c("agen","angoulême","aurillac","béziers","biarritz","brive","carcassonne","colomiers","dax","grenoble","mont de marsan","nevers","oyonnax","provence rugby","valence romans","vannes")
    tradteamlnr <- c("agen","angouleme","aurillac","beziers","biarritz","brive","carcassonne","colomiers","dax","grenoble","mont","nevers","oyonnax","provence","valence","vannes")
  }
  for(j in 1:length(tradteamlnr)) for(where in c("home","away")) data[where][data[where]==tradteamlnr[j]] <- tradteamgene[j]
  
  tradtypegene <- c("try","penalty try","penalty","drop","conversion","yellow","red","orange")
  tradtypelnr <- c("Essai","Essai de pénalité","Pénalité","Drop","conversion","Carton Jaune","Carton Rouge","Carton Orange")
  for(j1 in 1:length(tradtypegene)) data$type[data$type==tradtypelnr[j1]] <- tradtypegene[j1]
  
  if(is.null(addMat)){ return(data)   
  } else {
    addMat <- rbind(addMat,data)
    return(addMat[!duplicated(addMat),])
  }
}

# union <- c("top14") #c("prod2")  c("top14")
scrapeLNRstats<- function(union,addMat=NULL,whichSeasons=NULL,whichRounds=NULL){
  if(!is.element(union, c("prod2","top14"))) stop("Union not in the list.")
  if(!is.null(addMat)) if(any(colnames(addMat)!=c("type","home","away","statsHome","statsAway","round","date"))) stop("Cannot add to the given matrix. Wrong colnames.")
  tmp <- findMatchLNR(union)
  listSeason <- tmp$season
  listRound <- tmp$round
  if(is.null(whichRounds) || any(!is.element(whichRounds,listRound)) ){ stop("Wrong seasons. Check findMatchLNR().")
  } else { listSeason <- whichSeasons }
  if(is.null(whichSeasons) || any(!is.element(whichSeasons,listSeason)) ){ stop("Wrong rounds. Check findMatchLNR().")
  } else { listRound <- whichRounds }
  
  id <- array(dim=c(10,length(listRound),length(listSeason)))  # 1: number of matches, 2: number of rounds, 3: number of seasons 
  dimnames(id)[[3]] <- listSeason
  dimnames(id)[[2]] <- listRound
  for(s in listSeason){
    url <- paste0("https://",union,".lnr.fr/calendrier-et-resultats/",s,"/")
    for (j in listRound) {
      download.file(paste0(url,j,collapse=""),"lnr.html")
      lines <- readLines("lnr.html")
      pat <- paste0("https://",union,".lnr.fr/feuille-de-match/",s,"/",j,"/")
      link <- findlinerow(lines,pat)
      if(is.null(link)) next
      else{
        link <- link[rep(c(FALSE,TRUE),nrow(link)/2),]
        for(i in 1:nrow(link)) id[i,j,s] <- strsplit(substr(lines[link[i,1]],link[i,2],link[i,2]+10000),"'")[[1]]
      }
    }
  }
  # id <- id[!apply(id,1,function(vec) all(is.na(vec))),!apply(id,2,function(vec) all(is.na(vec))),!apply(id,3,function(vec) all(is.na(vec)))]
  
  stats <- data.frame("type"=NA,"home"=NA,"away"=NA,"statsHome"=NA,"statsAway"=NA,"round"=NA,"date"=NA)
  for(s in listSeason){
    for(jou in listRound){
      for(idm in id[,jou,s][!is.na(id[,jou,s])]){
        tmp <- strsplit(idm,"-")[[1]]
        taway <- strsplit(idm,"-")[[1]][length(tmp)]
        thome <- strsplit(idm,"-")[[1]][2]
        round <- substr(jou,2,100)
        print(paste(s,jou,thome,taway))
        
        url <- paste0("https://",union,".lnr.fr/feuille-de-match/",s,"/",jou,"/",idm,"/statistiques-du-match")
        b <- ChromoteSession$new()
        navigate_safe(b,url,"match-header__season-day",retries = 2)
        tmp <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.match-header__season-day')).map(el => el.innerText))")$result$value)
        date <- strsplit(tmp," ")[[1]][6]
        
        flag <- navigate_safe(b,url,"stats-bar",retries = 2)
        if(!flag) next
        
        for(where in c("home","away")){
          try({
            action <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.stats-bar')).map(el => el.innerText))")$result$value)
            for(i in 1:length(action)){
              tmp <- strsplit(action[i],"\n")[[1]]
              stats <- rbind(stats,c(tmp[1],thome,taway,tmp[2],tmp[3],round,date))
            } 
          },silent=TRUE)
        }
        b$close()
      }
    }
  }
  stats <- stats[-1,]
  if(union=="top14"){
    tradteamgene <- c("bayonne","bordeaux-bègles","castres","clermont","la rochelle","lyon","montauban","montpellier","pau","perpignan","racing 92","stade français","stade toulousain","toulon")
    tradteamlnr <- c("bayonne","bordeaux","castres","clermont","la","lyon","montauban","montpellier","pau","perpignan","racing","paris","toulouse","toulon")
  }
  if(union=="prod2"){
    tradteamgene <- c("agen","angoulême","aurillac","béziers","biarritz","brive","carcassonne","colomiers","dax","grenoble","mont de marsan","nevers","oyonnax","provence rugby","valence romans","vannes")
    tradteamlnr <- c("agen","angouleme","aurillac","beziers","biarritz","brive","carcassonne","colomiers","dax","grenoble","mont","nevers","oyonnax","provence","valence","vannes")
  }
  for(j in 1:length(tradteamlnr)) for(where in c("home","away")) stats[where][stats[where]==tradteamlnr[j]] <- tradteamgene[j]
  
  if(is.null(addMat)){ return(stats)   
  } else {
    addMat <- rbind(addMat,stats)
    return(addMat[!duplicated(addMat),])
  }
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#  
