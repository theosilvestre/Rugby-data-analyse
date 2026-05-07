library(chromote)

source("scrape_annex.R")

##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##

#Find for every seasons, every rounds. Number rounds, final, etc. can change depending on the season.
findMatchLNR <- function(union,verbose=TRUE){
  if(!is.element(union, c("prod2","top14"))) stop("Union not in the list.")
  url <- paste0("https://",union,".lnr.fr/calendrier-et-resultats/")
  b <- ChromoteSession$new()
  navigate_safe(b,url,"input--select",retries = 2)
  Sys.sleep(0.5)
  tmp <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.input--select')).map(el => el.innerText))")$result$value)
  season <- strsplit(tolower(tmp[1]),"\n")[[1]][-1]
  res <- list()
  for(s in season){
    if(verbose) print(s)
    js <- sprintf("
  new Promise(resolve => {
    const select = document.querySelector('#Saison');
    if(!select){ resolve('no select'); return; }
    const getText = () => select.options[select.selectedIndex]?.text.trim();
    const targetText = '%s';
    const opts = Array.from(select.options);
    const idx = opts.findIndex(o => o.text.trim() === targetText);
    if(idx >= 0){
      select.selectedIndex = idx;
      select.dispatchEvent(new Event('input',{bubbles:true}));
      select.dispatchEvent(new Event('change',{bubbles:true}));
    }

    const check = () => {
      if(getText() === targetText){
        resolve('updated');
      } else {
        setTimeout(check, 100);
      }
    };

    check();
  })
", s)
    js <- gsub("[\n\r]", " ", js)
    b$Runtime$evaluate(expression = js, awaitPromise = TRUE)
    res[[s]] <- strsplit(tolower(jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.input--select')).map(el => el.innerText))")$result$value)[2]),"\n")[[1]][-1]
  }
  b$close()
  return(res)
}

# union <- c("top14") #c("prod2")  c("top14")
scrapeLNR <- function(union,addMat=NULL,whichMatches=NULL,fetchdata=TRUE,fetchstats=TRUE,verbose=TRUE){
  ### Checking arguments ### 
  if(!fetchdata & !fetchstats) stop("Either fetchstats or fetchdata must be TRUE.")
  if(!is.element(union, c("prod2","top14"))) stop("Union not in the list.")
  if(!is.null(addMat)) if(!is.list(whichMatches) | any(colnames(addMat$data)!=c("minute","type","home","away","where","score","round","player","date")) | any(colnames(addMat$stats)!=c("type","home","away","statsHome","statsAway","round","date"))) stop("Cannot add to the given matrices. Wrong colnames or not a list.")
  ### Retrieving possible seasons and rounds ### 
  tmp <- findMatchLNR(union,FALSE)
  if(length(tmp)==0) stop("Probably no internet connection.")
  listSeason <- names(tmp)
  listRound <- tmp
  ### Check if the wanting seasons and rounds are effectively existing ### 
  if(any(!is.element(names(whichMatches),listSeason)) ){ 
    stop("Wrong seasons. Check findMatchLNR().")
  } else if(!is.null(whichMatches)) { 
    listSeason <- names(whichMatches) 
    listRound <- listRound[listSeason]
    for(s in listSeason){
      if(any(!is.element(whichMatches[[s]],listRound[[s]])) ){ stop("Wrong rounds. Check findMatchLNR().")
      } else if(!is.null(whichMatches)){ listRound[[s]] <- whichMatches[[s]] }
    }
  }
  ### Scraping data and stats ### 
  data <- data.frame("minute"=NA,"type"=NA,"home"=NA,"away"=NA,"where"=NA,"score"=NA,"round"=NA,"player"=NA,"date"=NA)
  stats <- data.frame("type"=NA,"home"=NA,"away"=NA,"statsHome"=NA,"statsAway"=NA,"round"=NA,"date"=NA)
  for(s in listSeason){
    for(j in listRound[[s]]){
      b <- ChromoteSession$new()
      flag <- navigate_safe(b,paste0("https://",union,".lnr.fr/calendrier-et-resultats/",s,"/",j,collapse=""),"match-line__score",retries = 2,timeout = 1000)
      if(!flag) next
      id <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.match-line__score')).map(el => el.href))")$result$value)
      for(idm in id){
        ### Finding home team, visitor team, round, date ###
        flag <- navigate_safe(b,idm,"title",retries = 2)
        if(!flag) next
        tmp <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.title.title--large.title--textured.title--centered')).map(el => el.innerText))")$result$value)
        if(length(tmp) > 0) score <- list("home"=strsplit(tmp," - ")[[1]][1],"away"=strsplit(tmp," - ")[[1]][2]) 
        tmp <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.match-header__season-day')).map(el => el.innerText))")$result$value)
        if(length(tmp) > 0){
          round <- tolower(strsplit(tmp," ")[[1]][4])
          date <- strsplit(tmp," ")[[1]][6]
        }
        tmp <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.match-header-club__title')).map(el => el.innerText))")$result$value)
        thome <- tolower(tmp[1])
        taway <- tolower(tmp[2])
        if(verbose) print(paste(s,j,thome,taway))
        ### Fetch data ###
        if(fetchdata){
          for(where in c("home","away")){
            action <- jsonlite::fromJSON(b$Runtime$evaluate(paste0("
              JSON.stringify(
                Array.from(document.querySelectorAll('.vertical-timeline-item--",where," .vertical-timeline-item__wrapper')).map(el => el.innerText)
              )"))$result$value)
            actime <- jsonlite::fromJSON(b$Runtime$evaluate(paste0("
              JSON.stringify(
                Array.from(document.querySelectorAll('.vertical-timeline-item--",where," .vertical-timeline-item__time-label')).map(el => el.innerText)
              )"))$result$value)
            if(length(action)>0){
              for(i in 1:length(action)){
                tmp <- strsplit(action[i],"\n")[[1]]
                if(length(tmp)==3) data <- rbind(data,c(actime[i],tmp[2],thome,taway,where,score[[where]],round,tolower(tmp[3]),date))
                else if(length(tmp)==4){
                  tmpbis <- strsplit(tmp[4]," ")[[1]]
                  data <- rbind(data,c(actime[i],"conversion",thome,taway,where,score[[where]],round,tolower(paste(tmpbis[3:length(tmp)],collapse=" ")),date))
                }
              }
            }
          }
        }
        ### Fetch stats ###
        if(fetchstats){
          flag <- navigate_safe(b,paste0(idm,"/statistiques-du-match"),"stats-bar",retries = 2,timeout = 1000)
          if(!flag) next
          for(where in c("home","away")){
            action <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.stats-bar')).map(el => el.innerText))")$result$value)
            if(length(action)>0){
              for(i in 1:length(action)){
                tmp <- strsplit(action[i],"\n")[[1]]
                stats <- rbind(stats,c(tmp[1],thome,taway,tmp[2],tmp[3],round,date))
              } 
            }
          }
        }
        
      }
      b$close()
    }
  }
  data <- data[-1,]
  stats <- stats[-1,]
  ### Translating from French to English ###
  tradtypegene <- c("try","penalty try","penalty","drop","conversion","yellow","red","orange")
  tradtypelnr <- c("Essai","Essai de pénalité","Pénalité","Drop","conversion","Carton Jaune","Carton Rouge","Carton Orange")
  for(j1 in 1:length(tradtypegene)) data$type[data$type==tradtypelnr[j1]] <- tradtypegene[j1]
  ### Concatenating input and output ###
  if(is.null(addMat)){ return(list("data"=data,"stats"=stats))   
  } else {
    addMat$data <- rbind(addMat$data,data)
    addMat$data[!duplicated(addMat$data),]
    addMat$stats <- rbind(addMat$stats,stats)
    addMat$stats[!duplicated(addMat$stats),]
    return(addMat)
  }
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#  
