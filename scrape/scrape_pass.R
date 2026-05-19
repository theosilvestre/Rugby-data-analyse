source("scrape_annex.R")

##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##

findMatchPass <- function(union,verbose=TRUE){
  if(!is.element(union, c("pro-d2","top-14","european-champions-cup","premiership","challenge-cup","super-rugby"))) stop("Union not in the list.")
  url <- paste0("https://www.rugbypass.com/",union,"/fixtures-results/")
  b <- ChromoteSession$new()
  navigate_safe(b,url,"dropdowns",retries = 3)
  b$Runtime$evaluate("
    const el = document.querySelector('.select-box.season .label');
    if (el) el.dispatchEvent(new MouseEvent('click', { bubbles: true }));
  ")
  season <- strsplit(jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.select-box.season ul')).map(el => el.innerText))")$result$value),"\n")[[1]]
  b$close()
  resRound <- list()
  resLink <- list()
  for(s in season){
    b <- ChromoteSession$new()
    navigate_safe(b,url,"dropdowns",retries = 3)
    if(verbose) print(s)
    Sys.sleep(0.5)  #lower waiting times may not work
    b$Runtime$evaluate("
      const el = document.querySelector('.select-box.season .label');
      if (el) el.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    ")
    Sys.sleep(0.5)  #lower waiting times may not work
    b$Runtime$evaluate(paste0("
    (() => {
      const ol = [...document.querySelectorAll('.select-box.season ul li')].find(e => e.textContent.trim() === '", s, "');
      if(!ol) return 'NOT_FOUND';
      ol.click();
      return ol.textContent;
    })()"))
    Sys.sleep(0.5)  #lower waiting times may not work
    res <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(
      [...document.querySelectorAll('.comp-game')].map(el => ({
        text: [...el.querySelectorAll('.round')].map(a => a.innerText),
        links: [...el.querySelectorAll('.link-box')].map(a => a.href)
        }))
    )")$result$value)
    round <- unlist(res$text)
    id <- sapply(res$links,function(st) ifelse(length(st)==0,NA,st))
    resRound[[s]] <- unique(round)
    tmp <- matrix(NA,nrow=max(table(unlist(res$text))),ncol=length(resRound[[s]]))
    for(i in 1:length(resRound[[s]])) tmp[1:sum(round==resRound[[s]][i]),i] <- id[round==resRound[[s]][i]]
    colnames(tmp) <- resRound[[s]]
    resLink[[s]] <- tmp
    resRound[[s]] <- resRound[[s]][!apply(tmp,2,function(vec) all(is.na(vec)))]
    b$close()
  }
  
  return(list(resRound,resLink))
}

# union <- "premiership" #"pro-d2" "top-14" "european-champions-cup" "premiership" "challenge-cup" "super-rugby"
scrapepass <- function(union,addMat=NULL,whichMatches=NULL,fetchdata=TRUE,fetchstats=TRUE,verbose=TRUE){
  ### Checking arguments ### 
  if(!fetchdata & !fetchstats) stop("Either fetchstats or fetchdata must be TRUE.")
  if(!is.element(union, c("pro-d2","top-14","european-champions-cup","premiership","challenge-cup","super-rugby"))) stop("Union not in the list.")
  if(!is.null(addMat)) if(!is.list(whichMatches) | any(colnames(addMat$data)!=c("minute","type","home","away","where","score","round","player","date")) | any(colnames(addMat$stats)!=c("type","home","away","statsHome","statsAway","round","date"))) stop("Cannot add to the given matrices. Wrong colnames or not a list.")
  ### Retrieving possible seasons and rounds ### 
  tmp <- findMatchPass(union,verbose=FALSE)
  if(length(tmp)==0) stop("Probably no internet connection.")
  listSeason <- names(tmp[[2]])
  listRound <- tmp[[2]]
  ### Check if the wanting seasons and rounds are effectively existing ### 
  if(any(!is.element(names(whichMatches),listSeason)) ){ 
    stop("Wrong seasons. Check findMatchLNR().")
  } else if(!is.null(whichMatches)) { 
    listSeason <- names(whichMatches) 
    listRound <- listRound[listSeason]
    for(s in listSeason){
      if(any(!is.element(whichMatches[[s]],tmp[[1]][[s]])) ){ stop("Wrong rounds. Check findMatchLNR().")
      } else if(!is.null(whichMatches)){ 
        if(length(whichMatches[[s]]) == 1){
          listRound[[s]] <- matrix(listRound[[s]][,whichMatches[[s]]],ncol=1) 
          colnames(listRound[[s]]) <- whichMatches[[s]]
        }
        else listRound[[s]] <-listRound[[s]][,whichMatches[[s]]]
      }
    }
  }
  ### Scraping data and stats ### 
  data <- data.frame("minute"=NA,"type"=NA,"home"=NA,"away"=NA,"where"=NA,"round"=NA,"player"=NA,"date"=NA)
  stats <- data.frame("type"=NA,"home"=NA,"away"=NA,"statsHome"=NA,"statsAway"=NA,"round"=NA,"date"=NA)
  for(s in listSeason){
    for(j in colnames(listRound[[s]])){
      for(idm in listRound[[s]][!is.na(listRound[[s]][,j]),j]){
        b <- ChromoteSession$new()
        flag <- navigate_safe(b,idm,"team-name",retries = 2)
        if(!flag) next
        tmp <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.team .team-name')).map(el => el.innerText))")$result$value)
        thome <- tmp[1]
        taway <- tmp[2]
        score <- list()
        score$home <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.home-score')).map(el => el.innerText))")$result$value)
        score$away <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.away-score')).map(el => el.innerText))")$result$value)
        tmp <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.match-details .title')).map(el => el.innerText))")$result$value)
        round <- tmp[2]
        date <- strsplit(tmp[3],",")[[1]][1]
        print(paste(round,thome,taway))
        ### Fetch data ###
        if(fetchdata){
          where <- ifelse(jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.key-event .side.home')).map(el => el.innerText))")$result$value)=="","away","home")
          action <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.key-event')).map(el => el.innerText))")$result$value)
          action <- sapply(action[-c(1,length(action))], function(val) strsplit(val,"\n")[[1]][c(1:3)])
          for(i in 2:3){
            stoppage40 <- sapply(action[i,which(action[1,]=="Half Time"):ncol(action)], function(val) as.numeric(strsplit(val,"'")[[1]][1]))
            if(length(stoppage40[which(stoppage40>40)]) > 0) action[i,which(action[1,]=="Half Time"):ncol(action)][which(stoppage40>40)] <- paste0("40'+",stoppage40[which(stoppage40>40)]-40)
            stoppage80 <- sapply(action[i,], function(val) as.numeric(strsplit(val,"'")[[1]][1]))
            if(length(stoppage80[which(stoppage80>80)]) > 0) action[i,][which(stoppage80>80)] <- paste0("80'+",stoppage80[which(stoppage80>80)]-80)
          }
          action <- action[,-which(action[1,]=="Half Time")]
          type <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.key-event .icon-image')).map(el => [...el.classList].find(c => c !== 'icon-image')))")$result$value)
          for(i in 1:length(where)){
            if(where[i]=="home"){
              if(action[,i][1]=="Penalty Try"){
                data <- rbind(data,c(action[,i][3],"penalty try",thome,taway,where[i],round,date))
              } else {
                data <- rbind(data,c(action[,i][3],type[i],thome,taway,where[i],round,tolower(action[,i][1]),date))
                if(((i>1 && type[i-1]!="con") || i==1) && type[i]=="try") data <- rbind(data,c(action[,i][3],"missed conversion",thome,taway,where[i],round,"NA",date))
              }
            }
            if(where[i]=="away"){
              if(action[,i][1]=="Penalty Try"){
                data <- rbind(data,c(action[,i][2],"penalty try",thome,taway,where[i],round,date))
              } else {
                data <- rbind(data,c(action[,i][2],type[i],thome,taway,where[i],round,tolower(action[,i][1]),date))
                if(((i>1 && type[i-1]!="con") || i==1) && type[i]=="try") data <- rbind(data,c(action[,i][2],"missed conversion",thome,taway,where[i],round,"NA",date))
              }
            }
          }
        }
        ### Fetch stats ###
        if(fetchstats){
          stats <- rbind(stats,c("Score",thome,taway,score[["home"]],score[["away"]],round,date))
          action <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.more-items')).map(el => el.innerText))")$result$value)
          tmp <- do.call("rbind",lapply(1:2,function(i) matrix(strsplit(action[i],"\n")[[1]],ncol=3,byrow=TRUE)))
          tmp <- cbind(matrix(c(tmp[,2],rep(thome,nrow(tmp)),rep(taway,nrow(tmp)),tmp[,1],tmp[,3],rep(round,nrow(tmp))),nrow=nrow(tmp),byrow=FALSE),rep(date,nrow(tmp)))
          colnames(tmp) <- colnames(stats)
          stats <- rbind(stats,tmp)
          
          action <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.ruck-speed')).map(el => el.innerText))")$result$value)
          tmp <- matrix(strsplit(action,"\n")[[1]][-1],ncol=3,byrow=TRUE)
          a <- tmp[1:3,2]
          tmp[1:3,2] <- tmp[1:3,1]
          tmp[1:3,1] <- a
          tmp <- cbind(matrix(c(tmp[,2],rep(thome,nrow(tmp)),rep(taway,nrow(tmp)),tmp[,1],tmp[,3],rep(round,nrow(tmp))),nrow=nrow(tmp),byrow=FALSE),rep(date,nrow(tmp)))
          colnames(tmp) <- colnames(stats)
          stats <- rbind(stats,tmp)
          
          navigate_safe(b,paste0(substr(idm,0,nchar(idm)-9),"stats/",substr(idm,nchar(idm)-8,nchar(idm))),"stat",retries = 2)
          Sys.sleep(0.1)
          action <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.stat')).map(el => el.innerText))")$result$value)
          tmp <- do.call("rbind",lapply(1:length(action),function(i) matrix(strsplit(action[i],"\n")[[1]],ncol=3,byrow=TRUE)))
          tmp <- cbind(matrix(c(tmp[,2],rep(thome,nrow(tmp)),rep(taway,nrow(tmp)),tmp[,1],tmp[,3],rep(round,nrow(tmp))),nrow=nrow(tmp),byrow=FALSE),rep(date,nrow(tmp)))
          colnames(tmp) <- colnames(stats)
          stats <- rbind(stats,tmp)
          
          action <- jsonlite::fromJSON(b$Runtime$evaluate("JSON.stringify(Array.from(document.querySelectorAll('.field')).map(el => el.innerText))")$result$value)
          tmp <- do.call("rbind",lapply(1:length(action),function(i) matrix(strsplit(action[i],"\n")[[1]],ncol=4,byrow=TRUE)))
          type <- matrix(c("Territory Total 0-22","Territory Total 22-50","Territory Total 50-22","Territory Total 22-0","Possession 0-22","Possession 22-50","Possession 50-22","Possession 22-0"),nrow=2,byrow=TRUE)
          tmp1 <- cbind(matrix(c(type[1,],rep(thome,ncol(tmp)),rep(taway,ncol(tmp)),tmp[1,],tmp[1,],rep(round,ncol(tmp))),nrow=ncol(tmp),byrow=FALSE),rep(date,ncol(tmp)))
          colnames(tmp1) <- colnames(stats)
          stats <- rbind(stats,tmp1)
          tmp2 <- cbind(matrix(c(type[2,],rep(thome,ncol(tmp)),rep(taway,ncol(tmp)),tmp[2,],tmp[3,],rep(round,ncol(tmp))),nrow=ncol(tmp),byrow=FALSE),rep(date,ncol(tmp)))
          colnames(tmp2) <- colnames(stats)
          stats <- rbind(stats,tmp2)
        }
        b$close()
      }
    }
  }
  data <- data[-1,]
  stats <- stats[-1,]
  
  tradtypegene <- c("try","penalty try","penalty","drop","conversion","yellow","red","orange")
  tradtypepass <- c("try","penalty try","pg","dg","con","yc","rc","oc")
  for(j1 in 1:length(tradtypegene)) data$type[data$type==tradtypepass[j1]] <- tradtypegene[j1]
  
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




