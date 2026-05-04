source("scrape_annex.R")

##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##
# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #-# BEGINNING #
##-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-##

findMatchRama <- function(union){
  #Rugbyrama only stores full match data of the current season (2025/2026 at the time of writing)
  if(!is.element(union, c("pro-d2","top-14","premiership","challenge-cup","champions-cup"))) stop("Union not in the list.")
  url <- paste0("https://www.rugbyrama.fr/resultats/rugby/",union,"/calendrier")
  download.file(url,"rugbyrama.html")
  lines <- readLines("rugbyrama.html")
  
  pat <- "a_idalgo_content_calendar_cup_date_match_score a_idalgo_content_result_match_score_end"
  link <- findlinerow(lines,pat)
  id <- rep(NA,nrow(link))
  for(i in 1:nrow(link)) id[i] <- strsplit(substr(lines[link[i,1]],link[i,2]+8,link[i,2]+1000),'"')[[1]][1]
  return(id)
}

# union <- c("pro-d2","top-14","premiership","challenge-cup","champions-cup")s
scraperama <- function(union,addMat=NULL,whichMatches=NULL){
  if(!is.element(union, c("pro-d2","top-14","premiership","challenge-cup","champions-cup"))) stop("Union not in the list.")
  if(!is.null(addMat)) if(any(colnames(addMat)!=c("minute","type","home","away","where","score","round","player","date"))) stop("Cannot add to the given matrix. Wrong colnames.")
  
  id <- findMatchRama(union)
  if(is.null(whichMatches) || !is.integer(whichMatches) || any(whichMatches < 1) || any(whichMatches > length(id))){
    stop("Wrong range for whichMatches. Check findMatchRama().")
  } else {
    id <- id[whichMatches]
  }
  data <- data.frame("minute"=NA,"type"=NA,"home"=NA,"away"=NA,"where"=NA,"score"=NA,"round"=NA,"player"=NA,"date"=NA)
  for(idm in id){
    url <- paste0("https://www.rugbyrama.fr",idm)
    download.file(url,"match.html",quiet=TRUE)
    weblines <- readLines("match.html", encoding = "UTF-8")
    
    indlocal <- grep("local",weblines)
    taway <- strsplit(findlinerowextract(weblines,"a_idalgo_content_match_header_full_main_header_name_teams_visitor",1000000), ">")[[1]][2]
    taway <- tolower(xml_text(read_html(paste0("<x>", substr(taway,1,nchar(taway)-3), "</x>"))))
    thome <- strsplit(findlinerowextract(weblines,"a_idalgo_content_match_header_full_main_header_name_teams_local",1000000), ">")[[1]][2]
    thome <- tolower(xml_text(read_html(paste0("<x>", substr(thome,1,nchar(thome)-3), "</x>"))))
    round <- tolower(xml_text(read_html(paste0("<x>", strsplit(findlinerowextract(weblines,"span_idalgo_content_match_header_full_detail",100,3) , "<")[[1]][1], "</x>"))))
    date <- tolower(xml_text(read_html(paste0("<x>", strsplit(findlinerowextract(weblines,"span_idalgo_content_match_header_full_date",100,2) , "<")[[1]][1], "</x>"))))
    scoreaway <- strsplit(findlinerowextract(weblines,"idalgo_match_visitorscore_",100,7),"</span>")[[1]][1]
    scorehome <- strsplit(findlinerowextract(weblines,"idalgo_match_localscore_",100,7),"</span>")[[1]][1]
    print(paste(round,thome,taway))
    
    data <- findtypematch(weblines,"idalgo_content_action_logo_penalite_in","penalty",indlocal,thome,taway,scorehome,scoreaway,round,date,data)
    data <- findtypematch(weblines,"idalgo_content_action_logo_penalite_out","missed penalty",indlocal,thome,taway,scorehome,scoreaway,round,date,data)
    data <- findtypematch(weblines,"idalgo_content_action_logo_yellow","yellow",indlocal,thome,taway,scorehome,scoreaway,round,date,data)
    data <- findtypematch(weblines,"idalgo_content_action_logo_red","red",indlocal,thome,taway,scorehome,scoreaway,round,date,data)
    data <- findtypematch(weblines,"idalgo_content_action_logo_yellowred","orange",indlocal,thome,taway,scorehome,scoreaway,round,date,data)
    data <- findtypematch(weblines,"idalgo_content_action_logo_drop_in","drop",indlocal,thome,taway,scorehome,scoreaway,round,date,data)
    data <- findtypematch(weblines,"idalgo_content_action_logo_drop_out","missed drop",indlocal,thome,taway,scorehome,scoreaway,round,date,data)
    indweb <- grep("idalgo_content_action_logo_try",weblines)
    if(length(indweb)>0){
      for(i in 1:length(indweb)){
        #Try
        tmp <- c()
        tmp <- c(tmp, strsplit(findlinerowextract(weblines[indweb[i]],"span_idalgo_content_match_action_part_minute",5,2),'<')[[1]][1] )
        if(is.element(indweb[i],indlocal)) tmp <- c(tmp,"try",thome,taway,"home",scorehome)
        else tmp <- c(tmp,"try",thome,taway,"away",scoreaway)
        tmp <- c(tmp,round)   #round
        name <- strsplit(findlinerowextract(weblines[indweb[i]],"a_idalgo_content_match_action_part_player",1000000), ">")[[1]][2]
        tmp <- c(tmp, tolower(xml_text(read_html(paste0("<x>", substr(name,1,nchar(name)-3), "</x>")))),date)
        if(tmp[8]=="essai de pénalité"){
          tmp[2] <- "penalty try"
          tmp[8] <- NA
        }
        data <- rbind(data,tmp)
        #Conversion
        if(data[nrow(data),2]!="penalty try"){
          if(length(grep("idalgo_content_action_logo_transformation_in",weblines[indweb[i]+2]))==1){ tmp[2] <- "conversion"; flag <- TRUE }
          if(length(grep("idalgo_content_action_logo_transformation_out",weblines[indweb[i]+2]))==1){ tmp[2] <- "missed conversion"; flag <- TRUE }
          if(flag){
            name <- strsplit(findlinerowextract(weblines[indweb[i]+2],"a_idalgo_content_match_action_part_detail",1000000), ">")[[1]][2]
            tmp[8] <- tolower(xml_text(read_html(paste0("<x>", substr(name,1,nchar(name)-3), "</x>"))))
          }
          else{
            tmp[2] <- "missed conversion"
            tmp[8] <- NA
          }
          data <- rbind(data,tmp)
          flag <- FALSE
        }
      }
    }
  }
  data <- data[!duplicated(data),][-1,]
  if(is.null(addMat)){ return(data)   
  } else {
    addMat <- rbind(addMat,data)
    return(addMat[!duplicated(addMat),])
  }
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #-# ENDING #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


