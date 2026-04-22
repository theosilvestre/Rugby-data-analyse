library(xml2)

#find in an html text the line (in lines) and the row (in the corresponding string) in which appear the last character of pat
findlinerow <- function(lines,pat){
  res <- matrix(grep(pat,lines),ncol=1)
  res <- cbind(res, sapply(1:nrow(res), function(i) gregexpr(pat,lines[res[i,1]])[[1]][1] + nchar(pat)))
  return(res)
}
#use findlinerow and then return the string begining at the end of the occurrence of pat + 'plus' and ending 'to' rows later 
findlinerowextract <- function(lines,pat,to,plus=0){
  ind <- findlinerow(lines,pat)
  return( substr(lines[ind[1]],ind[2]+plus,ind[2]+plus+to) )  
}
#wrap repetitive elements used below. It uses values defined outside of the function scope, as authorized in R, so it must be used after the line 50
findtypematch <- function(type,label,data){ 
  indweb <- grep(type,weblines)
  if(length(indweb)>0){
    for(i in 1:length(indweb)){
      tmp <- c()
      tmp <- c(tmp, strsplit(findlinerowextract(weblines[indweb[i]],"span_idalgo_content_match_action_part_minute",5,2),'<')[[1]][1] )
      if(is.element(indweb[i],indlocal)) tmp <- c(tmp,label,thome,taway,"home")
      else tmp <- c(tmp,label,thome,taway,"away")
      tmp <- c(tmp,round)   #round
      name <- strsplit(findlinerowextract(weblines[indweb[i]],"a_idalgo_content_match_action_part_player",1000000), ">")[[1]][2]
      tmp <- c(tmp, tolower(xml_text(read_html(paste0("<x>", substr(name,1,nchar(name)-3), "</x>")))))
      data <- rbind(data,tmp)
    }
  }
  return(data)
}

#Rugbyrama only stores full match data of the current season (2025/2026 at the time of writing)
url <- "https://www.rugbyrama.fr/resultats/rugby/top-14/calendrier"
webpage <-download.file(url,"rugbyrama.html")
lines <- readLines("rugbyrama.html")
pat <- "/resultats/rugby/top-14/phase-reguliere/rencontre/"
link <- findlinerow(lines,pat)

id <- rep(NA,nrow(link))
for(i in 1:nrow(link)) id[i] <- substr(lines[link[i,1]],link[i,2],link[i,2]+4)

data <- data.frame("minute"=NA,"type"=NA,"home"=NA,"away"=NA,"where"=NA,"round"=NA,"player"=NA)
for(idm in id){
  url <- paste0("https://www.rugbyrama.fr",pat,idm,"/")
  webpage <-download.file(url,"match.html")
  
  weblines <- readLines("match.html", encoding = "UTF-8")
  indlocal <- grep("local",weblines)
  indvisit <- grep("visitor",weblines)
  taway <- strsplit(findlinerowextract(weblines,"a_idalgo_content_match_header_full_main_header_name_teams_visitor",1000000), ">")[[1]][2]
  taway <- tolower(xml_text(read_html(paste0("<x>", substr(taway,1,nchar(taway)-3), "</x>"))))
  thome <- strsplit(findlinerowextract(weblines,"a_idalgo_content_match_header_full_main_header_name_teams_local",1000000), ">")[[1]][2]
  thome <- tolower(xml_text(read_html(paste0("<x>", substr(thome,1,nchar(thome)-3), "</x>"))))
  round <- strsplit(findlinerowextract(weblines,"Journ&eacute;e ",1) , "<")[[1]]
  
  data <- findtypematch("idalgo_content_action_logo_penalite_in","penalty",data)
  data <- findtypematch("idalgo_content_action_logo_penalite_out","missed penalty",data)
  data <- findtypematch("idalgo_content_action_logo_yellow","yellow",data)
  data <- findtypematch("idalgo_content_action_logo_red","red",data)
  data <- findtypematch("idalgo_content_action_logo_yellowred","orange",data)
  data <- findtypematch("idalgo_content_action_logo_drop_in","drop",data)
  data <- findtypematch("idalgo_content_action_logo_drop_out","missed drop",data)
  indweb <- grep("idalgo_content_action_logo_try",weblines)
  if(length(indweb)>0){
    for(i in 1:length(indweb)){
      #Try
      tmp <- c()
      tmp <- c(tmp, strsplit(findlinerowextract(weblines[indweb[i]],"span_idalgo_content_match_action_part_minute",5,2),'<')[[1]][1] )
      if(is.element(indweb[i],indlocal)) tmp <- c(tmp,"try",thome,taway,"home")
      else tmp <- c(tmp,"try",thome,taway,"away")
      tmp <- c(tmp,round)   #round
      name <- strsplit(findlinerowextract(weblines[indweb[i]],"a_idalgo_content_match_action_part_player",1000000), ">")[[1]][2]
      tmp <- c(tmp, tolower(xml_text(read_html(paste0("<x>", substr(name,1,nchar(name)-3), "</x>")))))
      if(tmp[7]=="essai de pénalité") tmp[2] <- "penalty try"
      data <- rbind(data,tmp)
      #Conversion
      if(data[nrow(data),2]!="penalty try"){
        if(length(grep("idalgo_content_action_logo_transformation_in",weblines[indweb[i]+2]))==1){ tmp[2] <- "conversion"; flag <- TRUE }
        if(length(grep("idalgo_content_action_logo_transformation_out",weblines[indweb[i]+2]))==1){ tmp[2] <- "missed conversion"; flag <- TRUE }
        if(flag){
          name <- strsplit(findlinerowextract(weblines[indweb[i]+2],"a_idalgo_content_match_action_part_detail",1000000), ">")[[1]][2]
          tmp[7] <- tolower(xml_text(read_html(paste0("<x>", substr(name,1,nchar(name)-3), "</x>"))))
        }
        else{
          tmp[2] <- "missed conversion"
          tmp[7] <- NA
        }
        data <- rbind(data,tmp)
        flag <- FALSE
      }
    }
  }
}
data <- data[!duplicated(data),][-1,]
data <- data[order(data$home,data$round),]

# Cards data is not consistent, it seems that at least one red and one orange card are missing and there is one yellow card that is not counted on the website of the LNR (to date round 21 season 2025/2026)
data[data$type=="yellow" & data$player=="staniforth" & data$round==6 & data$minute=="7'",]$type <- "orange" 
data[data$type=="yellow" & data$player=="taofifenua" & data$round==2 & data$minute=="70'",]$type <- "orange"
data[data$type=="red" & data$player=="simmons" & data$round==2 & data$minute=="69'",]$type <- "orange"
data[data$type=="red" & data$player=="anyanwu" & data$round==19 & data$minute=="61'",]$type <- "orange"
data[data$type=="red" & data$player=="owen maafu" & data$round==8 & data$minute=="21'",]$type <- "orange"
if(data[data$minute=="78'" & data$home=="racing 92" & data$round==2 & data$type=="yellow",7]=="carbonneau") data <- data[-which(data$minute=="78'" & data$home=="racing 92" & data$round==2 & data$type=="yellow"),]
data <- rbind(data,c("79'","yellow","castres","montauban","away",20,"ringuet"))
# Conversions are missing
data[data$home=="bordeaux-bègles" & data$away=="stade français" & data$minute=="33'" & data$type=="missed conversion",2] <- "conversion"
data <- rbind(data,c("63'","try","bayonne","la rochelle","home",20,"tiberghien"))
data <- rbind(data,c("63'","missed conversion","bayonne","la rochelle","home",20,NA))
data[data$home=="la rochelle" & data$away=="bordeaux-bègles" & data$type=="missed conversion" & data$minute=="25'",2] <- "conversion"
data[data$home=="pau" & data$away=="racing 92" & data$type=="missed conversion" & data$minute=="80'",2] <- "conversion"
data <- data[-which(data$minute=="80'+11"),]

save(data,file="top14.rda")

