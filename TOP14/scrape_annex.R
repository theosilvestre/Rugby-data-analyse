library(xml2)

#find in an html text the line (in lines) and the row (in the corresponding string) in which appear the last character of pat
findlinerow <- function(lines,pat){
  if(length(grep("pat",lines))) return(NULL)
  else{
    res <- matrix(grep(pat,lines),ncol=1)
    res <- cbind(res, sapply(1:nrow(res), function(i) gregexpr(pat,lines[res[i,1]])[[1]][1] + nchar(pat)))
    return(res)
  }
}
#use findlinerow and then return the string begining at the end of the occurrence of pat + 'plus' and ending 'to' rows later 
findlinerowextract <- function(lines,pat,to,plus=0){
  ind <- findlinerow(lines,pat)
  return( substr(lines[ind[1]],ind[2]+plus,ind[2]+plus+to) )  
}
#wrap repetitive elements used below. It uses values defined outside of the function scope, as authorized in R, so it must be used after the line 50
findtypematch <- function(weblines,type,label,indlocal,thome,taway,scorehome,scoreaway,round,date,data){ 
  indweb <- grep(type,weblines)
  if(length(indweb)>0){
    for(i in 1:length(indweb)){
      tmp <- c()
      tmp <- c(tmp, strsplit(findlinerowextract(weblines[indweb[i]],"span_idalgo_content_match_action_part_minute",5,2),'<')[[1]][1] )
      if(is.element(indweb[i],indlocal)) tmp <- c(tmp,label,thome,taway,"home",scorehome)
      else tmp <- c(tmp,label,thome,taway,"away",scoreaway)
      tmp <- c(tmp,round)   #round
      name <- strsplit(findlinerowextract(weblines[indweb[i]],"a_idalgo_content_match_action_part_player",1000000), ">")[[1]][2]
      tmp <- c(tmp, tolower(xml_text(read_html(paste0("<x>", substr(name,1,nchar(name)-3), "</x>")))),date)
      data <- rbind(data,tmp)
    }
  }
  return(data)
}

navigate_safe <- function(session, url, class=NULL, timeout = 5000, retries = 2) { 
  for (i in seq_len(retries)) {
    try({
      session$Page$navigate(url, timeout_ = timeout)
      if(is.null(class)){ session$Page$loadEventFired(wait_ = TRUE)
      } else {
        check <- NULL
        while(is.null(check)){
          check <- session$Runtime$evaluate(paste0("
          new Promise(resolve => {
            if (document.querySelector('.",class,"')) {
              resolve(true);
              return;
            }

            const observer = new MutationObserver(() => {
              if (document.querySelector('.",class,"')) {
                observer.disconnect();
                resolve(true);
              }
            });

          observer.observe(document.body, {
            childList: true,
            subtree: true
          });
        });
      "), awaitPromise = TRUE,wait_ = TRUE)$result$value
        }
      }
      return(TRUE)
    }, silent = TRUE)
    
    Sys.sleep(1)
  }
  # stop("Navigation failed: ", url)
  return(FALSE)
}
