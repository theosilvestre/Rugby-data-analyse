load("prod2.rda")


teams <- unique(data$home)
checkData <- data.frame(rep(NA,length(teams)),rep(NA,length(teams)),rep(0,length(teams)),rep(0,length(teams)),rep(0,length(teams)),rep(0,length(teams)))
colnames(checkData) <- c("Tries scored","Tries taken","Total points scored home","Total points taken home","Total points scored away","Total points taken away")
rownames(checkData) <- teams
for(i in 1:length(teams)) checkData[i,1] <- nrow(data[(data$type=="try" | data$type=="penalty try") & ((data$home==teams[i] & data$where=="home") | (data$away==teams[i] & data$where=="away")),])
for(i in 1:length(teams)) checkData[i,2] <- nrow(data[(data$type=="try" | data$type=="penalty try") & ((data$home==teams[i] & data$where=="away") | (data$away==teams[i] & data$where=="home")),])
type <- c("try","conversion","penalty try","penalty","drop")
weights <- c(5,2,7,3,3)
for(i in 1:length(teams)){
  for(j in 1:length(type)) checkData[i,3] <- checkData[i,3] + weights[j] * nrow(data[data$type==type[j] & data$home==teams[i] & data$where=="home",])
  for(j in 1:length(type)) checkData[i,4] <- checkData[i,4] + weights[j] * nrow(data[data$type==type[j] & data$home==teams[i] & data$where=="away",])
  for(j in 1:length(type)) checkData[i,5] <- checkData[i,5] + weights[j] * nrow(data[data$type==type[j] & data$away==teams[i] & data$where=="away",])
  for(j in 1:length(type)) checkData[i,6] <- checkData[i,6] + weights[j] * nrow(data[data$type==type[j] & data$away==teams[i] & data$where=="home",])
}
checkData$`Total points scored` <- checkData$`Total points scored home`+checkData$`Total points scored away`

checkDataMatch <- data.frame(rep(NA,length(teams)*(length(teams)-1)),rep(NA,length(teams)*(length(teams)-1)),rep(0,length(teams)*(length(teams)-1)),rep(0,length(teams)*(length(teams)-1)))
for(i in 1:length(teams)){
  for(j in 1:length(teams)){
    if(j != i){
      for(t in 1:length(type)) checkDataMatch[j+(i-1)*length(teams),1] <- teams[i]
      for(t in 1:length(type)) checkDataMatch[j+(i-1)*length(teams),2] <- teams[j]
      for(t in 1:length(type)) checkDataMatch[j+(i-1)*length(teams),3] <- checkDataMatch[j+(i-1)*length(teams),3] + weights[t] * nrow(data[data$home==teams[i] & data$away==teams[j] & data$type==type[t] & data$where=="home",])
      for(t in 1:length(type)) checkDataMatch[j+(i-1)*length(teams),4] <- checkDataMatch[j+(i-1)*length(teams),4] + weights[t] * nrow(data[data$home==teams[i] & data$away==teams[j] & data$type==type[t] & data$where=="away",])
    }
  }
}
colnames(checkDataMatch) <- c("team1","team2","home","away")
checkDataMatch <- checkDataMatch[!is.na(checkDataMatch$team1),]

cards <- data.frame(rep(NA,length(teams)),rep(NA,length(teams)),rep(NA,length(teams)))
colnames(cards) <- c("yellow","red","orange")
rownames(cards) <- teams
for(i in 1:length(teams)) cards[i,1] <- nrow(data[data$type=="yellow" & ((data$home==teams[i] & data$where=="home") | (data$away==teams[i] & data$where=="away")),])
for(i in 1:length(teams)) cards[i,2] <- nrow(data[data$type=="red" & ((data$home==teams[i] & data$where=="home") | (data$away==teams[i] & data$where=="away")),])
for(i in 1:length(teams)) cards[i,3] <- nrow(data[data$type=="orange" & ((data$home==teams[i] & data$where=="home") | (data$away==teams[i] & data$where=="away")),])

cat(paste0("Number of cards:\n",
           "Number of yellow cards: ",sum(data$type=="yellow"),"\n",
           "Number of red cards: ",sum(data$type=="red"),"\n",
           "Number of orange cards: ",sum(data$type=="orange"),"\n"
))
print(checkData)

