library(rvest)
library(stringr)

#2017 data
Electorate2017Key = read.csv("Data/NZ/Electorate2017Key.csv", header = TRUE)
Election2017Data <- data.frame()
for (elec in 1:length(Electorate2017Key$Value)) {
  ElecData <- read_html(
    paste("Data/NZ/2017/E9 Statistics - Electorate Status",as.character(elec),".html",sep = "")
  )
  MMPNames <- ElecData %>% 
    str_extract_all('(?<=<span>)(.*?)(?=</span>)') %>% 
    unlist()
  Party <- MMPNames[seq(2,length(MMPNames)-2,2)]
  Candidates <- MMPNames[seq(1,length(MMPNames)-3,2)]
  MMPNumber <- ElecData %>% 
    str_extract_all('(?<=<span class="float-right">)(.*?)(?=</span>)') %>% 
    unlist()
  PartyVotes <- as.integer(MMPNumber[seq(2,length(MMPNames)-2,2)])
  CandidateVotes <- as.integer(MMPNumber[seq(1,length(MMPNames)-3,2)])
  
  Summary <- data.frame(cbind(Candidates,CandidateVotes,Party,PartyVotes),stringsAsFactors = FALSE)
  Summary$Electorate= Electorate2017Key$Electorate[Electorate2017Key$Value==elec]
  Summary$CandidateVotes = as.integer(Summary$CandidateVotes)
  Summary$PartyVotes = as.integer(Summary$PartyVotes)
  Summary$Year = as.factor(2017)
  Election2017Data <- rbind(Election2017Data,Summary)
}

#2020 data
Electorate2020Key = read.csv("Data/NZ/Electorate2020Key.csv", header = TRUE)
Election2020Data <- data.frame()
for (elec in 1:length(Electorate2020Key$Value)) {
ElecData <- read_html(
  paste("Data/NZ/2020/Election - Electorate Status",as.character(elec),".html",sep = "")
)
MMPNames <- ElecData %>% 
  str_extract_all('(?<=<span>)(.*?)(?=</span>)') %>% 
  unlist()
Party <- MMPNames[seq(2,length(MMPNames)-2,2)]
Candidates <- MMPNames[seq(1,length(MMPNames)-3,2)]
MMPNumber <- ElecData %>% 
  str_extract_all('(?<=<span class="float-right">)(.*?)(?=</span>)') %>% 
  unlist()
PartyVotes <- as.integer(MMPNumber[seq(2,length(MMPNames)-2,2)])
CandidateVotes <- as.integer(MMPNumber[seq(1,length(MMPNames)-3,2)])

Summary <- data.frame(cbind(Candidates,CandidateVotes,Party,PartyVotes),stringsAsFactors = FALSE)
Summary$Electorate= Electorate2020Key$Electorate[Electorate2020Key$Value==elec]
Summary$CandidateVotes = as.integer(Summary$CandidateVotes)
Summary$PartyVotes = as.integer(Summary$PartyVotes)
Summary$Year = as.factor(2020)
Election2020Data <- rbind(Election2020Data,Summary)
}

ElectionData <- rbind(Election2020Data,Election2017Data)
rm(Election2020Data,Election2017Data)
