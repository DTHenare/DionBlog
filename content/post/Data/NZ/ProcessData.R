library(dplyr)
library(ggplot2)
library(tidyr)

MPrty = ElectionData$Party[15]
mainParties = c("New Zealand First Party","Labour Party","National Party","Green Party","ACT New Zealand", MPrty)
maoriElecs = c( "Hauraki-Waikato","Ikaroa-Rawhiti","Tamaki Makaurau","Te Tai Hauauru","Te Tai Tokerau","Te Tai Tonga","Waiariki")

PartyVotesCast <- ElectionData %>%
  filter(!is.na(CandidateVotes) & !is.na(PartyVotes)) %>%
  summarise(Total = sum(PartyVotes))
  
ElectionData %>%
  filter(!is.na(CandidateVotes), !is.na(PartyVotes)) %>%
  filter(Party == "New Zealand First Party"| Party == "Labour Party"| Party == "National Party"| Party == "Green Party"| Party == "ACT New Zealand") %>%
  group_by(Party,Electorate) %>%
  summarise(Strategic = CandidateVotes-PartyVotes) %>%
  ggplot(., aes(x=Electorate, y=Strategic)) +
  geom_bar(stat = "identity") +
  facet_grid(Party~.) +
  theme(axis.text.x = element_text(angle = 90))

ElectionData %>%
  #filter(!is.na(CandidateVotes), !is.na(PartyVotes)) %>%
  gather("VoteType","VoteCount", CandidateVotes,PartyVotes) %>%
  filter(Party %in% mainParties) %>%
  ggplot(., aes(x=Electorate, y=VoteCount, Group = VoteType, fill = VoteType)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(Party~.) +
  theme(axis.text.x = element_text(angle = 90))

ElectionData %>%
  #filter(!is.na(CandidateVotes), !is.na(PartyVotes)) %>%
  gather("VoteType","VoteCount", CandidateVotes,PartyVotes) %>%
  #filter(!Party %in% mainParties) %>%
  filter(Party == "National Party" & (Electorate %in% Electorate2017Key$Electorate & Electorate %in% Electorate2020Key$Electorate)) %>%
  ggplot(., aes(x=Electorate, y=VoteCount, Group = Year, fill = Year)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(VoteType~.) +
  theme(axis.text.x = element_text(angle = 90))

ElectionData %>%
  #filter(!is.na(CandidateVotes), !is.na(PartyVotes)) %>%
  gather("VoteType","VoteCount", CandidateVotes,PartyVotes) %>%
  #filter(!Party %in% mainParties) %>%
  filter(Party == "National Party" & (Electorate %in% Electorate2017Key$Electorate & Electorate %in% Electorate2020Key$Electorate) & !(Electorate %in% maoriElecs)) %>%
  ggplot(., aes(x=Year, y=VoteCount, group = Electorate)) +
  geom_line() +
  facet_grid(.~VoteType) +
  scale_x_discrete(limits = c("2017","2020"))

ElectionData %>%
  #filter(!is.na(CandidateVotes), !is.na(PartyVotes)) %>%
  gather("VoteType","VoteCount", CandidateVotes,PartyVotes) %>%
  #filter(!Party %in% mainParties) %>%
  filter((Party == "National Party" |Party == "Labour Party") & !(Electorate %in% maoriElecs)) %>%
  ggplot(., aes(x=VoteType, y=VoteCount, group = Electorate)) +
  geom_line() +
  geom_point() +
  facet_grid(Party~Year)


##Maori Electorates
Totals <- ElectionData %>%
  gather("VoteType","VoteCount", CandidateVotes,PartyVotes) %>%
  mutate(Party = ifelse(!(Party %in% mainParties),"Other",Party)) %>%
  mutate(Year = ifelse(Year==2020,"Twenty","Seventeen")) %>%
  group_by(Electorate, Year, VoteType) %>%
  summarise(VoteTotal = sum(VoteCount, na.rm = TRUE))
ElectionData <- ElectionData %>% 
  gather("VoteType","VoteCount", CandidateVotes,PartyVotes)
for (index in 1:length(ElectionData$VoteCount)) {
  CurrentYear = ifelse(ElectionData$Year[index]==2020,"Twenty","Seventeen")
  ElectionData$Total[index] = Totals %>% ungroup() %>% filter(Electorate == ElectionData$Electorate[index], Year == CurrentYear, VoteType == ElectionData$VoteType[index]) %>% pull(VoteTotal)
}
ElectionData %>%
  mutate(Party = ifelse(!(Party %in% mainParties),"Other",Party), 
         Party = as.factor(Party), 
         Party =factor(Party, levels = c("Green Party", MPrty, "Labour Party", "New Zealand First Party", "National Party", "ACT New Zealand", "Other")), 
         Year = as.factor(Year), 
         Year = factor(Year, levels = c("2017","2020"))) %>%
  select(-"Candidates")%>%
  group_by(Party,Electorate,Year,VoteType) %>%
  summarise(VoteCount = sum(VoteCount, na.rm = TRUE), Total = mean(Total)) %>%
  ungroup() %>%
  filter((Electorate %in% maoriElecs)) %>%
  mutate(Percent = (VoteCount/Total)*100) %>%
  ggplot(., aes(x=Party, y=Percent, group= Year, fill =Party)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_grid(Electorate~VoteType) +
  scale_fill_manual(values=c("chartreuse4", "firebrick4", "red3", "grey14", "royalblue3", "gold", "grey40")) +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.spacing.x = unit(2, "lines"))
ElectionData %>%
  mutate(Party = ifelse(!(Party %in% mainParties),"Other",Party), 
         Party = as.factor(Party), 
         Party =factor(Party, levels = c("Green Party", MPrty, "Labour Party", "New Zealand First Party", "National Party", "ACT New Zealand", "Other")), 
         Year = as.factor(Year), 
         Year = factor(Year, levels = c("2017","2020"))) %>%
  select(-"Candidates")%>%
  group_by(Party,Electorate,Year,VoteType) %>%
  summarise(VoteCount = sum(VoteCount, na.rm = TRUE), Total = mean(Total)) %>%
  ungroup() %>%
  filter(!(Electorate %in% maoriElecs), VoteType == "PartyVotes", Year == "2020") %>%
  mutate(Percent = (VoteCount/Total)*100) %>%
  ggplot(., aes(x=Party, y=Percent, fill =Party)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_wrap(~Electorate) +
  scale_fill_manual(values=c("chartreuse4", "firebrick4", "red3", "grey14", "royalblue3", "gold", "grey40")) +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.spacing.x = unit(2, "lines"))
ElectionData %>%
  mutate(Party = ifelse(!(Party %in% mainParties),"Other",Party), 
         Party = as.factor(Party), 
         Party =factor(Party, levels = c("Green Party", MPrty, "Labour Party", "New Zealand First Party", "National Party", "ACT New Zealand", "Other")), 
         Year = as.factor(Year), 
         Year = factor(Year, levels = c("2017","2020"))) %>%
  select(-"Candidates")%>%
  group_by(Party,Electorate,Year,VoteType) %>%
  summarise(VoteCount = sum(VoteCount, na.rm = TRUE), Total = mean(Total)) %>%
  ungroup() %>%
  filter(!(Electorate %in% maoriElecs), VoteType == "CandidateVotes") %>%
  mutate(Percent = (VoteCount/Total)*100) %>%
  ggplot(., aes(x=Party, y=Percent, group= Year, fill =Party)) +
  geom_bar(stat="identity", position = "dodge") +
  facet_wrap(~Electorate) +
  scale_fill_manual(values=c("chartreuse4", "firebrick4", "red3", "grey14", "royalblue3", "gold", "grey40")) +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.spacing.x = unit(2, "lines"))
  
Totals %>% ungroup() %>% filter(Electorate == "Hauraki-Waikato", Year == "2020", VoteType == "CandidateVotes") %>% select(-c("Electorate", "Year", "VoteType")) %>% pull(VoteTotal)
  
  