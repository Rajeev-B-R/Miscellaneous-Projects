#loading crime by state dataset
df <- read.csv("C:/Users/Rajeev/Downloads/crimeanalysis/crime_by_state_rt.csv")

#viewing dataset
View(df)

#dimensions
dim(df)

#feature names
names(df)

#summary of features present
str(df)

#Let's check if there is any missing data.
sum(is.na(df))

#Crimes v/s STATE/UT
#Exploring  Murder, Assault on Women, Kidnapping and Abduction v/s STATE/UT
p1 = ggplot(df) +      
  geom_point(aes(Murder, STATE.UT), colour = "coral1", alpha = 0.3) +     
  theme(axis.title = element_text(size = 8.5))
p2 = ggplot(df) +       
  geom_point(aes(Assault.on.women, STATE.UT), colour = "green", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))
p3 = ggplot(df) +       
  geom_point(aes(Kidnapping.and.Abduction, STATE.UT), colour = "blue", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p2, p3, ncol = 2) 
plot_grid(p1, second_row_2, nrow = 2)

#Exploring  Dacoity, Robbery, Arson v/s STATE/UT
p4 = ggplot(df) +      
  geom_point(aes(Dacoity, STATE.UT), colour = "black", alpha = 0.3) +     
  theme(axis.title = element_text(size = 8.5))
p5 = ggplot(df) +       
  geom_point(aes(Robbery, STATE.UT), colour = "red", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))
p6 = ggplot(df) +       
  geom_point(aes(Arson, STATE.UT), colour = "brown", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p5, p6, ncol = 2) 
plot_grid(p4, second_row_2, nrow = 2)

#Exploring  Hurt, POA Act, PCR Act v/s STATE/UT
p7 = ggplot(df) +      
  geom_point(aes(Hurt, STATE.UT), colour = "coral1", alpha = 0.3) +     
  theme(axis.title = element_text(size = 8.5))
p8 = ggplot(df) +       
  geom_point(aes(Prevention.of.atrocities..POA..Act, STATE.UT), colour = "black", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))
p9 = ggplot(df) +       
  geom_point(aes(Protection.of.Civil.Rights..PCR..Act, STATE.UT), colour = "green", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p8, p9, ncol = 2) 
plot_grid(p7, second_row_2, nrow = 2)

#Exploring Other crimes against SCs v/s STATE/UT
ggplot(df) +       
  geom_point(aes(Other.Crimes.Against.SCs, STATE.UT), colour = "red", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))


#Crimes v/s Year
#Exploring  Murder, Assault on Women, Kidnapping and Abduction v/s Year
p10 = ggplot(df) +      
  geom_point(aes(Murder, Year), colour = "coral1", alpha = 0.3) +     
  theme(axis.title = element_text(size = 8.5))
p11 = ggplot(df) +       
  geom_point(aes(Assault.on.women, Year), colour = "green", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))
p12 = ggplot(df) +       
  geom_point(aes(Kidnapping.and.Abduction, Year), colour = "blue", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p11, p12, ncol = 2) 
plot_grid(p10, second_row_2, nrow = 2)

#Exploring  Dacoity, Robbery, Arson v/s STATE/UT
p13 = ggplot(df) +      
  geom_point(aes(Dacoity, Year), colour = "black", alpha = 0.3) +     
  theme(axis.title = element_text(size = 8.5))
p14 = ggplot(df) +       
  geom_point(aes(Robbery, Year), colour = "red", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))
p15 = ggplot(df) +       
  geom_point(aes(Arson, Year), colour = "brown", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p14, p15, ncol = 2) 
plot_grid(p13, second_row_2, nrow = 2)

#Exploring  Hurt, POA Act, PCR Act v/s STATE/UT
p16 = ggplot(df) +      
  geom_point(aes(Hurt, Year), colour = "coral1", alpha = 0.3) +     
  theme(axis.title = element_text(size = 8.5))
p17 = ggplot(df) +       
  geom_point(aes(Prevention.of.atrocities..POA..Act, Year), colour = "black", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))
p18 = ggplot(df) +       
  geom_point(aes(Protection.of.Civil.Rights..PCR..Act, Year), colour = "green", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))

second_row_2 = plot_grid(p17, p18, ncol = 2) 
plot_grid(p16, second_row_2, nrow = 2)

#Exploring Other crimes against SCs v/s STATE/UT
ggplot(df) +       
  geom_point(aes(Other.Crimes.Against.SCs, Year), colour = "red", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))


#load the crime by district dataset
data <- read.csv("C:/Users/Rajeev/Downloads/crimeanalysis/crime_by_district_rt.csv")

#viewing dataset
View(data)

#dimensions
dim(data)

#feature names
names(data)

#summary of features present
str(data)

#Let's check if there is any missing data.
sum(is.na(data))

#Adding population data -
popu = data.frame(Year=c(2001:2012),Pop.billion=c(1070000000,1080000000,1100000000,1120000000,1140000000,1160000000,1170000000,1190000000,1210000000,1230000000,1240000000,1260000000))
data = merge(data,popu)


#We add a new column called Total Crimes as indicated below.
data$Total.Crimes <- data$Murder + data$Assault.on.women + data$Kidnapping.and.Abduction +
                     data$Dacoity + data$Robbery + data$Arson + data$Hurt +
                     data$Prevention.of.atrocities..POA..Act + data$Protection.of.Civil.Rights..PCR..Act +
                     data$Other.Crimes.Against.SCs

data$st.dt = paste(tolower(data$STATE.UT),"-",tolower(data$DISTRICT)) #creating new col with st-dist name

#viewing dataset
View(data)

#Creating subsets 
totby_pop_yr = data %>% select(Year,Total.Crimes,Pop.billion) %>% 
  group_by(Year) %>% 
  summarise(Total.Crimes = sum(Total.Crimes),pop=mean(Pop.billion))

totby_st_yr = data %>% select(Year,STATE.UT,Total.Crimes,Pop.billion) %>%  group_by(STATE.UT,Year) %>%  summarise(Total.Crimes=sum(Total.Crimes),pop = mean(Pop.billion))
totby_st = totby_st_yr %>% group_by(STATE.UT) %>% summarise(Total.Crimes=sum(Total.Crimes),pop = max(pop))

totby_dis_yr = data %>% select(DISTRICT,Total.Crimes,Year,st.dt) %>%  group_by(DISTRICT,Year,st.dt) %>%  summarise(Total.Crimes = sum(Total.Crimes))
totby_dis = totby_dis_yr %>% group_by(DISTRICT,st.dt) %>% summarise(Total.Crimes=sum(Total.Crimes))


#Crimes Per Year
ggplot(totby_pop_yr) +
  geom_bar(aes(Year,Total.Crimes,fill=Year),stat='identity') +
  xlab("Year") +
  ylab("Total Crimes") + 
  ggtitle("Crimes per Year") + 
  guides(fill=FALSE) + 
  theme_classic()


#Crimes per State 
ggplot(totby_st) + 
  geom_bar(aes(STATE.UT,Total.Crimes,fill=Total.Crimes),stat='identity') +
  xlab("State") +
  ylab("Crimes") + 
  ggtitle("Crimes per State") + 
  guides(fill=FALSE) + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#STATE/UT v/s Total Crimes: Top 10 states
states = totby_st %>% select(STATE.UT,Total.Crimes) %>% arrange(desc(Total.Crimes))
head(states,10)


#Crimes per District 
ggplot(totby_dis,aes(DISTRICT,Total.Crimes)) + 
  geom_point(aes(size=Total.Crimes,colour=DISTRICT,label=DISTRICT)) + 
  geom_text(aes(label=ifelse(Total.Crimes>1500,as.character(DISTRICT),'')),hjust=0.01,vjust=0) +
  xlab("District") +
  ylab("Crimes") + 
  ggtitle("Crimes per District") + 
  guides(size=FALSE,colour=FALSE) + 
  theme_classic() +
  theme(axis.text.x=element_blank(),axis.line.x = element_blank())

#Crimes per District focused on top ones
ggplot(totby_dis[totby_dis$Total.Crimes>1500,],aes(DISTRICT,Total.Crimes)) + 
  geom_point(aes(size=Total.Crimes,colour=Total.Crimes,label=st.dt)) + 
  geom_text(aes(label=ifelse(Total.Crimes>1500,as.character(st.dt),'')),hjust=0.01,vjust=0) +
  xlab("District") +
  ylab("Crimes") + 
  ggtitle("Crimes per District")   + 
  guides(size=FALSE,colour=FALSE) + 
  theme_classic() +
  theme(axis.text.x=element_blank())

#Top 20 Districts
dist = totby_dis %>% select(DISTRICT,Total.Crimes) %>% arrange(desc(Total.Crimes))
head(dist,20)
