install.packages("tidyverse")
install.packages("viridis")
library(tidyverse)
library(readr)
library(ggplot2)
library(treemapify)
library(viridis)
EPL <- read_csv("EPL_20_21.csv")

View(EPL)

EPL <- transform(EPL, avg_goals = Goals/Matches)


EPL_1 <- EPL %>% 
  select(Name, avg_goals, xG) %>% 
  transform(performance = (avg_goals/xG))

head(EPL_1)

EPL_1[order(EPL_1[4], decreasing = TRUE),]

clean_data <- filter(EPL_1, performance <4.0, xG <1)
head(clean_data)

#xG line graph of players goals scored and xG to see who over performed their expected goals

ggplot(clean_data, aes(x=xG, y=avg_goals)) +
  geom_point() +
  geom_smooth() +
  geom_point(aes(color = performance)) +
  scale_color_gradient2(low = ("red"), mid = ("blue"), high = ("yellow"), midpoint =0.5, space ="Lab", na.value = "grey50", guide = "colorbar", aesthetics = "color") +
  geom_text(aes(label=ifelse(avg_goals>0.49,as.character(Name),'')),hjust=1,vjust=-.75)

goalscorers_only <- filter(EPL_1, avg_goals>0, performance<3.8) %>% 
  transform(performance= avg_goals/xG)

#filtered out players who hadn't scored a goal

ggplot(goalscorers_only, aes(x=xG, y=avg_goals)) +
  geom_point() +
  geom_smooth() +
  geom_point(aes(color = performance)) +
  scale_color_gradient2(low = ("red"), mid = ("blue"), high = ("yellow"), midpoint =0.5, space ="Lab", na.value = "grey50", guide = "colorbar", aesthetics = "color") +
  geom_text(aes(label=ifelse(avg_goals>0.49,as.character(Name),'')),hjust=1,vjust=-.1,cex=3)

#Create a treemap of player nationalities
 EPL_2 <- EPL %>% 
  count(Nationality, sort = TRUE)
ggplot(EPL_2, aes(area=n, label=n, fill=Nationality, subgroup=Nationality)) +
  geom_treemap() +
  geom_treemap_text(place = "center", color = "white", cex=8) + 
  geom_treemap_subgroup_text(place = "top", color = "white", reflow = T, cex=10,) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE, option = "D")

#Counted the number of players who played for each club
  EPL_3 <- EPL %>% 
  group_by(Club) %>% 
  count(Club, sort = TRUE)
ggplot(EPL_3, aes(area=n, label=n, fill=Club, subgroup=Club,)) +
  geom_treemap() +
  geom_treemap_text(place = "center", color= "black", cex=8) +
  geom_treemap_subgroup_text(place = "top", color ="black", cex=8)

#created a chart of age distribution in PL

df <- data.frame(range=c("under_20", "20_to_25", "26_to_30", "over_30"),
                 n=c(44, 220, 197, 71))                 
  head(df)
  
 ggplot(df, aes(x=range, y=n,)) +
    geom_bar(stat="identity", fill="blue") +
    geom_text(aes(label=n), vjust=3, color="white", size=3.5,)+
    scale_fill_viridis(discrete = TRUE)+
   ggtitle("Age Distribution In EPL") +
   theme_minimal()
 
#Number of goals scored by each team
 
EPL_4 <- EPL %>% 
  select(Club, Goals) %>% 
  group_by(Club)

ggplot(EPL_4, aes(x = Goals, y = Club, fill = Goals)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  ggtitle("Most Goals Scored By C") 


  
  
 

  


  




