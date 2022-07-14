
file.choose()


weekthemes <- read.csv("GBBO_week_themes.csv")

gbbo_all <- read.csv("GBBO_allseasons_episodes_and_status_022422.csv")

gbbo_lat_long = read.csv("GBBO_contestants_lat_long_042622.csv")

library(dplyr)
library(ggplot2)


#Example heat map for baker age by season and episode

gbbo_lat_long <- gbbo_lat_long %>%
  mutate(baker.season = paste(firstname,'_',season))

gbbo_all <- gbbo_all %>%
  mutate(baker.season = paste(baker,'_',season))

colorpal <- colorRampPalette(c('lightsalmon','royalblue'))(5)

gbbo_all %>%
  merge(gbbo_lat_long, by = 'baker.season') %>%
  group_by(season.x,episode) %>%
  summarize(avg.age = mean(age)) %>%
  mutate(agegroup = case_when(avg.age <= 30  ~ '<=30',
                         avg.age > 30 & avg.age < 36 ~ '31-35',
                         avg.age > 36 & avg.age <= 40 ~ '36-40',
                         avg.age > 40 & avg.age <= 46 ~ '40-45',
                         TRUE ~ '46+')) %>%
  ggplot(aes(as.factor(episode),as.factor(season.x), fill = agegroup)) +
  geom_tile(color = "white",
            lwd = .5,
            linetype = 1) +
  #scale_fill_gradient(low = "pink", high = "brown", name = "Avg. Age") +
  geom_text(aes(label = round(avg.age,0)), color="white", size=rel(4)) +
  xlab("Episode") + 
  ylab("Season") +
  ggtitle("Average Baker Age by Season and Episode") +
  theme_minimal() +
  theme(panel.grid.major = element_blank()) +
  scale_fill_manual(values = colorpal, name = "Age Group") 
  



#Final Week themes chart

mycolors <- colorRampPalette(c('#fa8ca9','#ffdbfa','lightgoldenrod','#cce0ff',"#d4b7a9"))(12)

weekthemes %>%
  group_by(category) %>%
  summarize(avgweek = mean(week)) %>%
  mutate(ranking = rank(avgweek, ties.method = 'first')) %>%
  inner_join(weekthemes) %>%
  mutate(week_theme = factor(week_theme, levels= unique(week_theme[order(desc(ranking))]))) %>%
  mutate(season = as.factor(season)) %>%
  
  ggplot(aes(season, week_theme, fill=category)) + 
  geom_tile(color = 'gray20', size = .5) +
  scale_fill_manual(values = mycolors, name = "Category") +
  scale_x_discrete(position = "top",
                   labels=c("2" = "S2", "3" = "S3",
                            "4" = "S4", "5" = "S5",   
                            "6" = "S6", "7" = "S7", 
                            "8" = "S8", "9" = "S9", 
                            "10" = "S10", "11" = "S11", 
                            "12" = "S12")) +   
  labs(color = "Category") +
  geom_text(aes(label = week), color="black", size=rel(5)) +
  xlab("") + 
  ylab("") +
  ggtitle("Great British Bake Off Week Themes Season by Season Comparison") +
  theme(panel.grid.major.y = element_line(color = "#edd99f"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_line(),
        panel.border = element_rect(fill=NA,color="white", size=0.5, linetype="solid"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill="white"),
        plot.background = element_rect(fill="white"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        title = element_text(size =14),
        axis.text = element_text(color="black", size=14))

)
  
  
  
  
  