#Calculating avg daily steps of users

daily_average <- daily_activity.sleep %>%
  group_by(id) %>%
  summarise (mean_daily_steps = mean(total_steps), mean_daily_calories = mean(calories),
             mean_daily_sleep = mean(total_minutes_asleep))
head(daily_average)

#Classsifying and labeling based on  daily steps
user_type_steps <- daily_average %>%
  mutate(user_type = case_when(
    mean_daily_steps < 5000 ~ "sedentary",
    mean_daily_steps >= 5000 & mean_daily_steps < 7499 ~ "lightly active", 
    mean_daily_steps >= 7500 & mean_daily_steps < 9999 ~ "fairly active", 
    mean_daily_steps >= 10000 ~ "very active"
  ))
head(user_type_steps)
#Visualizing types of users with boxplot
ggplot(data = user_type_steps) +
  geom_boxplot(mapping = aes(x = user_type, y = mean_daily_steps, fill = user_type),
               outlier.color = "blue", outlier.shape = 8) +
  coord_cartesian(ylim = c(2000,12000)) + labs(title = "Summary of User Types",
                                               y = "Average Daily Steps",
                                               x = "User Type") + theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
                                                                        axis.text.x=element_text(size=12), 
                                                                        axis.text.y=element_text(size=10),
                                                                        axis.title.x=element_text(size=15),
                                                                        axis.title.y=element_text(size=15),                                                                        legend.key.size=unit(1.2,"cm"))
options(repr.plot.width = 8,
        repr.plot.height = 8)

#Percentage of each usertype
steps_percent <- user_type_steps %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

steps_percent$user_type <- factor(steps_percent$user_type , levels = c("very active", "fairly active", "lightly active", "sedentary"))

head(steps_percent)

#Percentage visualization in pie chart
pie <- ggplot(data = steps_percent, mapping = aes(x = "", y = total_percent, fill = user_type)) +
  geom_bar(stat = "identity")+
  coord_polar("y")+
  theme_void()+
  labs(title="User type distribution") + 
  theme(plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        legend.text = element_text(size = 10)) +
  scale_fill_discrete(name = "User Types") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))

pie + scale_fill_brewer("Pastel1") + theme_minimal()

#Sleep patterns

#Categorising by Sleep
user_type_sleep <- daily_average %>%
  mutate(user_type = case_when(
    mean_daily_sleep >= 420 ~ "sufficient sleep",
    mean_daily_sleep < 420 ~ "insufficient sleep"
  ))

#Verifying
head(user_type_sleep)

#Calculating Percentage
sleep_percent <- user_type_sleep %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))

sleep_percent$user_type_sleep <- factor(sleep_percent$user_type , levels = c("sufficient sleep", "insufficient sleep"))

#Verifying
head(sleep_percent)

#Visualizing sleep data
ggplot(data = sleep_percent, mapping = aes(x = "", y = total_percent, fill = user_type)) +
  geom_bar(stat = "identity")+
  coord_polar("y")+
  theme_void()+
  labs(title="Proportion of users by sleep") + 
  theme(plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        legend.key.size=unit(0.8,"cm")) +
  scale_fill_manual(name = "User Types",
                    values = c("#00F3FF","#0019FF")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))
options(repr.plot.width = 7,
        repr.plot.height = 6)

#Correlation between Steps, Sleep and Calories
daily_activity.sleep %>%
  mutate(activity_level = case_when(
    total_steps < 5000 ~ "sedentary",
    total_steps >= 5000 & total_steps < 7499 ~ "lightly active", 
    total_steps >= 7500 & total_steps < 9999 ~ "fairly active", 
    total_steps >= 10000 ~ "very active"), 
    sleep_type = case_when( total_minutes_asleep >= 420 ~ "sufficient sleep",
                            total_minutes_asleep < 420 ~ "insufficient sleep")) %>% 
  ggplot(mapping = aes(x = calories, y = total_steps)) + 
  geom_jitter(aes(color = activity_level, shape = sleep_type), size = 2.5) + 
  geom_smooth(method = 'loess',formula = 'y ~ x',se = FALSE) + labs(title = "Correlation: Steps vs. Calories",
       subtitle = "Relationship between steps/day and calories burned per day", 
       x = "Calories Burned", 
       y = "Steps/Day")+
  guides(color = guide_legend("Activity Level"), 
         shape = guide_legend("Sleep Type")) + theme_minimal()  +
  theme(plot.title=element_text(size=21, face="bold"),
        plot.subtitle=element_text(size=15),
        axis.text.x=element_text(size=12), 
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=18),
        axis.title.y=element_text(size=18),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(size = 11)) 

options(repr.plot.width = 9,
        repr.plot.height = 10)


#Classyfing by usage of smart device

users_byid <- daily_activity.sleep %>%
  group_by(id) %>%
  summarize(days_used=sum(n())) %>%
  mutate(usage = case_when(
    days_used >= 1 & days_used <= 10 ~ "low use",
    days_used >= 11 & days_used <= 20 ~ "moderate use", 
    days_used >= 21 & days_used <= 31 ~ "high use", 
  ))

head(users_byid)

users_percent <- users_byid %>%
  group_by(usage) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(usage) %>%
  summarise(total_percent = (total / totals)*100)

users_percent$usage <- factor(users_percent$usage, levels = c("high use", "moderate use", "low use"))

head(users_percent)

# Computing the cumulative percentages (top of each rectangle)
users_percent$ymax = cumsum(users_percent$total_percent)

# Computing the bottom of each rectangle
users_percent$ymin = c(0, head(users_percent$ymax, n=-1))

# Computing label position
users_percent$labelPosition <- (users_percent$ymax + users_percent$ymin) / 2

# Computing label
users_percent$label <- paste0(users_percent$usage,
                              "\n value: ", users_percent$total_percent)
#Plot
ggplot(users_percent, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=usage)) +
  geom_rect() +
  geom_label( x=2, aes(y=labelPosition, label=label), size=6) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  scale_fill_manual(values = c("#00FFBC","#07A978","#A2FF00"),
                    labels = c("High use ",
                               "Moderate use ",
                               "Low use"))+
  labs(title="Daily use of smart device") + 
  theme(plot.title=element_text(size=20, face="bold"),
        legend.key.size=unit(1,"cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))

options(repr.plot.width = 10,
        repr.plot.height = 10)

#Usage of smart device by minutes

daily_use_merged <- merge(daily_activity, users_byid, by=c ("id"))
head(daily_use_merged)

minutes_worn <- daily_use_merged %>% 
  mutate(total_minutes_worn = very_active_minutes+fairly_active_minutes+lightly_active_minutes+sedentary_minutes)%>%
  mutate (percent_minutes_worn = (total_minutes_worn/1440)*100) %>%
  mutate (worn = case_when(
    percent_minutes_worn == 100 ~ "All day",
    percent_minutes_worn < 100 & percent_minutes_worn >= 50~ "More than half day", 
    percent_minutes_worn < 50 & percent_minutes_worn > 0 ~ "Less than half day"
  ))

head(minutes_worn)
