require("dplyr")
require("ggplot2")
climbs <- read.csv("ClimbTracker.csv", header=T, sep=",", as.is=T)

climbs$Percentage.Complete <- climbs$Percentage.Complete / 10
climbs$Date <- format(strptime(climbs$Timestamp, format = "%m/%d/%Y %H:%M:%S", tz="PST"), "%Y-%m-%d")
climbs$Hour <- format(strptime(climbs$Timestamp, format = "%m/%d/%Y %H:%M:%S", tz="PST"), "%H")

#Melted Data
climbs_per_session <- {climbs %>%
  group_by(Date, Route.Location, Route.Grade) %>%
  summarize(
    Climbs = n(),
    Sends = sum(ifelse(Percentage.Complete == 1.0, 1, 0))
    )}

climbs_by_day <- {climbs %>% 
    group_by(Date) %>% 
    summarize(
      Climbs = n(),
      Sends = sum(ifelse(Percentage.Complete == 1.0, 1, 0))
      )}

climbs_per_vgrade <-
{subset(climbs, Route.Scale == 'V') %>%
    group_by(Route.Grade) %>%
    summarize(
      Climbs = n(),
      Sends = sum(ifelse(Percentage.Complete == 1.0, 1, 0))
    )}

climbs_per_gym <- {climbs %>%
    group_by(Route.Location, Route.Grade) %>%
    summarize(
      Climbs = n(),
      Sends = sum(ifelse(Percentage.Complete == 1.0, 1, 0))
    )}

climbs_per_wall <- {climbs %>%
    group_by(Wall.Type) %>%
    summarize(
      Climbs = n(),
      Sends = sum(ifelse(Percentage.Complete == 1.0, 1, 0))
    )}

success_fail_spot <- {climbs %>%
    group_by(Percentage.Complete) %>%
    summarize(
      Climbs = n()
    )}

success_fail_by_grade <- {climbs %>%
    group_by(Route.Location, Route.Scale, Route.Grade, Percentage.Complete) %>%
    summarize(
      Climbs = n()
    )}

success_fail_by_hour <- {climbs %>%
    group_by(Hour, Route.Grade) %>%
    summarize(
      Climbs = n(),
      Sends = sum(ifelse(Percentage.Complete == 1.0, 1, 0))
    )}

data_melt <- {climbs %>%
    group_by(Date,Route.Grade) %>%
    summarize(
      Climbs = n(),
      Sends = sum(ifelse(Percentage.Complete == 1.0, 1, 0))
    )}

#Summary Stats
total_days <- dim(climbs_by_day)[1]
avg_climbs <- mean(climbs_by_day$Climbs)
avg_sends <- mean(climbs_by_day$Sends)
total_climbs <- dim(climbs)[1]
total_gyms <- length(unique(climbs_per_gym$Route.Location))
max_session <- max(climbs_by_day$Climbs)
max_sends <- max(climbs_by_day$Sends)

#Plots
plot_successfail_grade <- ggplot(data = success_fail_by_grade, aes(x = Route.Grade, y=Climbs, fill=Percentage.Complete)) + 
  geom_bar(stat="identity") + 
  facet_wrap("Route.Location", scale="free") +
  labs(x = "Grade Climbed", y = "Total Climbs", title = "Success/Fail by Grade")

plot_successfail_hour <- ggplot(data = success_fail_by_hour) + 
  geom_bar(aes(x = Hour, y=Climbs, fill=Sends, alpha = Route.Grade ), stat="identity") +
  labs(x = "Hour", y = "Total Climbs", title = "Climbs by Hour")

plot_climbs_day <- ggplot(data = climbs_per_session, aes(x=as.Date(Date), y=Climbs, fill = Route.Grade)) + 
  geom_bar(stat="identity") + 
  labs(x = "Date", y = "Total Climbs", title = "Climbs by Day")

plot_climbs_gym <- ggplot(data = climbs_per_gym, aes(x=Route.Location, y = Climbs, fill=Route.Grade)) + 
  geom_bar(stat="identity") + 
  labs(x = "Date", y = "Total Climbs", title = "Climbs by Gym")

plot_climbs_vgrade <- ggplot(data = climbs_per_vgrade, aes(x=Route.Grade, y = Climbs, fill = Sends/Climbs)) +
  geom_bar(stat="identity") +
  labs(x = "Grade", y = "Total Climbs", title = "Climbs by Grade")