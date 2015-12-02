require("dplyr")
require("ggplot2")
climbs <- read.csv("ClimbTracker.csv", header=T, sep=",", as.is=T)

climbs$Percentage.Complete <- climbs$Percentage.Complete / 10
climbs$Date <- format(strptime(climbs$Timestamp, format = "%m/%d/%Y %H:%M:%S", tz="PST"), "%Y-%m-%d")
head(climbs)

#Climbs per session
climbs_per_session <- {climbs %>%
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
    group_by(Route.Location) %>%
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
    group_by(Route.Scale, Route.Grade, Percentage.Complete) %>%
    summarize(
      Climbs = n()
    )}

ggplot(data = success_fail_by_grade, aes(x = Route.Grade, y=Climbs, fill=Percentage.Complete)) + geom_bar(stat="identity") + facet_wrap("Route.Scale", scale="free")