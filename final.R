# load packages
library(tidyverse)
library(ggmap)
library(gridExtra)


# load data
jobs = read_csv("data/datafest2018.csv")
state_abbr <- read_csv("data/state_abbr.csv")

jobs_us = filter(jobs, country == "US")
jobs_tech <- filter(
  jobs_us,
  #grepl("finance", normTitleCategory),
  grepl("techinfo", normTitleCategory) | grepl("techsoftware", normTitleCategory)
  experienceRequired <= 1 | is.na(experienceRequired)
)

# maps
usa <- map_data("usa")
states <- map_data("state")

mean_sal <- mean(jobs_tech$estimatedSalary)
state_salaries <- jobs_tech %>%
  group_by(stateProvince) %>%
  summarize(
    med_salary = median(estimatedSalary),
    weighted_rel_dem = (n() / sum(clicks)) * (mean(estimatedSalary) / mean_sal),
    rel_dem = n() / sum(clicks)
  ) %>%
  left_join(state_abbr, by = c("stateProvince" = "abbr")) %>%
  select(region, med_salary, weighted_rel_dem, rel_dem) %>%
  na.omit()


ggplot() +
  geom_map(data=states, map=states, aes(x=long, y=lat, map_id=region), fill="#ffffff", color="#ffffff", size=0.15) +
  geom_map(data=state_salaries, map=states, aes(fill=weighted_rel_dem, map_id=region), color="#ffffff", size=0.15) +
  scale_fill_continuous(low='thistle2', high='darkred', guide='colorbar') +
  labs(
    x=NULL,
    y=NULL
  ) +
  theme(
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )


#grid.arrange(gg1, gg2, nrow = 2)

jobs_tech_intern <- filter(
  jobs_us,
  #grepl("^tech", normTitleCategory),
  grepl("intern", normTitle)|grepl("entry level", normTitle),
  experienceRequired <= 1 |is.na(experienceRequired)
)

supply_demand <- group_by(jobs_tech_intern, date) %>%
  summarise(clicks = sum(clicks), positions = n())

supply_demand_norm <- mutate(supply_demand,
                              date = date, clicks = clicks/max(clicks),
                              positions = positions/max(positions)
)

ggplot(data = supply_demand, mapping=aes(x=date,y=positions/clicks, colour=positions)) +
  geom_point()+
  geom_smooth(colour="red")+
  ylim(0,NA)+
  labs(x="Date",y="Job postings per click",colour="Number of \nopenings")+
  ggtitle("Supply-demand differentials for internships")+
  theme(plot.title = element_text(hjust = 0.5))
  