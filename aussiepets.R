library(tidytuesdayR)
library(tidyverse)
library(lattice)
library(readr)
library(ggthemes)
library(ggsci)
library(rvest)
library(lubridate)
library(forecast)

tuesdata <- tidytuesdayR::tt_load('2020-07-21')
tuesdata <- tidytuesdayR::tt_load(2020, week = 30)

animal_outcomes <- tuesdata$animal_outcomes

#get suburb data
url <- "https://www.microburbs.com.au/heat-map/population-density#151.170674:-33.840703:13"
populations <- url %>%
  read_html() %>%
  html_table()
populations <- populations[[1]]



ani_out <- animal_outcomes %>% pivot_longer(4:12, names_to="state") %>% filter(state != "Total") 

ggplot(ani_out, aes(year, value, col=animal_type)) +
  geom_point() + facet_wrap(~outcome)

tuesdata$brisbane_complaints %>% 
  filter(animal_type=="Dog", date_range %in% c("jan-mar-2019.csv","apr-jun-2019.csv","jul-to-sep-2019.csv","cars-srsa-open-data-animal-related-complaints-oct-to-dec-2019.csv" )) %>%
  group_by(animal_type, category, suburb) %>%
  tally() %>% filter(!is.na(category)) %>%
  ggplot(aes(reorder(category, n), n, col=animal_type)) + geom_point() + coord_flip() +
  facet_wrap(~animal_type, scales="free")

ani_complaint <- tuesdata$animal_complaints %>% 
  mutate(year=parse_number(`Date Received`)) %>%
  group_by(year, `Animal Type`, `Complaint Type`, Suburb) %>% tally()

ani_complaint_d <- tuesdata$animal_complaints %>%
  mutate(date=dmy(paste("01 ",`Date Received`, sep=""))) %>%
  group_by(date, `Animal Type`, `Complaint Type`, Suburb) %>% tally() %>%
  pivot_wider(names_from=`Animal Type`, values_from = n) 

ani_complaint_date <- tuesdata$animal_complaints %>%
  mutate(year = parse_number(`Date Received`), month=str_extract(`Date Received`, "[A-Za-z]+" )) %>%
  mutate(month = factor(month, levels = c("January", "February", "March", "April", "May", "June",
                                       "July", "August", "September", "October", "November", "December") )) %>%
  group_by(year, month, `Animal Type`, `Complaint Type`) %>% tally()

forecast_ani <- ani_complaint_date %>% filter(year != 2020) %>%
  group_by(month, `Animal Type`, `Complaint Type`) %>%
  summarise(mean_incident = mean(n, na.rm=TRUE), sd_incident=sd(n, na.rm=TRUE)) %>%
  left_join(ani_complaint_date %>% filter(year==2020) %>% select(-year), 
            by=c("month","Animal Type", "Complaint Type")) %>%
  mutate(z_incident= (n-mean_incident)/sd_incident)

predict_2020 <- forecast_ani %>% group_by(`Animal Type`, `Complaint Type`) %>%
  summarise(mean_z = mean(z_incident, na.rm=T)) %>% 
  left_join(forecast_ani, by = c("Animal Type", "Complaint Type")) %>%
  mutate(predict_incident=mean_incident+sd_incident*mean_z) %>%
  select(year, month, `Animal Type`, `Complaint Type`, n = predict_incident) %>%
  filter(month %in% c("July", "August", "September", "October", "November", "December")) %>%
  mutate(year=2020, predicted=1)

ggplot(ani_complaint %>% filter(n<300), aes(year, n, col=year)) + 
  geom_line() + facet_grid(`Animal Type`~`Complaint Type`) +
  theme_few() + scale_fill_nejm()

ani_complaint_date[ani_complaint_date$year == 2020,] <- predict_2020$predict_incident

ggplot(ani_complaint_date %>% filter(year !=2020), aes(month, n, col=as.factor(year),
                                                 group=as.factor(year))) + 
  geom_line(alpha=0.7) + 
  geom_line(data=ani_complaint_date %>% 
              mutate(predicted=0) %>% bind_rows(predict_2020) %>%
              filter(year==2020, predicted==0), aes(month, n, col=as.factor(year),
                                         group=as.factor(year)), col="#DE1738", size=2) +
  geom_line(data=ani_complaint_date %>% 
              mutate(predicted=0) %>% bind_rows(predict_2020) %>%
              filter(year==2020, predicted==1), aes(month, n, col=as.factor(year),
                                                    group=as.factor(year)), 
             linetype=3, col="#DE1738", size=2, alpha=0.8) +
  facet_grid(~`Animal Type`~`Complaint Type`, scales="free_y") +
  theme_few() + scale_color_brewer(palette="Greys") + 
  theme(axis.text.x = element_text(angle=90),
        text= element_text(family="Avenir"),
        plot.title = element_text(family="Futura Medium", size = 30),
        legend.position = "none") +
  labs(x="", y="", title = "Complaining cats and dogs", 
         subtitle="Dog and cat complaints in Brisbane - 2020 compared to previous years",
       caption="Source: RSPCA")
  

ani_ts <- ts(ani_complaint_d[,"n"], frequency=12, start=c(2013,10))
autoplot(ani_ts)

ggseasonplot(ani_ts %>% select(date,))




