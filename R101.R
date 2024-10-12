ages <- c(5, 6)
ages


names <- c("izu", "chi")

myname <- data.frame (names, ages)
myname

View(myname)
str(myname)

myname$names
myname$ages

myname[1,1]
myname[1,2]
myname[1, ]

data()
View(women)

library(tidyverse)

women %>%
  filter(height > 60 & weight < 130) %>% 
  mutate(body_mass_index = weight/((height/100)^2)) %>% 
  select(weight, body_mass_index) %>% 
  arrange(body_mass_index) %>% 
  #View()
  plot()

glimpse(women)
