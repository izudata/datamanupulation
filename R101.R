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

head(starwars)
class(starwars$skin_color)
length(starwars)
names(starwars)
unique(starwars$eye_color)

na_rows <- rowSums(is.na(starwars)) > 0
rows_with_na <- starwars[na_rows, ]
rows_with_na
print(rows_with_na, n = 58)

starwars %>% 
  select (name, skin_color, eye_color)

starwars %>% 
  select (name, skin_color, eye_color) %>% 
  print(n=20)

starwars %>% 
  select (2:3)

starwars %>% 
  select (ends_with("color"))

starwars %>% 
  filter(startsWith(sex, "m"))

starwars %>% 
  select(name, sex) %>% 
  filter(startsWith(sex, "m"))

# renaming a column aka variable
women %>% 
  rename ("mass" = "weight") %>% 
  head()


women %>% 
  head()

# changing a data type to a 'factor'
starwars$hair_color <-as.factor(starwars$hair_color)
class(starwars$hair_color)

# changing a data type with a tidyverse 
starwars %>% 
  mutate(hair_color = as.character(hair_color)) %>% 
  glimpse()


# changing factor levels
df <- starwars
df$sex <- as.factor(df$sex)
levels(df$sex)

# use force levels to return factor in a desired order

df <- df %>%
  mutate(sex = factor(sex,
                       levels = c("male", "female",  "hermaphroditic", "none" )))
levels(df$sex)
