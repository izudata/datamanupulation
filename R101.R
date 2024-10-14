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


# weight in pounds is converted to kg and hieght in inches is converted to m
women %>%
  filter(height > 60 & weight < 130) %>% 
  mutate(body_mass_index = (weight * 0.453592) / ((height * 0.0254)^2)) %>% 
  select(weight, body_mass_index) %>% 
  arrange(body_mass_index) %>% 
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

# returns all columns
starwars %>% 
  filter(startsWith(sex, "m"))

# returns only two columns name and sex
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

# filter rows
starwars %>% 
  select (mass, sex) %>% 
  filter(mass < 55 &
           sex == "male")

# Recode data
starwars %>% 
  select (sex) %>% 
  mutate(sex = recode(sex, "male" = "man",
                      "female" = "woman"))

# Dealing with missing data
mean(starwars$height, na.rm = TRUE)

# Dealing with duplicates
Students_Names <- c("Peter", "John", "Andrew", "Peter")
Students_Ages <- c(22, 33, 44, 22)
students <- data.frame(Students_Names, Students_Ages)
students

students %>% 
  distinct()

distinct(students)

# Data manipulation
women %>% 
  select (height) %>% 
  mutate(tallness = 
           if_else(height < 60,
                   "short",
                   "tall"))

 # Reshaping data with Pivot wider
data <- select(gapminder, country, year, lifeExp)
View(data)


wide_data <- data %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

View(wide_data)


# Reshaping data with Pivot longer
long_data <- wide_data %>% 
  pivot_longer(2:13,
               names_to = "year",
               values_to = "lifeExp")

View(long_data)

# Describing your data

min(msleep$sleep_total)
max(msleep$sleep_total)
range(msleep$sleep_total)
# Interquartile range
IQR(msleep$sleep_total)

# Centrality 
mean(msleep$sleep_total)
median(msleep$sleep_total)


# Variance
var(msleep$sleep_total)

summary(msleep$sleep_total)

msleep %>% 
  select (awake, sleep_total) %>% 
  summary()
