
library(tidyverse)
library(nycflights13)

airlines
airports
planes
weather

planes%>%
  count(tailnum)%>%
  filter(n>1)
weather%>%
  count(year,month,day,hour,origin)%>%
  filter(n>1)
flights %>% 
  count(year, month, day, flight) %>% 
  filter(n > 1)
flights %>% 
  count(year, month, day, tailnum) %>% 
  filter(n > 1)


?row_number

x <- c(5, 1, 3, 2, 2, NA)
row_number(x)
x
min_rank(x)
y <- c(4,7,2,0.4,1,98)
row_number(y)

view(Lahman::Managers)
view(Lahman::AwardsManagers)
view(Lahman::Salaries)
babynames::babynames
install.packages("babynames")
nasaweather::atmos
install.packages("nasaweather")
fueleconomy::vehicles
install.packages("fueleconomy")
diamonds
?babynames
?atmos
Batting



flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, carrier)
flights2
airlines
flights2%>%
  select(-origin,-dest)%>%
  left_join(airlines, by="carrier")


flights2%>%
  select(-origin, -dest)%>%
  mutate(name=airlines$name[match(carrier, airlines$carrier)])
view(weather)
flights2%>%
  mutate(temp_dest=weather$temp[match(origin, weather$origin)])%>%
  tail
flights2%>%
  left_join(weather, by="origin")

x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  3, "x3"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  4, "y3"
)  
x
y
x%>%
  inner_join(y, by = "key")
x%>%
  left_join(y)
x%>%
  right_join(y)
x%>%
  full_join(y)
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  1, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2"
)
left_join(x, y, by = "key")
x
y
x <- tribble(
  ~key, ~val_x,
  1, "x1",
  2, "x2",
  2, "x3",
  3, "x4"
)
y <- tribble(
  ~key, ~val_y,
  1, "y1",
  2, "y2",
  2, "y3",
  3, "y4"
)
left_join(x, y, by = "key")

flights2 %>% 
  left_join(weather)

flights2 %>%
  left_join(planes, by = "tailnum")
view(airports)
flights2 %>%
  left_join(airports, by = c("dest"="faa"))

flights2%>%
  left_join(airports, c("origin"="faa"))

airports %>%
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon, lat)) +
  borders("state", colour="yellow", fill="lightblue") +
  geom_point(color="blue") +
  coord_quickmap()
view(flights2)
flights3 <- flights %>%
  select(year:day, dest, tailnum, arr_delay)%>%
  group_by(dest)%>%
  summarise(average_delay = mean(arr_delay[arr_delay>0], na.rm = TRUE))
view(flights)
airports2 <- airports %>%
  left_join(flights3, by = c("faa"="dest"))%>%
  filter(!is.na(average_delay))

airports2%>%
  ggplot(aes(lon,lat, color = average_delay, size=average_delay,label=faa))+
  borders("state", colour = "lightblue", fill = "white")+
  geom_point()+
  geom_text_repel(data=subset(airports2, average_delay > 30))

library(ggrepel)
  coord_quickmap()
  
  install.packages("ggrepel")

?geom_label
airports2





library(tidyverse)
library(nycflights13)
?geom_point

view(airports)

lon_lat <- airports %>%
  select(faa, lon, lat)

?left_join
view(flights)
flights2 <- flights%>%
  left_join(lon_lat, by = c("origin"="faa"))%>%
  select(year:origin, lon, lat, dest:time_hour)%>%
  rename("lon_origin"=lon, "lat_origin"=lat)
flights2 <- flights2 %>%
  left_join(lon_lat, by = c("dest"= "faa"))%>%
  select(year:dest, lon, lat:time_hour)%>%
  rename("lon_dest"=lon, "lat_dest"=lat)

planes2 <- planes %>%
  select(tailnum, year)

flights2 <- flights2 %>%
  left_join(planes2, by="tailnum")%>%
  rename("plane_built"=year.y)
flights2 <- flights2%>%
  select(year.x:plane_built)%>%
  rename("year" = year.x)
flights3 <- flights2%>%
  group_by(tailnum)%>%
  summarise(average_delay=mean(arr_delay[arr_delay>0], na.rm = TRUE))
flights3 <- flights3 %>%
  left_join(planes2, by="tailnum")

flights3 <- flights3%>%
  filter(!is.na(year))

ggplot(data = flights3, mapping=aes(x=year, y=average_delay,color=average_delay))+
  geom_point()+
  geom_smooth(se=FALSE)


view(flights) 
naflights <- flights%>%
  filter(is.na(tailnum))

flights100 <- flights %>%
  count(tailnum) %>%
  filter(n >= 100, !is.na(tailnum))

flights%>%
  semi_join(flights100)

library(fueleconomy)

view(vehicles)
view(common)

vehicles2 <- vehicles %>%
  semi_join(common, by=c("make","model"))

super_delay <- flights%>%
  group_by(month, day)%>%
  summarise(average_delay=mean(arr_delay[arr_delay>0], na.rm = TRUE))%>%
  filter(average_delay>60)
flights_delay <- weather%>%
  semi_join(super_delay)

view(flights)


fl <- anti_join(flights, airports, by = c("dest" = "faa"))
airports%>%
  filter(faa == "STT")
anti_join(airports, flights, by = c("faa" = "dest"))

carrier_plane <- flights%>%
  select(tailnum, carrier)

carrier_plane%>%
  inner_join(airlines)

df1 <- tribble(
  ~x, ~y,
  1,  1,
  2,  1
)
df2 <- tribble(
  ~x, ~y,
  1,  1,
  1,  2
)

intersect(df1, df2)
union(df1, df2)
setdiff(df1,df2)
setdiff(df2,df1)


string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

double_quote <- "\""
double_quote
single_quote <- '\''
single_quote

x <- "\u00b5"
x
c("one","two","three")
str_length(c("a", "R for data science", NA))
str_c("aghdv","ijgiojv", "kd2]lc")
str_c("aghdv","ijgiojv", "kd2]lc", sep = "    ,   ")

x <- c("abc", NA)
str_c("|-", x, "-|")
str_c("|-", str_replace_na(x), "-|")


str_c("prefix-", c("a", "b", "c"), "-suffix")


name <- "Hadley"
time_of_day <- "morning"
bithday <- FALSE

str_c(
  "Good ", time_of_day," ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  ".")
str_c(c("x", "y", "z"), collapse = ", ")

x <- c("Apple", "Banana", "Pear")
str_sub(x,1,3)
str_sub(x,-3,-1)

str_sub(x,1,1) <- str_to_lower(str_sub(x,1,1))
x

str_to_upper(c("??", "??", "??", "??", "??"))
z <- c(" ??", " ??", "??", "??", "?? ")

?paste
str_length(z)
str_sub(z)
str_sub(z)
str_wrap(z)
?str_trim
str_trim(z)
y <- c("a", "b", "c")
str_c(c("a", "b", "c"))
str_length(y)
str_sub(y)
x <- c("apple", "banana", "pear")
str_view(x, "an")
install.packages("htmlwidgets")
library(htmlwidgets)
str_view(x, ".a.")



dot <- "\\."
dot

writeLines(dot)
?writeLines
str_view(c("abc", "a.c","bef"), "a\\.c")
writeLines(c("abc", "a.c","bef"), "a\\.c")

x <- "a\\b"
writeLines(x)
str_view(x, "a\\\\b")
x <- "1993\\02\\08"
x
writeLines(x)
str_view(x, "^19")

x <- c("apple", "banana", "pear")
str_view(x, "^ba")
str_view(x, "ar$")


x <- c("apple pie", "apple", "apple cake")
str_view(x, "\bsum\b")
q <- "$^$"
q
str_view(q, "^$^")
words
y <- str_view(words, "^y")
y

words%>%
  str_view("^y")%>%
  writeLines()

str_view(words, "^y", match = TRUE)
str_view(words, "x$", match = TRUE)

str_view(words, "^...$", match = TRUE)
str_view(words, "^.......", match=TRUE)
?str_view
