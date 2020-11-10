library(tidyverse)
library(nycflights13)
ggplot(mpg) + geom_point(mapping = aes(x=drv, y=hwy))
?mpg

flights_sml <- select(flights,
                      year:day,
                      ends_with("delay"),
                      distance,
                      air_time)
gain_speed <- mutate(flights_sml,
       gain=dep_delay-arr_delay,
       speed=distance/air_time * 60)
view(gain_speed)

gain_per_hour <- mutate(flights_sml,
                        gain=dep_delay-arr_delay,
                        hours=air_time/60,
                        gain_per_hour=gain/hours)
gain_per_hour <- transmute(flights,
                  gain=dep_delay-arr_delay,
                  hours=air_time/60,
                  gain_per_hour=gain/hours)

transmute(flights, 
          dep_time,
          hour=dep_time %/% 100,
          minute=dep_time %% 100)

t <- c(9303, 701,5882, 920, 509)
t
data.frame(t)
m <- data.frame(t)
m
m$x <- c(4011,343,1230,112,928)
m
transmute(m,
          t,x,
          x1=x %/% 100,
          x2=x %% 100)
x <- c(1:10)
cumsum(x)
cummean(x)

cummin(x)
x
cummax(x)
cumprod(x)

factorial(x)

min_rank(x)
dense_rank(x)
dense_rank(m)
dense_rank(t)
row_number(t)
min_rank(t)
percent_rank(t)

select(flights, dep_time, sched_dep_time)
practice <- transmute(flights,
          dep_time_hour=dep_time %/% 100 ,
          dep_time_minute=dep_time %% 100,
          sched_dep_time_hour=sched_dep_time %/% 100,
          sched_dep_time_minute=sched_dep_time %% 100)

practice <- arrange(practice, dep_time_hour, dep_time_minute, sched_dep_time_hour, sched_dep_time_minute)

pr <- transmute(flights, 
          air_time,
          real_air_time=arr_time-dep_time,
          hour_arr=arr_time %/% 100,
          min_arr=arr_time %% 100,
          hour_dep=dep_time %/% 100,
          min_dep=dep_time %% 100,
          true_time=(hour_arr*60+min_arr) - (hour_dep*60+min_dep),
          )
view(pr)

delay <- select(flights, dep_time, sched_dep_time, dep_delay)
view(delay)
?min_rank
min_rank(flights$dep_delay, 10)

ntile(flights$dep_delay, 10)
 
mutate(flights, row_number(dep_delay) == 10)


library(tidyverse)
library(nycflights13)

?min_rank

d <- arrange(flights, desc(dep_delay))
d <- head(d, n=10)
?head
min_rank(d$dep_delay)
1:3+1:10
q <- c(1:3)
w <- c(1:10)
q+w
q
w

summarise(flights, delay=mean(dep_delay, na.rm = TRUE))
by_day <- group_by(flights, year,month,day)
summarise(by_day, delay=mean(dep_delay, na.rm = TRUE))
by_month <- group_by(flights, year,month)
summarise(by_month, delay=mean(dep_delay, na.rm = TRUE))
by_dest <- group_by(flights, dest)
delay <- summarise(by_dest,
                   count=n(),
                   dist=mean(distance, na.rm = TRUE),
                   delay=mean(arr_delay, na.rm = TRUE))
delay
delay <- filter(delay, count>20, dest!="HNL")
delay
ggplot(data = delay, mapping = aes(x=dist, y=delay))+
  geom_point(aes(size=count), alpha=1/3)+
  geom_smooth(se=FALSE)

delays <- flights %>%
  group_by(dest) %>%
  summarise(
    count=n(),
    dist=mean(distance,na.rm = TRUE),
    delay=mean(arr_delay, na.rm = TRUE)
  )%>%
  filter(count>20, dest!="HNL")
view(delays)
not_cancelled <- flights%>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled%>%
  group_by(year,month,day)%>%
  summarise(mean=mean(dep_delay))

delays <- not_cancelled%>%
  group_by(tailnum)%>%
  summarise(delay=mean(arr_delay))
ggplot(data = delays, mapping = aes(x=delay))+
  geom_freqpoly(binwidth=10)
delays <- not_cancelled %>%
  group_by(tailnum)%>%
  summarise(
    delay=mean(arr_delay,na.rm = TRUE),
    n=n()
  )
ggplot(data = delays, mapping = aes(x=n, y=delay))+
  geom_point(alpha=1/10)

delays %>%
  filter(n>25) %>%
  ggplot(mapping = aes(x=n, y=delay))+
  geom_point(alpha=1/10)
install.packages("Lahman")
library(Lahman)
batting <- as_tibble(Lahman::Batting)

batters <- batting %>%
  group_by(playerID)%>%
  summarise(
    ba=sum(H, na.rm = TRUE)/sum(AB, na.rm = TRUE),
    ab=sum(AB, na.rm = TRUE)
  )
view(batters)

batters %>%
  filter(ab>100) %>%
  ggplot(mapping = aes(x=ab, y=ba))+
  geom_point()+
  geom_smooth(se=FALSE)

not_cancelled %>%
  group_by(year,month,day) %>%
  summarise(
    avg_delay1=mean(arr_delay),
    avg_delay2=mean(arr_delay[arr_delay>0])
  )

x
min(x)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )
not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))

not_cancelled %>%
  group_by(dest) %>%
  summarise(carriers=n_distinct(carrier))%>%
  arrange(desc(carriers))
not_cancelled %>%
  count(tailnum, wt=distance)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))

# What proportion of flights are delayed by more than an hour?
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_prop = mean(arr_delay > 60))

daily <- group_by(flights, year,month,day)

(per_day <- summarise(daily, flights = n()))

(per_month <- summarise(per_day, flights = sum(flights)))
per_day


per_year <- summarise(per_month, flights = sum(flights))
view(per_year)
per_year

daily %>% 
  ungroup() %>%             # no longer grouped by date
  summarise(flights = n())  # all flights

library(tidyverse)
library(nycflights13)

flights %>%
  group_by(tailnum)%>%
  filter(dep_delay == 10)

early_late <- flights %>%
  select(year,month,day,ends_with("delay"), tailnum)
  
early_late <- early_late%>%
  filter(dep_delay == 15&-15)

library(tidyverse)
library(nycflights13)
  
early_late %>%
  filter(dep_delay == -15)
view(early_late)

early_late <- flights %>%
  filter(arr_delay == -15)
view(early_late)

early <- flights %>%
  select(tailnum, arr_delay) %>%
  filter(arr_delay == -15)

early <- early %>%
  group_by(tailnum)%>%
  summarise(n_early=n())%>%
  arrange(desc(n_early))

late <- flights %>%
  select(tailnum, arr_delay) %>%
  filter(arr_delay == 15)
late <- late %>%
  group_by(tailnum)%>%
  summarise(n_late = n())%>%
  arrange(desc(n_late))
early_late <- merge(early,late, by="tailnum")

early_late <- early_late %>%
  filter(n_early==n_late)%>%
  arrange(desc(n_early))

n_flights <- select(flights,tailnum)
n_flights <- n_flights %>%
  group_by(tailnum)%>%
  summarise(n=n())

total <- merge(early, n_flights, by="tailnum")

filter(total, n==n_early*2)

total2 <- merge(late, n_flights, by="tailnum")
filter(total2, n==2*n_late)


flights%>%
  group_by(tailnum)%>%
  filter(dep_delay==10)

on_time <- select(flights, tailnum, arr_delay)
flights%>%
  group_by(tailnum)%>%
  summarise(
    go = sum(arr_delay==0,na.rm = TRUE),
    bad = sum(dep_delay==120, na.rm = TRUE),
    n=n())%>%
  filter(go/n >= 0.99)
not_cancelled %>% count(dest)
not_cancelled %>% count(tailnum, wt = distance)  
not_cancelled %>%
  group_by(dest)%>%
  summarise(n=n())
not_cancelled %>%
  group_by(tailnum)%>%
  summarise(n=sum(distance))

cancelled <- flights %>%
  filter(is.na(dep_delay), is.na(arr_delay))
cl <- cancelled %>%
  group_by(year,month, day)%>%
  summarise(count=n())
mean(cl$count)
median(cl$count)
mean(flights$arr_delay, na.rm = TRUE)
mean(flights$dep_delay, na.rm = TRUE )
median(flights$dep_delay, na.rm = TRUE)

n_flights <- flights%>%
  group_by(year, month, day)%>%
  summarise(flights=n())
cancelled <- merge(cl, n_flights, by="year")
  
worst_carrier <- not_cancelled %>%
  select(carrier, arr_delay)%>%
  filter(arr_delay>=1, dep_delay>=1)

worst_carrier%>%
  group_by(carrier)%>%
  count()%>%
  arrange(desc(n))

flights %>%
  group_by(carrier, dest) %>%
  summarise(n())

flights_sml%>%
  group_by(year,month, day)%>%
  filter(rank(desc(arr_delay)) <10)

popular_dests <- flights%>%
  group_by(dest)%>%
  filter(n()>365)
popular_dests %>%
  filter(arr_delay>0) %>%
  mutate(prop_delay=arr_delay/sum(arr_delay)) %>%
  select(year:day, dest, arr_delay, prop_delay)

vignette("window-functions")

not_cancelled %>%
  filter(arr_delay >= 10, dep_time>400)%>%
  ggplot(mapping = aes(x=dep_time, y=arr_delay))+
  geom_point(alpha=1/10)+
  geom_smooth(se=FALSE)


not_cancelled %>%
  group_by(dest) %>%
  summarise(total_dep_delay = sum(dep_delay[dep_delay>0], na.rm = TRUE),
            total_arr_delay = sum(arr_delay[arr_delay>0], na.rm = TRUE))
not_cancelled%>%
    filter(arr_delay>0)%>%
    mutate(prop_delay=arr_delay/sum(arr_delay)) %>%
    select(tailnum, dest, prop_delay)

flights%>%
  group_by(carrier,dest)%>%
  summarise(n())

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

diamonds %>%
  count(cut_width(carat,0.5))

smaller <- diamonds %>%
  filter(carat < 3)

ggplot(data=smaller, mapping = aes(x=carat))+
  geom_histogram(binwidth = 0.1)

ggplot(data=smaller, mapping = aes(x=carat, color=cut)) +
  geom_freqpoly(binwidth=0.1)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))


unusual <- diamonds%>%
  filter(y<3 | x>20) %>%
  select(price, x,y,z) %>%
  arrange(y)


ggplot(diamonds) + geom_histogram(mapping = aes(x=price), binwidth = 5000)

diamonds %>%
  filter(carat == 1)%>%
  count()
diamonds2 <- diamonds %>% 
  mutate(y = ifelse(y < 3 | y > 20, NA, y))


ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point()


ggplot(data = diamonds2, mapping = aes(x = x, y = y)) + 
  geom_point(na.rm = TRUE)


flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min/60
  )%>%
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(mapping = aes(color=cancelled), binwidth=1/4)

ggplot(data = diamonds2, mapping = aes(x = carat)) + 
  geom_bar(na.rm = TRUE)


s <- c(4,5,6,7,3.3,57,2,NA,45)
mean(s, na.rm = TRUE)

diamonds%>%
  mean(price, na.rm = TRUE)

library(tidyverse)
library(nycflights13)
view(not_cancelled)
 ggplot(data=not_cancelled, mapping = aes(x=month, y=dep_time, group=month)) + 
   geom_boxplot() 


install.packages("ggstance")

ggstance::geom_boxploth(mapping = aes(x=month, y=dep_time, group=month), data=not_cancelled)

ggplot(data=diamonds, mapping = aes(y=price, x=cut,  fill=cut))+geom_lv(color="black")
install.packages("lvplot")
library(lvplot)

ggplot(data=diamonds, mapping = aes(y=price, x=cut,  fill=cut)) +geom_violin()
ggplot(data=diamonds, mapping = aes(x=price,  fill=cut))+geom_histogram(binwidth = 3000)
ggplot(data=diamonds, mapping = aes(y=price, x=cut,  color=cut))+geom_jitter()
install.packages("ggbeeswarm")
library(ggbeeswarm)
?`ggbeeswarm-package`
ggplot(data=diamonds, mapping = aes(y=price, x=cut))+geom_beeswarm()


ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))


ggplot(data = not_cancelled) +
  
  geom_count(mapping = aes(x=dest, y=carrier))

not_cancelled%>%
  filter(carrier == "DL")%>%
  count(dest)%>%
  arrange(desc(n))
not_cancelled%>%
  count(carrier, dest)%>%
  ggplot(mapping = aes(x=dest, y=carrier))+
  geom_tile(mapping = aes(fill=n))
  
diamonds %>% 
  count(color, cut) %>%  
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

not_cancelled%>%
  filter(arr_delay>0)%>%
  count(month, dest)%>%
  ggplot(mapping = aes(x=dest, y=month)) +
  geom_tile(mapping = aes(fill=n)) +  coord_cartesian(xlim = c(0, 10))


not_cancelled%>%
  
  filter(arr_delay>0)%>%
  count(dest)


install.packages("hexbin")
library(hexbin)


ggplot(data = smaller) +
  geom_bin2d(mapping = aes(x = carat, y = price))
ggplot(data = smaller) +
  geom_hex(mapping = aes(x = carat, y = price), color="lightblue")
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))
ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_number(carat, 10)))


library(tidyverse)
library(nycflights13)


ggplot(data = smaller, mapping = aes(x = cut, y = price)) + 
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))
view(diamonds)

diamonds %>% 
  ggplot(mapping = aes(x =carat, y =cut)) +
  geom_point()

library(hexbin)
ggplot(data=diamonds, mapping = aes(x=cut, y=price))+
  geom_hex()

ggplot(data = smaller, mapping = aes(x = carat, y = cut)) + 
  geom_boxplot(mapping = aes(group = cut_number(price, 10)))

library(modelr)
?modelr
 ?lm
?log

mod <- lm(log(price) ~ log(carat), data = diamonds)
mod


diamonds2 <- diamonds %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))


ggplot(data = diamonds2) + 
  geom_point(mapping = aes(x = carat, y = resid))


ggplot(data = diamonds2) + 
  geom_boxplot(mapping = aes(x = cut, y = resid))
ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_freqpoly(binwidth = 0.25)
ggplot(faithful, aes(eruptions)) + 
  geom_freqpoly(binwidth = 0.25)
diamonds %>% 
  count(cut, clarity) %>% 
  ggplot(aes(clarity, cut, fill = n)) + 
  geom_tile()


getwdl


library(tidyverse)

ggplot(diamonds, aes(carat, price)) + 
  geom_hex()
ggsave("diamonds.pdf")

write_csv(diamonds, "diamonds.csv")


library(nycflights13)
iris
as_tibble(iris)

tibble(
  x=1:5,
  y=1,
  z=x^2+y
)

tb <- tibble(
  ':)' = "smile",
  ' ' = "space",
  '2000' = "number"
)
tb

tribble(
  ~x, ~y, ~z,
  #--/--/----
  "a", 2, 3.6,
  "b", 1, 8.5
)

tibble(
  a = lubridate::now() + runif(1e3)*86400,
  b= lubridate::today() + runif(1e3)*30,
  c=1:1e3,
  d=runif(1e3),
  e=sample(letters, 1e3, replace = TRUE)
)

nycflights13::flights %>% 
  print(n = 10, width = Inf)

flights$dep_time
flights[["dep_time"]]

flights[[4]]
flights[[2]]

class(as.data.frame(tb))
tb

tibble(mtcars)

mtcars%>%
  print(n=10)
print(mtcars)

df <- data.frame(abc=1, xyz = "a")
df
df$x
df$z
df$y
df$a
df$b
df[,"xyz"]
df[,c("abc", "xyz")]
dt <- tibble(abc=1, xyz="a")
dt
dt$abc
dt$a
dt$abc$xyz
dt[c("abc", "xyz")]
df$abc

annoying <- tibble(
  `1` = 1:10,
  `2` = `1` * 2 + rnorm(length(`1`))
)
annoying
ggplot(data=annoying, mapping = aes(x=annoying$'1', y=annoying$'2')) +
  geom_point()
annoying$'3' <- annoying$'2'/annoying$'1'

annoying <- annoying %>%
  rename(one='1', two='2', three='3')
annoying
?tibble::enframe()
enframe(1:5)
enframe(c(a=5,b=7), name = "id", value = "index")
deframe(tibble(a = 1:3))

deframe(enframe(3:1))

annoying%>%
  print(n=10)

heights <- read_csv("data/heights.csv")

read_csv("a,b,c
         1,2,3
         4,5,6")

read_csv("The first line of metadata
  The second line of metadata
  x,y,z
  1,2,3", skip = 2)

read_csv("# A comment I want to skip
  x,y,z
  1,2,3", comment = "#")
 
read_csv("1,2,3\n4,5,6", col_names = FALSE)

read_csv("1,2,3\n4,5,6", col_names = c("x","y","z"))

read_csv("a,b,c\n1,2,.", na = ".")
?read_tsv
?read_fwf
read_delim("a|b\n1.0|2.0", delim = "|")
?read_csv

read_csv("x,y\n1,'a,b'", quote = "'a,b'")

read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n1,2\n a,b")
read_csv("a,b\n1,3")

str(parse_logical(c("TRUE", "FALSE", "NA")))

str(parse_integer(c("1", "2", "3")))

str(parse_date(c("2010-01-01", "1979-10-14")))





library(tidyverse)
parse_double("1.23")
parse_double("1,23", locale = locale(decimal_mark = ","))

parse_number("$100")

parse_number("20%")

parse_number("It cost $123.45")

# Used in America
parse_number("$123,456,789")


# Used in many parts of Europe
parse_number("123.456.789", locale = locale(grouping_mark = "."))


# Used in Switzerland
parse_number("123'456'789", locale = locale(grouping_mark = "'"))
Hadley
charToRaw("Hadley")
charToRaw("ivan")

x1 <- "El Ni\xf1o was particularly bad this year"
x2 <- "\x82\xb1\x82\xf1\x82\xc9\x82\xbf\x82\xcd"

parse_character(x1, locale = locale(encoding = "Latin1"))
#> [1] "El Niño was particularly bad this year"
parse_character(x2, locale = locale(encoding = "Shift-JIS"))
#> [1] "???????????????"


guess_encoding(charToRaw(x1))
guess_encoding(charToRaw(x2))

fruit <- c("apple", "banana")
parse_factor(c("apple", "banana", "bananana"), levels = fruit)


parse_datetime("2010-10-01T2010")

parse_datetime("2010-10-10")

parse_date("2010-10-01")
library(hms)
parse_time("01:10 am")
parse_time("01:10 pm")
parse_time("20:10:01")

parse_date("01/02/15", "%m/%d/%y")
#> [1] "2015-01-02"
parse_date("01/02/15", "%d/%m/%y")
#> [1] "2015-02-01"
parse_date("01/02/15", "%y/%m/%d")
#> [1] "2001-02-15"

parse_date("1 janvier 2015", "%d %B %Y", locale = locale("fr"))
#> [1] "2015-01-01"

?locale

vignette("locales")
parse_double(c("3,45", "23,2", "1,67"), locale = locale(grouping_mark = "."))

?parse
library(tidyverse)

?date_format
?locale
?read_csv2

read_csv2("a;b\n1,3;2,2")
read_csv("a,b\n1.3,2.2")
?Encoding

d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14" # Dec 30, 2014
t1 <- "1705"
t2 <- "11:15:10.12 PM"

parse_date(d1, format = "%B-%d-%Y")
parse_date(d2, "%Y-%b-%d")
mdy(d1)
library(lubridate)
mdy(d1) #best_choice
parse_date(d3, "%d-%b-%Y")
parse_date(d4, format = "%B %d %Y")
mdy(d4)
parse_date(d5, format = "%m/%d/%y")
parse_date(d5, "%m/%d/%y")
parse_time(t1, "%H%M")
parse_time(t2)
challenge

challenge <- read_csv(readr_example("challenge.csv"))

problems(challenge)
tail(challenge)

challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_logical()
  )
)
stop_for_problems(challenge)
tail(challenge)
challenge <- read_csv(
  readr_example("challenge.csv"), 
  col_types = cols(
    x = col_double(),
    y = col_date()
  )
)
tail(challenge)
?readr_example

challenge2 <- read_csv(
  readr_example("challenge.csv"), guess_max = 1001
)
challenge2 <- read_csv(readr_example("challenge.csv"),
                       col_types = cols(.default = col_character()))
challenge2


df <- tribble(
  ~x,  ~y,
  "1", "1.21",
  "2", "2.32",
  "3", "4.56"
)
df

type_convert(df)
write_csv(challenge, "challenge.csv")
write_csv(challenge, "challenge-2.csv")

write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")
library(feather)
 install.packages("feather")
write_feather(challenge, "challenge.feather")
read_feather("challenge.feather")

table1
table2
table3
table4b

table1 %>%
  mutate(rate=cases/population*10000)

?wt
?count

table1%>%
  group_by(year)%>%
  summarise(sum(cases))
table1%>%
  count(year, wt = cases)

ggplot(table1, aes(year, cases)) + 
  geom_line(aes(group=country), color="grey50") +
  geom_point(aes(color=country))

table2n <- table2%>%
  filter(type == "cases")
table2n <- table2n %>%
  select(country, year, count)%>%
  rename("cases" = count)
table2np <- table2%>%
  filter(type=="population")
table2n <- table2n %>%
  mutate(population = table2np$count,
         rate=cases/population*10000)
table2%>%
  filter(type == "cases")%>%
  ggplot(aes(year,count)) +
  geom_line(aes(group=country), color="grey20")+
  geom_point(aes(color=country))

table4a
table4b

table4a %>%
  pivot_longer(cols = c("1999","2000"),
               names_to="year",
               values_to="cases")
tidy4b <- table4b %>%
  pivot_longer(cols = c("1999","2000"),
               names_to="year",
               values_to="population")

library(tidyverse)
left_join(tidy4a, tidy4b)
table2
table2%>%
  pivot_wider(names_from = type, values_from = count)

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks

stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")%>%
  arrange(year)

table4a %>% 
  pivot_longer(c("1999", "2000"), names_to = "year", values_to = "cases")

people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
 people %>%
   pivot_wider(names_from = names, values_from = values, values_fn=mean)
?pivot_wider 
 
 preg <- tribble(
   ~pregnant, ~male, ~female,
   "yes",     NA,    10,
   "no",      20,    12
 )
preg %>%
  pivot_longer(cols=ends_with("male"), names_to = "gender", values_to = "count")%>%
  select(gender,pregnant,count)
 
library(tidyverse)
 
stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return") %>%
  arrange(year) 
 
people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

people%>%
  pivot_wider(names_from = names, values_from=values, values_fn=mean)

preg%>%
  pivot_longer(names_to = c("yes", "no"), values_to=c("male","female"))

table3%>%
  separate(rate, into = c("cases", "population"))

table3 %>%
  separate(rate, into = c("cases", "poppulation"), sep = "/", convert = TRUE )
table3 %>%
  separate(rate, into = c("cases", "population"), convert = TRUE)
table3%>%
  separate(year, into = c("century", "year"), sep = 2)
table5%>%
  unite(new, century, year, sep="")

tibble(x = c("a,b,c", "d,e,f,g", "h,i,j")) %>% 
  separate(x, c("one", "two", "three"), extra = "merge")
?separate  

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  separate(x, c("one", "two", "three"), fill = "right")
?extract

tibble(x = c("a,b,c", "d,e", "f,g,i")) %>% 
  extract(x, c("one", "two", "three"))

stocks <- tibble(
  year   = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
  qtr    = c(   1,    2,    3,    4,    2,    3,    4),
  return = c(1.88, 0.59, 0.35,   NA, 0.92, 0.17, 2.66)
)
stocks %>%
  pivot_wider(names_from = year, values_from=return) %>%
  pivot_longer(
    cols = c("2015", "2016"),
    names_to = "year",
    values_to = "return",
    values_drop_na = TRUE
  )
stocks %>%
  complete(year,qtr)
view

treatment <- tribble(
  ~ person,           ~ treatment, ~response,
  "Derrick Whitmore", 1,           7,
  NA,                 2,           10,
  NA,                 3,           9,
  "Katherine Burke",  1,           4
)
treatment%>%
  fill(person, .direction = "down")
?complete
?tidyr::fill

who
view(who)

who1 <- who %>%
  pivot_longer(
    col=new_sp_m014:newrel_f65,
    names_to = "key",
    values_to= "cases",
    values_drop_na = TRUE
  )
who1
view(who1)
who1%>%
  count(key)

who2 <- who1 %>%
  mutate(names_from = stringr::str_replace(key, "newrel", "new_rel"))
who2
view(who2)

who3 <- who2 %>%
  separate(key, c("new", "type", "sexage"), sep = "_")
who3%>%
  count(new)
who4 <- who3 %>%
  select(-new,-iso2,-iso3)
who5 <- who4 %>%
  separate(sexage, c("sex", "age"), sep=1)
who5 <- who %>%
  pivot_longer(
    cols = new_sp_m014:newrel_f65, 
    names_to = "key", 
    values_to = "cases", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>%
  separate(key, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)
who_na <- who %>%
  pivot_longer(
    col=new_sp_m014:newrel_f65,
    names_to = "key",
    values_to= "cases")
who_na%>%
  complete(key, cases)
countr <- who%>%
    select(country, iso2,iso3)%>%
    count(country, iso2, iso3)
country <- who5%>%
  group_by(country)%>%
  summarise(country, n=n())
library(tidyverse)    
who_by_country <- who5 %>%
  group_by(country)%>%
  summarise(cases=sum(cases))%>%
  arrange(desc(cases))
who_by_year <- who5 %>%
  group_by(year) %>%
  summarise(cases=sum(cases))
who_by_sex <- who5 %>%
  group_by(sex)%>%
  summarise(cases = sum(cases))

ggplot(data = who_by_year, mapping = aes(x=year,y=cases,size=cases)) +
  geom_count(alpha=1/7)

install.packages("gganimate")
?ggplot
library(gganimate)
animation_who3 <- who5%>%
  filter(country == c("Ukraine", "Poland", "Romania", "Slovakia", "Hungary"))%>%
  group_by(country,year,var,sex)%>%
  summarise(cases=sum(cases))%>%
  ggplot(aes(year, cases, size = cases, colour = var)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~country)+
  # Here comes the gganimate specific bits
  transition_time(year) +
  shadow_mark()

animate(animation_who3, fps = 13, width = 1500, height = 900)
anim_save("animation_who5.gif")

gifski_renderer(animation_who)
anim_save("animation_who")
last_animation()
install.packages("gifski")
library(gifski)
?scale_colour_manual
print(animation_who)
animation_who <- animate(animation_who)
anim_save("who.gif")


animate(animation_who, fps = 10, width = 750, height = 450)
anim_save("animation_who.gif")

anim_who2 <- who5%>%
  filter(country == c("Ukraine", "Poland", "Romania", "Slovakia", "Hungary"))%>%
  group_by(country,year,var,sex)%>%
  summarise(cases=sum(cases))%>%
  ggplot(aes(x = year, y = cases)) +
  geom_point(shape = 21, color = "purple", aes(fill = cases), size=5, stroke=1) +
  scale_x_continuous(limits = c(1980,2015)) +
  scale_y_continuous() +
  scale_fill_distiller(palette = "RdYlBu", limits = c(-1,1), guide = FALSE) +
  xlab("year") +
  ylab("TB cases") +
  theme_minimal(base_size = 16, base_family = "Georgia") +
  facet_wrap(~country)+
  transition_time(year) +
  shadow_mark()
  # gganimate code
  
# save as a GIF
animate(anim_who2, fps = 10, width = 750, height = 450)
anim_save("anim_who2.gif")






