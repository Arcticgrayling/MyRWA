require(stringr)
require(lubridate)

tbl.time = "913"  # hour min time value
st.min <- str_sub(tbl.time,-2,-1)  # gets min  - 1st and second digit from right
st.hour <- str_sub(tbl.time, 1, -3)  #gets hours - 1st digit to 3rd digit from right
new.time <- paste(st.hour, st.min, sep=":")  # combine to create hour:min  "9:13"
new.time                                      # print result

tbl.date = "6/27/79"
new.date <- as.Date(tbl.date, format = "%m/%d/%y") # rearanges date to "1979-06-27"
                                                   #with as.Date - /79 is 1979 not 2079
new.date                                 # print result

new.dt <- paste(new.date, new.time, sep=" ")  # gives format "6/27/79 9:13"
new.dt                                         # print result

final.date <- ymd_hm(new.dt, tz = "EST")       # gives "1979-06-27 09:13:00 EST"n
final.date                                     # print result




#more tries

new.dt <- "1979-06-27 9:13"
final.date <- ymd_hm(new.dt, tz = "EST")
final.date 

tbl.date = "1/1/79"
dtl <- as.Date(tbl.date, format = "%m/%d/%y")
dtl

# this worked
require(stringr)
require(lubridate)

tbl.time = "913"  # hour min time value
st.min <- str_sub(tbl.time,-2,-1)  # gets min  - 1st and second digit from right
st.hour <- str_sub(tbl.time, 1, -3)  #gets hours - 1st digit to 3rd digit from right
new.time <- paste(st.hour, st.min, sep=":")  # paste to create hour:min  "9:13"
new.time

tbl.date = "6/27/79"
new.date <- as.Date(tbl.date, format = "%m/%d/%y") # rearanges date to "1979-06-27"
new.date

new.dt <- paste(new.date, new.time, sep=" ")  # gives format "1979-06-27 9:13"
new.dt

final.date <- ymd_hm(new.dt, tz = "EST")       # gives "1979-06-27 09:13:00 EST"n
final.date
# end of this worked

final.date2 < dmy_hm(new)

#other tries  


tbl.time = "913"  # hour min time value
st.min <- str_sub(tbl.time,-2,-1)  # gets min  - 1st and second digit from right
st.hour <- str_sub(tbl.time, 1, -3)  #gets hours - 1st digit to 3rd digit from right
new.time2 <- paste(st.hour, st.min, sep=":")  # paste to create hour:min  "9:13"
new.time2

date2 <- "4/6/79"
new.date2 <-  dmy_hm(date2, new.time2)
new.date2



f.date2 <- paste(new.date2, new.time2, sep=" ")
final.date2 <- y

ddd <- dmy_hm("6/04/1979 9:13")
#ddd <- dmy_hm("06/04/1979 9:13", tz = "EST")
ddd
