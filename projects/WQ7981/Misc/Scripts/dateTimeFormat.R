require(stringr)
require(lubridate)
""" From Table 2, Mystic River Basin Survey 1979-81
    Time"""

  # example from AJ01 - 12/12/79
tbl.time = "1150"  # hour and min time value used in report
tbl.date = "12/12/79"  # Date from report

  # Format time so it can be used
st.min <- str_sub(tbl.time,-2,-1)  # gets min  - 1st and second digit from right
st.hour <- str_sub(tbl.time, 1, -3)  #gets hours - 1st digit from Left to 3rd digit from right
                                     # can handle 1 or 2 digit hour values
new.time <- paste(st.hour, st.min, sep=":")  # combine to create hour:min  "11:50"
new.time                                      # print result

  #Format Date so it can be used and is in the 1900's
new.date <- as.Date(tbl.date, format = "%m/%d/%y") # rearanges date to ""1979-12-12"
                                                   #with as.Date - /79 is 1979 not 2079
new.date                                 # print result

  #paste date and time together
new.dt <- paste(new.date, new.time, sep=" ")  # gives format "1979-12-12 11:50"
new.dt                                         # print result

final.date <- ymd_hm(new.dt, tz = "EST")       # gives "1979-12-12 11:50:00 EST" ; desired format
final.date                                     # print result

