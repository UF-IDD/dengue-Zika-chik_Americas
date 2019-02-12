
# copied from EpiWeek package (version 1.1) which was discontinued
#  @Manual{,
#    title = {EpiWeek: Conversion Between Epidemiological Weeks and Calendar Dates},
#    author = {Xiahong Zhao},
#    year = {2016},
#    note = {R package version 1.1},
#    url = {https://CRAN.R-project.org/package=EpiWeek},
#  }
epiweekToDate = function (year, weekno, firstday = "Sunday") 
{
    if (!(firstday == "Sunday" || firstday == "Monday")) {
        print("Wrong firstday!")
        break
    }
    if (year < 0 || weekno < 0) {
        print("Wrong Input!")
        break
    }
    jan4 = strptime(paste(year, 1, 4, sep = "-"), format = "%Y-%m-%d")
    wday = jan4$wday
    wday[wday == 0] = 7
    wdaystart = ifelse(firstday == "Sunday", 7, 1)
    if (wday == wdaystart) 
        weekstart = jan4
    if (wday != wdaystart) 
        weekstart = jan4 - (wday - ifelse(firstday == "Sunday", 
            0, 1)) * 86400
    jan4_2 = strptime(paste(year + 1, 1, 4, sep = "-"), format = "%Y-%m-%d")
    wday_2 = jan4_2$wday
    wday_2[wday_2 == 0] = 7
    wdaystart_2 = ifelse(firstday == "Sunday", 7, 1)
    if (wday_2 == wdaystart_2) 
        weekstart_2 = jan4_2
    if (wday_2 != wdaystart_2) 
        weekstart_2 = jan4_2 - (wday_2 - ifelse(firstday == "Sunday", 
            0, 1)) * 86400
    if (weekno > ((weekstart_2 - weekstart)/7)) {
        print(paste("There are only ", (weekstart_2 - weekstart)/7, 
            " weeks in ", year, "!", sep = ""))
        break
    }
    d0 = weekstart + (weekno - 1) * 7 * 86400
    d1 = weekstart + (weekno - 1) * 7 * 86400 + 6 * 86400
    return(list(d0 = strptime(d0, format = "%Y-%m-%d"), d1 = strptime(d1, 
        format = "%Y-%m-%d")))
}