# # ===================================================
# GBA464: Assignment 2 
# Author: Yufeng Huang
# Description: working with data frame
# Data: European car characteristics, prices and sales, 1970-1999
# Source: 
#   cars: https://sites.google.com/site/frankverbo/data-and-software/data-set-on-the-european-car-market
#   crude oil price: OPEC; IEA; extracted from 
#   http://www.statista.com/statistics/262858/change-in-opec-crude-oil-prices-since-1960/
# Optionally, you can also try plotting against UK gasoline price from www.theaa.com 
#   link: https://dl.dropboxusercontent.com/u/13844770/rdata/assignment_2/ukgas.csv
#   (https://www.theaa.com/public_affairs/reports/Petrol_Prices_1896_todate_gallons.pdf)
# Acknowledgement: Frank Verboven has contributed significant effort
#   making the car sales dataset publically available
# ===================================================
 
# ===== Step 0: load data and required packages =====

# download file to local folder if file does not exist
url <- 'https://dl.dropboxusercontent.com/s/nchoevokxmodlqu/cars.csv' 
if (!file.exists('cars.csv')) {     # check whether data exists in local folder (prevents downloading every time)
    download.file(url, 'cars.csv', mode = 'wb')
}
df <- read.csv('cars.csv')  # load data

url <- 'https://dl.dropboxusercontent.com/s/t9z1oe5e4d7uqya/oil.csv' 
if (!file.exists('oil.csv')) {
    download.file(url, 'oil.csv', mode = 'wb')
}
oil <- read.csv('oil.csv')  # load data

# ===== Question 0: what are the keys of the data frames? =====
# Before we start, let's think about what are the keys in the data frame
#   You don't have to do anything now; just think about it

# keys are $year for oil, and $ye + $ma + $co

# ===== Question 1: cleanup data =====
# 1) Take a subset of df, focusing on observations where class ($cla) is "standard" or 
#   "intermediate" or "luxury", use this sub-data-frame to replace the original df (now df is different)
# 2) Generate a column in df, $mpg, that measures fuel efficiency in mile per gallon
#   note that 1 mile per gallon = 235.215 / (1 liter per 100km). In other words, mpg is not 
#   proportional to liter per 100km, but is proportional to 1/(liter per 100). To check your answers, 
#   keep in mind that we usually see mpgs between 10 and 40. 
#   Also note: in the variable description, $li should be liter per 100km rather than liter per km.
# 3) Find a way to replace year in dataframe oil ($ye) into up to 2 digit (e.g. 1990 becomes 90, and 2001 becomes 1)

df <- df[df$cla=="standard" | df$cla=="intermediate" | df$cla=="luxury", ]
# df <- df[df$cla %in% c("standard", "intermediate", "luxury")]
# df <- subset(df, cla %in% c("standard", "intermediate", "luxury"))
df$mpg <- 235.215/df$li # convert liter/100km into mpg
oil$ye <- oil$ye %% 100 # note the operator %%

# ===== Question 2: summarize fuel efficiency by year and manufacturer =====
# Take average of fuel efficiency $mpg for given the firm ($frm) and year ($ye)
#   You could use the function aggregate(). Name the output (data frame) as "df_avg".
# Then, plot the average $mpg for firm ($frm) Volkswagen ("VW") across all years.
#   Set your axis label to "year" and "mile per gallon" and put up a title
#   "Volkswagen fuel efficiency"

df_avg <- aggregate(x = mpg ~ ye + frm, 
                    data = df_avg, 
                    FUN = mean, 
                    na.rm = T)
# df_avg <- aggregate(x = list(mpg = df$mpg),
#                     by = list(ye = df$ye, frm = df$frm),
#                     FUN = mean,
#                     na.rm = T)

plot(df_avg$ye[df_avg$frm == "VW"], df_avg$mpg[df_avg$frm == "VW"], 
     xlab = "year", ylab = "mile per gallon", 
     main = "Volkswagen fuel efficiency", type = 'l')
# plot(formula = mpg ~ ye, 
#      data = df.mean[df.mean$frm == "VW", ])


# ===== Question 3: merge with gasoline price data =====
# 1) Merge "df_avg" with crude oil price data, replace data frame "df_avg" itself
# 2) Create the same plot as above (also for VW) but add a plot of crude oil price over time
#   when doing so:  a) set xlab and ylab to "" in order to not generate any label,
#                   b) generate an axis on the right by using axis(side = 4, ...)
# 3) 1985 was the start of the new US regulation on fuel efficiency (which was announced in 1975).
#   Add a vertical line in the year 1985 to indicate this (search help lty or col for line type or 
#   color; adjust them so that the graph does not look that busy)

df_avg <- merge(df_avg, oil, by = "ye")

with(df_avg[df_avg$frm == "VW", ], {  # the with() function specifies that we're working within the data frame df.mean
    plot(ye, mpg, xlab = "year", ylab = "mile per gallon", type = 'l', main = "Volkswagen fuel efficiency (right = crude oil price)")
    par(new = T)
    plot(ye, oilpr, type = 'l', lty = 2, col = "grey55", axes=FALSE, xlab = "", ylab = "")
    axis(side = 4, col = "grey65", col.ticks = "grey65", col.lab = "grey65")
    par(new = F)
    abline(v = 85, lty = 3)
})

# ===== Question 4: find new cars models =====
# Start with the subsetted data focusing on Volkswagen and on "standard" or "intermediate"
#   or "luxury" cars.
# 1) Find the first year where a specific car model (indicated by variable $type) 
#   has positive sales in a given market ($ma); i.e. that's the year when the model started to sell at all
#   Think of this as the year of introduction; consider using aggregate(). Name the variable $first_year
# Note: You might want to construct a data frame for this, and then merge it to the subset of df that  
#   focuses on Volkswagen, and assign the merge result to df_augment
# 2) Generate a sub-data frame where each car model just started selling for the first/second year;
#   that is, year <= first_year + 1; name the data frame as "df_new"
# 3) Generate average $mpg by year, for all the cars (in our subset) that started selling for the first/second year; 
#   use aggregate(). Name the data frame "df_avg_new"
# 4) [Optional] Generate the same plot as in Question 3, but now focusing on the "new cars" that we defined above. 

brand_intro_year <- aggregate(x = ye ~ type + brand + ma, data = df, min, na.rm = T)
colnames(brand_intro_year) <- c('type', 'brand', 'ma', 'first_year')
df_augment <- merge(df, brand_intro_year, by = c("type", "brand", "ma"))
df_new <- subset(df_augment, ye <= first_year + 1)
df_avg_new <- aggregate(x = mpg ~ ye + frm, data = df_new, mean, na.rm = T)
df_avg_new <- merge(df_avg_new, oil, by = "ye")

with(df_avg_new[df_avg_new$frm == "VW", ], {
    plot(ye, mpg, xlab = "year", ylab = "mile per gallon", type = 'l', main = "Volkswagen fuel efficiency (right = crude oil price)")
    par(new = T)
    plot(ye, oilpr, type = 'l', lty = 2, col = "grey55", axes=FALSE, xlab = "", ylab = "")
    axis(side = 4, col = "grey65", col.ticks = "grey65", col.lab = "grey65")
    par(new = F)
    abline(v = 85, lty = 2)
})


