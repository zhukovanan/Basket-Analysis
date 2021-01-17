#Load all libraries
library(DBI)
library(data.table)
library(odbc)
library(lubridate)
library(ggplot2)
library(magrittr)


#Create connection to database
con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "####", 
                 Database = "####", Trusted_Connection = "True", encoding = "1251")

order_stat <- dbGetQuery(con, "select *
                                from ####")

#Count of occurrences of 3 subcategory in order and separate for next joins
new <- setDT(order_stat)[, if (.N >= 3L) .(triplet =  
                                      sapply(combn(Подкатегория, 3L, simplify = FALSE),paste, collapse = "|")),
                  by = .(
                    Number,Date) ][, c("one", "two", "three") := tstrsplit(triplet, "|", fixed=TRUE)]
#Set keys
setkey(new, Number,one, two, three)
setkey(order_stat, Number, Подкатегория)

#Join sales
new <- new[, one_sale := order_stat[new, on = c(Number = "Number", Подкатегория = "one" ), 
         x.Sale] ][, two_sale := order_stat[new, on = c(Number = "Number", Подкатегория = "two" ), 
         x.Sale]][, three_sale := order_stat[new, on = c(Number = "Number", Подкатегория = "three" ), 
                                           x.Sale]]
#Join Reward
new <- new[, one_profit := order_stat[new, on = c(Number = "Number", Подкатегория = "one" ), 
                                    x.Reward] ][, two_profit := order_stat[new, on = c(Number = "Number", Подкатегория = "two" ), 
                                    x.Reward]][, three_profit := order_stat[new, on = c(Number = "Number", Подкатегория = "three" ), 
                                    x.Reward]]
#Get total sum for sales and reward in terms of combinations
new <- new[, Sale := Reduce(`+`, .SD), .SDcol = 7:9][,Reward := Reduce(`+`, .SD), .SDcol = 10:12][,c(4:12) := NULL]
#Get mean statistics 
new <- new[ , .(Sale = sum(Sale), Reward = sum(Reward), Q= .N) ,by = .(triplet, month(Date))][, .(Sale = 
                                                                                      mean(Sale, na.rm =  T), Reward = mean(Reward, na.rm = T), Q = round(mean(Q))) , by = triplet ][order(-Q)]
