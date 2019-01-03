#install.packages("knitr", repos = 'http://cran.us.r-project.org')

library(dplyr)
library(data.table)
library(lubridate)
library(tidyr)
library(ggplot2)
library(sf)
library(plotly)
library(knitr)
library(sf)

data <- readRDS("N:/QGIS/POSTGRAD_GIS/Night_Tube/NT/tidied_cpc_data.rds")
stations <- readRDS("N:/QGIS/POSTGRAD_GIS/Night_Tube/NT/stations2.rds")

times_night_start <- "00:00:00"
times_night_end <- "06:15:00"

# All dates with Night Tube services
dates_night <- c("19/05/2018", "20/05/2018", 
                 "26/05/2018", "27/05/2018", 
                 "02/06/2018", "03/06/2018",
                 "09/06/2018", "10/06/2018")

fridays <- c("19/05/2018","26/05/2018","02/06/2018","09/06/2018")
saturdays <- c("20/05/2018","27/05/2018","03/06/2018","10/06/2018")

#######JUBILEE LINE######

#######FRIDAYS##########


# Identify all journeys on Night Tube dates = 5,250,777 TOTAL JOURNEYS
friday_subset <- data%>%
  filter(calendardate %in% fridays)

# Make empty list to store each Night Tube date's journeys

friday_night_list <- vector("list", length = length(fridays))

for (i in 1:length(fridays)){
  
  print(paste0("Processing data for ", i, " of ", length(fridays), " nights..."))
  
  # one_night <- data_split[[dates_night[i]]]
  
  f_one_night <- subset %>% 
    filter(calendardate == fridays[i])
  
  ############# # Filter for Night Tube journeys #elaborate on this step!!!!!!!!!!##########
  f_one_night <- f_one_night %>% 
    filter(entrytime > as.POSIXct(strptime(x = paste(fridays[i], times_night_start), 
                                           format = "%d/%m/%Y %H:%M:%S")),
           entrytime < as.POSIXct(strptime(x = paste(fridays[i], times_night_end), 
                                           format = "%d/%m/%Y %H:%M:%S")))
  
  # Replace each date's journeys with the Night Tube subset
  friday_night_list[[fridays[i]]] <- f_one_night
  
}

friday_night_data_entries <- rbindlist(friday_night_list)

# FRIDAY ENTRIES
f_entry_station_count<-aggregate(data.frame(count = friday_night_data_entries$entrystation), list(value = friday_night_data_entries$entrystation), length)

f_sorted_entry_time<-friday_night_data_entries[order(friday_night_data_entries$entrytime),]

f_sorted_entry_time$by1hour<-cut(f_sorted_entry_time$entrytime,breaks = "1 hour")


# 1 HOUR: aggregate based on the 1 hour chunks FOR ALL FRIDAYS

f_summary_counts_by1hour<-aggregate(data.frame(count = f_sorted_entry_time$by1hour), list(value = f_sorted_entry_time$by1hour), length) 

#summary_counts_by1hour[which.max(summary_counts_by1hour$count),]
#summary_counts_by1hour[which.min(summary_counts_by1hour$count),]

f_sorted_entry_station<-friday_night_data_entries[order(friday_night_data_entries$entrystation),]

f_sorted_entry_station<-f_sorted_entry_station[with(f_sorted_entry_station, order(entrystation, entrytime)), ]

f_sorted_entry_station$by1hour<-cut(f_sorted_entry_station$entrytime, breaks = "1 hour")

f_summary_entrystation_by1hour_all_fridays <- f_sorted_entry_station %>%
  group_by(entrystation, by1hour) %>%
  tally() %>%
  spread(by1hour, n, fill = 0)

#####JUBILEE PLOTS######


jubilee_NT <- c(jubilee_NT <- c("Stanmore", "Canons Park", "Queensbury", "Kingsbury", "Wembley Park", "Neasden", "Dollis Hill", "Willesden Green",
                                "Kilburn", "West Hampstead LO", "Finchley Road", "Swiss Cottage", "St Johns Wood", "Baker Street", "Bond Street", 
                                "Green Park", "Westminster", "Waterloo LU (Jubilee)", "Southwark", "London Bridge LU", 
                                "Bermondsey", "Canada Water", "Canary Wharf LU (E1)","Canary Wharf LU (E2)","North Greenwich", "Canning Town", "West Ham", "Stratford"))

### FRIDAY Jubilee line entries ###

f_jubilee_subset_entries<- f_summary_entrystation_by1hour_all_fridays[f_summary_entrystation_by1hour_all_fridays$entrystation %in% jubilee_NT, ] #or subset(df, ID %in% jubilee_NT)


#jubilee_F1_subset<- summary_entrystation_by1hour[summary_entrystation_by1hour$entrystation %in% jubilee_NT,1:8] #or subset(df, ID %in% jubilee_NT)

#DF1 <- data.frame(jubilee_F1_subset$entrystation,
#                  BakerStreet = jubilee_F1_subset[1],midnight = jubilee_F1_subset$`2018-05-19 00:00:00`,
#                  one = jubilee_F1_subset$`2018-05-19 01:00:00`,
#                 two = jubilee_F1_subset$`2018-05-19 02:00:00`)

#DF1 <- melt(jubilee_F1_subset,  id.vars = 'entrystation', variable.name = 'hours')

f_DF <- melt(f_jubilee_subset_entries,  id.vars = 'entrystation', variable.name = 'hours')

f_jubilee_entries_plot <- ggplot(f_DF, aes(hours, value)) + geom_point(aes(colour = entrystation)) + ggtitle("Jubilee line | Fridays | All entries")
#f_jubilee_entries_plot



f_jubilee_entries_plot<- f_jubilee_entries_plot + 
  scale_x_discrete(labels=c("2018-05-19 00:00:00" = "F1-00","2018-05-19 01:00:00"="F1-01",
                            "2018-05-19 02:00:00"="F1-02","2018-05-19 03:00:00"="F1-03",
                            "2018-05-19 04:00:00"="F1-04","2018-05-19 05:00:00" = "F1-05",
                            "2018-05-19 06:00:00" = "F1-06", "2018-05-26 00:00:00" = "F2-00",
                            "2018-05-26 01:00:00" = "F2-01","2018-05-26 02:00:00" = "F2-02",
                            "2018-05-26 03:00:00" = "F2-03","2018-05-26 04:00:00" = "F2-04",
                            "2018-05-26 05:00:00" = "F2-05","2018-05-26 06:00:00" = "F2-06",
                            "2018-06-02 00:00:00" = "F3-00","2018-06-02 01:00:00" = "F3-01",
                            "2018-06-02 02:00:00" = "F3-02","2018-06-02 03:00:00" = "F3-03",
                            "2018-06-02 04:00:00" = "F3-04","2018-06-02 05:00:00" = "F3-05",
                            "2018-06-02 06:00:00" = "F3-06","2018-06-09 00:00:00" = "F4-00",
                            "2018-06-09 01:00:00" = "F4-01","2018-06-09 02:00:00" = "F4-02",
                            "2018-06-09 03:00:00" = "F4-03","2018-06-09 04:00:00" = "F4-04",
                            "2018-06-09 05:00:00" = "F4-05","2018-06-09 06:00:00" = "F4-06"))



F_entries <- ggplotly(f_jubilee_entries_plot)
F_entries
