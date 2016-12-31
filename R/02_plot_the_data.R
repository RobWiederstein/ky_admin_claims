file <- "./data_tidy/2012_2015_KYOHP_Admin_Claims_tidy.csv"
df <-read.csv(file = file, header = T, stringsAsFactors = F)

library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(scales)

#summary on medicaid percentages
df.1 <-
        df %>%
        select(hospital, medicaid.pct.total, year) %>%
        group_by(year) %>%
        summarise(min = min(medicaid.pct.total, na.rm = T),
                  Q_1 = quantile(medicaid.pct.total, .25, na.rm = T),
                  median = median(medicaid.pct.total, na.rm = T),
                mean = mean(medicaid.pct.total, na.rm = T),
                Q_3 = quantile(medicaid.pct.total, .75, na.rm = T),
                max = max(medicaid.pct.total, na.rm = T)) %>%
        arrange(-mean)
df.1
#top 10 medicaid percentages in latest year
df.2 <- 
        df %>%
        select(hospital, year, medicaid.pct.total) %>%
        filter(year == max(year)) %>%
        arrange(-medicaid.pct.total) %>%
        slice(1:25)

df.2
#top 10 medicaid percentages in first year
df.21 <- 
        df %>%
        select(hospital, year, medicaid.pct.total ) %>%
        filter(year == min(year)) %>%
        arrange(-medicaid.pct.total) %>%
        slice(1:25)

df.21

#plot densities for first and latest years
library(ggplot2)
p <- ggplot(df, aes(medicaid.pct.total, colour = as.factor(year)))
p <- p + geom_density()
p <- p + ggtitle("Medicaid as Percent of Inpatient Admissions")
p <- p + theme(plot.title=element_text(hjust=0.5))
p
file <- "./plots/Medicaid_As_Pct_Inpatient_Admissions.pdf"
ggsave(filename = file, width = 8, height = 5, units = "in")


#create data frame for medicaid increase
df.3 <- 
        df %>%
        select(hospital,  year, medicaid.pct.total) %>%
        spread(year, medicaid.pct.total) %>%
        select(hospital, `2012`, `2015`) %>%
        na.omit() %>%
        mutate(increase = `2015` - `2012`)%>%
        arrange(-increase)
df.3$rank <- row.names(df.3)
df.3

df.4 <- 
        df.3 %>%
        select(hospital, `2012`, `2015`) %>%
        gather(key = year, value = medicaid.pct.total, -hospital)
df.4$year <- as.integer(df.4$year)


#plot percent medicaid inpatient hospitalization days by facility
p <- ggplot(df.4, aes(year, medicaid.pct.total, group = hospital))
p <- p + geom_line(colour = "gray")
df.5 <- df.4[df.4$hospital == "Methodist Hosp.", ]
p <- p + geom_line(data = df.5, aes(year, medicaid.pct.total), 
                   colour = "blue3",
                   size = 1.5)
p <- p + scale_x_continuous(name = "year", 
                            breaks = seq(from =  min(df.4$year),
                                         to = max(df.4$year), 
                                         by = 3)
                            )
p <- p + scale_y_continuous(name = "",
                            breaks = c(0, 20, 40, 60, 80),
                            labels = paste(c(0, 20, 40, 60, 80), "%", sep = "")
)

p <- p + theme(plot.title=element_text(hjust=0.5))
p <- p + annotate("text", x = 2014.5, y = 33, label= "Methodist", color = "blue3", size = 2.5)
p <- p + ggtitle("Percent Medicaid Inpatient Hosp. Days by Facility \n 2012-2015")
p <- p + ylab("pct.")
p
file <- "./plots/Pct_Medicaid_Inpatient_Hosp_by_Facility.pdf"
ggsave(filename = file, width = 5, height = 8, units = "in")

#

df.other <- 
        df %>%
        select(hospital,
               year,
               medicaid.pct.total,
               medicare.pct.total,
               commerical.pct.total,
               other.pct.total) %>%
        filter(year %in% c(2012, 2015))

hosp.2012 <- df.other$hospital[which(df.other$year == 2012)]
hosp.2015 <- df.other$hospital[which(df.other$year == 2015)]
hosp.both <- intersect(hosp.2012, hosp.2015)
df.other <-
        df.other %>%
        filter(hospital %in% hosp.both)
df.other <- 
        df.other %>%
        gather(key = variable, value = pct, -hospital, -year)
head(df.other, 20)

#build Methodist data frame
df.local <- filter(df.other, hospital %in% c("Methodist Hosp."))
df.local$hospital[which(df.local$hospital == "Methodist Hosp.")] <- "Methodist"

#build state average data frame
df.mean.2012 <- 
        df.other %>%
        filter(year == 2012)%>%
        group_by(variable) %>%
        summarise(pct = mean(pct)) %>%
        mutate(year = 2012) %>%
        mutate(hospital = "all_hospitals") %>%
        select(hospital, year, variable, pct)
head(df.mean.2012)
df.mean.2015 <-
        df.other %>%
        filter(year == 2015) %>%
        group_by(variable) %>%
        summarise(pct = mean(pct)) %>%
        mutate(year = 2015) %>%
        mutate(hospital = "all_hospitals") %>%
        select(hospital, year, variable, pct)
head(df.mean.2015, 20)
df.mean.2012.2015 <- rbind(df.mean.2012, df.mean.2015)

#make plot of payor source
library(ggplot2)
library(RColorBrewer)
p <- ggplot(df.other, aes(year, pct))
p <- p + geom_line(aes(group = hospital), colour = "gray")
p <- p + facet_grid(. ~ variable)
#add Methodist
p <- p + geom_line(data = df.local, aes(x = year, y = pct), colour = "blue3")
#add Kentucky
p <- p + geom_line(data = df.mean.2012.2015, aes(x = year, y = pct), colour = "black")
p

p <- p + scale_x_continuous(breaks = c(2012, 2013, 2014, 2015),
                            labels = c("12", "13", "14", "15"),
                            minor_breaks = NULL,
                            name = "")
p <- p + scale_y_continuous(breaks = c(0, 20, 40, 60, 80),
                            labels = paste(seq(0, 80, by = 20), "%", sep = ""),
                            name = "")
p <- p + ggtitle("Percentage of Inpatient Days by Payor Source \n 2012 & 2015")
p <- p +  theme(plot.title = element_text(hjust = 0.5))
p <- p + scale_color_brewer("Hospital", palette = "Set1")
p
p <- p + annotate("text", x = 2014.5, y = 86, label = "-- Methodist", colour = "blue3", size = 2)
p <- p + annotate("text", x = 2014.5, y = 89, label = "-- Kentucky", colour = "black", size = 2)
p
file <- "./plots/Pct_Inpatient_Days_by_Payer_Source_2012_2015.pdf"
ggsave(filename = file, width = 8, height = 5, unit = "in")


