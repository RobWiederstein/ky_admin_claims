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
        summarise(min = min(medicaid.pct.total),
                  Q_1 = quantile(medicaid.pct.total, .25),
                  median = median(medicaid.pct.total),
                mean = mean(medicaid.pct.total),
                Q_3 = quantile(medicaid.pct.total, .75),
                max = max(medicaid.pct.total)) %>%
        arrange(-mean)
df.1
#top 10 medicaid percentages in latest year
df.2 <- 
        df %>%
        select(hospital, year, medicaid.pct.total) %>%
        filter(year == max(year)) %>%
        arrange(-medicaid.pct.total) %>%
        slice(1:10)

df.2
#top 10 medicaid percentages in first year
df.21 <- 
        df %>%
        select(hospital, year, medicaid.pct.total ) %>%
        filter(year == min(year)) %>%
        arrange(-medicaid.pct.total) %>%
        slice(1:10)

head(df.21, 10)

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
df$year <- paste("year_", df$year, sep = "")
df.3 <- df %>%
        select(hospital, medicaid.pct.total, year) %>%
        spread(year, medicaid.pct.total) %>%
        na.omit() %>%
        mutate(increase = year_2015 - year_2012)%>%
        arrange(-increase)
df.3$rank <- row.names(df.3)
df.3

df.4 <- 
        df.3 %>%
        select(hospital, year_2012, year_2015) %>%
        gather(key = year, value = medicaid.pct.total, -hospital)
df.4$year <- gsub("year_", "", df.4$year)
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
