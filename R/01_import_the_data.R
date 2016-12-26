create_my_dirs <- function(){
        dir.names <- c("R", "data_raw", "data_tidy", "plots", "tables", "research")
        dir.check <- gsub("\\./", "", list.dirs())
        if (any(dir.names %in% dir.check) == TRUE)(stop("Directory already exists!"))
        sapply(dir.names, dir.create)
}

#create_my_dirs()

my_files <- list.files(path = "./data_raw", pattern = "\\.csv$", full.names = T )

import_claims_data <- function(my_files){
        df <- data.frame()
        library(stringr)
        the_years <- str_extract(my_files, "20[0-9][0-9]")
        for(i in 1:length(my_files)){
                x <- read.csv(my_files[i], header = F, stringsAsFactors = F,
                              strip.white = T)
                x$year <- the_years[i]
                df <- rbind(df, x)
                
        }
        df
}
#get the data
df <- import_claims_data(my_files = my_files)

#name the variables
name_the_variables <- c(
        "hospital",
        "medicaid.inpatient.days",
        "medicaid.pct.total",
        "medicare.inpatient.days",
        "medicare.pct.total",
        "commercial.inpatient.days",
        "commerical.pct.total",
        "other.inpatient.days",
        "other.pct.total",
        "total.inpatient.days",
        "year"
)
names(df) <- name_the_variables

#convert strings to numerics
df[2:ncol(df)] <- apply(df[2:ncol(df)], 2, function(x) gsub(",|%", "", x))
df[2:ncol(df)] <- apply(df[2:ncol(df)], 2, as.numeric)

#chart missing data / blank
library(Amelia)
missmap(df)
apply(df, 2, function(x) sum(is.na(x))/length(x))*100

#shorten names
df$hospital <- gsub("Hospital", "Hosp.", df$hospital)
df$hospital <- gsub("Center", "Ctr.", df$hospital)
df$hospital <- gsub("Behavioral", "Beh.", df$hospital)
df$hospital <- gsub("Regional", "Reg.", df$hospital)
df$hospital <- gsub("Rehabilitation", "Rehab.", df$hospital)
df$hospital <- gsub("Kosair Norton Hsp Kosair Chdrn Hsp Norton Healthcare Pavillion",
"Kosair Chdrn Hsp Pavillion", df$hospital)
df$hospital <- gsub("Norton Norton Hsp Kosair Chdrn Hsp Norton Healthcare Pavillion", 
     "Norton Chrdn Hsp Pavillion", df$hospital)

#see which hospitals are the same
a <- table(df$hospital)
plot(hist(a))
print(df$hospital[which(a == max(a))])



file <- "./data_tidy/2012_2015_KYOHP_Admin_Claims_tidy.csv"
write.csv(df, file = file, row.names = F)


