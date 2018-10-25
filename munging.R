install.packages("tidyverse")
install.packages("data.table")
library(dplyr)
library(tidyr)
library(data.table)
library(tidyverse)
library(geoclient)
library(stringr)

boros <- c("bk","bx","mn","qn","si")

## Inhale Voting Data
si_file = "VoteData/SI-Voter Data.txt"
mn_file = "VoteData/MN-Voter Data.txt"
bk_file = "VoteData/BK-Voter Data.txt"
bx_file = "VoteData/BX-Voter Data.txt"
qn_file = "VoteData/QN-Voter Data.txt"

layout <- c(9, 30, 30, 1, 4, 10, 10, 15, 50, 40, 5, 4, 50, 50, 50, 50, 8, 1, 3, 30, 3, 2, 2, 2, 2, 2, 2, 8, 2, 1, 8, 4, 20, 3, 15, 8)

si <- read.fwf(si_file, layout, header= F, fill= T, stringsAsFactors= F)

labelz <- c("County.EMSID", "Last.Name", "First.Name", "Middle.Initial", "Name.Suffix", "House.Number", "House.Number.Suffix",
            "Apartment.Number", "Street.Name", "City", "Zip.Code", "Zip.Code4", "Mailing.Address.1", "Mailing.Address.2", "Mailing.Address.3",
            "Mailing Address.4", "Birth.Date", "Gender", "Political.Party", "Other.Party", "Election.District", "Assembly.District", "Congress.District",
            "Council.District", "Senate.District", "Civil.Court.District", "Judicial.District", "Registration.Date", "Status.Code", "Voter.Type",
            "Eff.Status.Change.Date", "Year.Last.Voted", "Telephone.(optional)", "Future.Party", "Future.Other.Party", "Future.Party.Effective.Date")

colnames(si) <-labelz

# Adding Geocoding bit

geoclient_api_keys(id = "deb9acf6", key = "71d0679ec47bccb16eadb0674235f15a") 


si <- si %>% drop_na("House.Number")
si <- si %>% drop_na("Street.Name")
si <- si %>% drop_na("Zip.Code") #dropped from 283,187 to 283,156

si.s <- si %>% sample_n(150)

si.s <- si.s %>% select(House.Number, Street.Name, City, Zip.Code)

si.t <- dplyr::bind_cols(si.s, geo_address_data(si.s, house_number = House.Number, 
                                               street = Street.Name,
                                               borough = City,
                                               zip = Zip.Code))

si.A <- dplyr::bind_cols(si, geo_address_data(si, house_number = House.Number, 
                                                  street = Street.Name,
                                                  borough = City,
                                                  zip = Zip.Code))

colz <- colnames(si.t)


write.csv(si.A, file = "si_A.csv")

#BK 
bk <- read.fwf(bk_file, layout, header= F, fill= T, stringsAsFactors= F)
colnames(bk) <-labelz

bk <- bk %>% drop_na("House.Number")
bk <- bk %>% drop_na("Street.Name")
bk <- bk %>% drop_na("Zip.Code") #dropped from 1,388,601 to 1,388,147

bk.A <- dplyr::bind_cols(bk, geo_address_data(bk, house_number = House.Number, 
                                              street = Street.Name,
                                              borough = City,
                                              zip = Zip.Code))
write.csv(bk.A, file = "bk_A.csv")

#QN 
qn <- read.fwf(qn_file, layout, header= F, fill= T, stringsAsFactors= F)
colnames(qn) <-labelz

qn.before <- nrow(qn)
qn <- qn %>% drop_na("House.Number")
qn <- qn %>% drop_na("Street.Name")
qn <- qn %>% drop_na("Zip.Code") 
qn.after <- nrow(qn) #dropped from 1,126,865 to 1,126,684

qn.A <- dplyr::bind_cols(qn, geo_address_data(qn, house_number = House.Number, 
                                              street = Street.Name,
                                              #borough = City,
                                              zip = Zip.Code))
write.csv(qn.A, file = "qn_A.csv")

#MN 
'mn <- read.fwf(mn_file, layout, header= F, fill= T, stringsAsFactors= F)
colnames(mn) <-labelz'

mn.before <- nrow(mn)
mn <- mn %>% drop_na("House.Number")
mn <- mn %>% drop_na("Street.Name")
mn <- mn %>% drop_na("Zip.Code") 
mn.after <- nrow(mn) #dropped from 957,311 to 957,269

mn.A <- dplyr::bind_cols(mn, geo_address_data(mn, house_number = House.Number, 
                                              street = Street.Name,
                                              borough = City,
                                              zip = Zip.Code))
write.csv(mn.A, file = "mn_A.csv")

#BX 
bx <- read.fwf(bx_file, layout, header= F, fill= T, stringsAsFactors= F)
colnames(bx) <-labelz

bx.before <- nrow(bx)
bx <- bx %>% drop_na("House.Number")
bx <- bx %>% drop_na("Street.Name")
bx <- bx %>% drop_na("Zip.Code") 
bx.after <- nrow(bx) #dropped from 690,621 to 690,394

bx.A <- dplyr::bind_cols(bx, geo_address_data(bx, house_number = House.Number, 
                                              street = Street.Name,
                                              borough = City,
                                              zip = Zip.Code))
write.csv(bx.A, file = "bx_A.csv")




# Recovery 
start_time <- Sys.time()
for (i in boros){
  name <- paste(i, "A.csv", sep = "_")
  nam <- paste(i, "A", sep = ".")
  dset <- fread(file = name, stringsAsFactors = F)
  assign(nam, dset)
  rm(dset)
}
end_time <- Sys.time()
elapsed <- end_time - start_time
print( paste("Execution took", elapsed ,"seconds", sep = " "))

# Column checking
for (i in boros){
  name <- paste(i, "cols", sep = ".")
  dset <- paste(i, "A", sep = ".") 
  cols <- colnames(get(dset)) #needs get
  assign(name, cols, inherit = T)
}

t1 <- cbind (bx.cols, bk.cols, mn.cols, qn.cols, si.cols)

# Summarize people in NTAs
si.R <- si.A %>%
  group_by(nta) %>%
  count(Political.Party)

si.R %>% sum(NA)
NaS <- sum(is.na(get(sourz)$nta))
print( paste( NaS, "NAs in", "SI" ,sep = " "))

#Geocoding Success Looper
for (i in boros){
  output <- paste(i, "B", sep = ".")
  sourz <- paste(i, "A", sep = ".")
  NaS <- sum(is.na(get(sourz)$nta))
  print( paste( NaS, "NAs in", i, sep = " "))
  medium <- (get(sourz)) %>%
    group_by(nta) %>%
    count(Political.Party)
  assign(output, medium, inherit = T)
}
# 732 NAs in bk, 894 in bx, 1327 in mn, 793 in qn, 99 in si

for (i in boros){
  output <- paste(i, "C", sep = ".")
  sourz <- paste(i, "B", sep = ".")
  medium <- sourz %>% filter(str_detect(i))
  assign(output, medium, inherit = T)
}

# Found a lot of failed geocoded NTAs in Queens (>1M) - invalid boro code, looks
##  like there's a bunch of "cities" i.e. Breezy Point in the data
More queens check
QNS1 <- sample_n(qn.A, 500)
##
##

#qn recheck
'qn2 <- read.fwf(qn_file, layout, header= F, fill= T, stringsAsFactors= F)
colnames(qn2) <-labelz
qn2.before <- nrow(qn2)
qn2 <- qn2 %>% drop_na("House.Number")
qn2 <- qn2 %>% drop_na("Street.Name")
qn2 <- qn2 %>% drop_na("Zip.Code")
qn2.after <- nrow(qn2) #dropped from XX to YY
remove(qn2)'


## Documenting geoclient bug
'library(geoclient)
library(dplyr)
x1 <- c(13, 289, 476)
x2 <- c("BAILEY PLACE", "RIDGEWOOD AVENUE", "MOUNTAINVIEW AVENUE")
x3 <- c("STATEN ISLAND", "STATEN ISLAND", "STATEN ISLAND")
x4 <- c(10303, 10312, 10314)
xx <- data.frame(x1,x2,x3,x4)

xtest <- dplyr::bind_cols(xx, geo_address_data(xx, house_number = x1, 
street = x2,
borough = x3,
zip = x4))
glimpse(xtest)
reprex()'