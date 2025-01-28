# run all queries for zorgmijding
# Make sure that you have the right database in con
# Make sure the necessary schema is created
# Make sure all function queries are run beforehand, as they don't work in this setting
library(DBI)
library(stringr)
library(readr)
library(RPostgres)
library(dplyr)
library(SPlit)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "09 Oh",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = 'Stadsrechten1340!'
)

setwd("//1906-RECHTS/Research/Care avoidance")

#Make sure they are in the right order!
queries_to_run <- c(
  #"0A. Functie - datediff.sql",
  #"0B. Functie - try cast float.sql",
  "1. Patient selection.sql",
  "2. All records K-chapter.sql",
  "2A. Exlusion DM type 1.sql",
  "3. GP contacts.sql",
  "4A. Incidence.sql",
  "4B. Prevalent cases.sql",
  "5. Measurements.sql",
  "6A. Glucose levels free text.sql",
  "6B. Cholesterol levels free text.sql",
  "6C. Blood pressure measurements.sql",
  "6D. Blood pressure free text.sql",
  "6E. Smoking status free text.sql",  
  "7A. Glucose levels.sql",
  "7B. Cholesterol levels.sql",
  "7C. Blood pressure.sql",
  "8. All medication.sql",
  "9. Medication.sql",
  "10. Medication incidence.sql"
)

for (i in 1:length(queries_to_run[1:2])){
  query <- read_file(queries_to_run[i])
  n <- str_count(query, ";")
  for (j in 1:n){
    q <- str_split_i(query, ";",j)
    dbSendQuery(con,q)
  }
}
  
for (i in 1:length(queries_to_run[3:5])){
  query <- read_file(queries_to_run[i])
  n <- str_count(query, ";")
  for (j in 1:n){
    q <- str_split_i(query, ";",j)
    dbSendQuery(con,q)
  }
}


for (i in 1:length(queries_to_run[6:8])){
  query <- read_file(queries_to_run[i])
  n <- str_count(query, ";")
  for (j in 1:n){
    q <- str_split_i(query, ";",j)
    dbSendQuery(con,q)
  }
}


for (i in 1:length(queries_to_run[9:11])){
  query <- read_file(queries_to_run[i])
  n <- str_count(query, ";")
  for (j  in 1:n){
    q <- str_split_i(query, ";",j)
    dbSendQuery(con,q)
  }
}

 for (i in 1:length(queries_to_run[12:13])){
  query <- read_file(queries_to_run[i])
  n <- str_count(query, ";")
  for (j in 1:n){
    q <- str_split_i(query, ";",j)
    dbSendQuery(con,q)
  }
}

# Medication can come later 
# for (i in 1:length(queries_to_run[14:16])){
#   query <- read_file(queries_to_run[i])
#   n <- str_count(query, ";")
#   for (j in 1:n){
#     q <- str_split_i(query, ";",j)
#     dbSendQuery(con,q)
#   }
# }