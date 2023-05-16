######################## TVIV Risk Factors #########################


# Initiate -----

## Load packages ------
library(dplyr)
library(readxl)

## Set path -----
path <- "/Users/clara/Desktop/Masterarbeit/endireh_documents/2021/data/"

## Load data -------
load(paste0(path,"TVIV.RData"))

# Risk Factors -----

## AVERAGE NUMBER OF HOUSEHOLD MEMBERS PER ROOM IN THE DWELLING -------
# Variable name: hacin
# Questions: P1_7 (how many people in the dwelling) and P1_2 (how many sleeping rooms)
# Outcome: 01 - 99 Persons living in the dwelling (included are children and elder people)
#          01 - 30 Rooms to sleep
# Level: numeric
TVIV$hacin <- as.numeric(as.character(TVIV$P1_7))/as.numeric(as.character(TVIV$P1_2))
head(TVIV[, c("P1_7", "P1_2", "hacin")], n = 35)
# Summary Stat:
table(TVIV$hacin, useNA = "ifany")
summary(TVIV$hacin)









