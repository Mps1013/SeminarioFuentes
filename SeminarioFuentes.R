library(readr)
desi <- read_delim("INPUT/DATA/Desigualdad (S80_S20) (CCAA).csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
ina <- read_delim("INPUT/DATA/47444.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
sui <- read_delim("INPUT/DATA/02001bsc.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
                          