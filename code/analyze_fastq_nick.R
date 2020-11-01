#playing around with fastq files
#What do fastq files actually look like?
library(tidyverse)
fastq_file <- "data/untrimmed_fastq/SRR097977.fastq"
fastq_data <- read_lines(fastq_file)
#string matching using logical subsetting might be tricky

seq_index <- seq(from=2,
                 by=4,
                 length.out = length(fastq_data)/4)
quality_index <- seq(from=4,
                 by=4,
                 length.out = length(fastq_data)/4)



seq_data <- fastq_data[seq_index]
qual_data <- fastq_data[quality_index]

#Pull out all the sequences with the letter 'N'
#from seq_data and qual_data
# Total number of reads that contain the letter 'N'
contains_N <- str_detect(seq_data,"N")
seq_N <- seq_data[contains_N]
count_N <- str_count(seq_data,"N")

#Lists in R
#variables have both shape and type
#vectors in R have a 1d shape and all elements have the 
#same type

#Variables that are lists can have elements with different types

#Quality scores are ascii (UTF8)
#We can conver to integer using utf8ToInt
qual_1 <- utf8ToInt(qual_data[1])
qual_2 <- utf8ToInt(qual_data[2])
qual_l <- map(qual_data,utf8ToInt)
mean(qual_l[[1]])
mean(qual_l[[2]])
qual_mean <- map(qual_l,mean)
qual_mean <- map_int(qual_l,mean)

qual_length <- map_dbl(qual_l,length)



#map the length function over qual_l 

# 
# source("fastq2csv.R")
# fastq_df <- read_tidy_fastq(fastq_file,max_reads = Inf)
