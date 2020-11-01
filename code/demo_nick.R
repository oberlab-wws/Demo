#Playing around with R
#Made a change on github
#R has variables
#Variables associate values with names
x <- 4
#Assignment changes the state of R
#We can also change state by loading libraries
library(tidyverse)

#We can query the "state of R" using the console
#install.packages("tidylog")
library(tidylog)
#We know that Variables in R are values that have names
#We can change the value 

metadata <- read_tsv("data/sra_metadata/SraRunTable.txt")
metadata_factor <- read.table(file = "data/sra_metadata/SraRunTable.txt",
                              sep = "\t",header = TRUE)


#Variables in R have names
#The values associated have "type"
# and "shape"

#To get "slices" of the data we can use the subset operators
insert_size <- pull(metadata,InsertSize_l)
#
lib_name <- pull(metadata,Library_Name_s)
lib_name_factor <- pull(metadata_factor,Library_Name_s)

data_vector <- as.factor(c("1","2","MISSING DATA!!!"))

str_detect(lib_name,"ZDB")

#There is another way of changing the value of a variable
#The subset operator '[' can take a number as input
insert_size[1]
insert_size[1:10]
insert_size[10:1]

# We can also use "logical vectors" to subset data
insert_is_zero <- insert_size==0
insert_isnt_zero <- insert_size != 0
insert_size[insert_is_zero]
insert_size[insert_isnt_zero]

#subsetting changes the "shape"
#of data without changing the type

new_vector <- c(a=TRUE,
                b=FALSE,
                c=TRUE)

#We can also subset our data by name
#names are strings
new_vector["b"]
#The following three are equivalent
new_vector[c("a","c")]
new_vector[c(1,3)]
new_vector[c(TRUE,FALSE,TRUE)]

lib_name[insert_size != 0]


