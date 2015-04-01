load(url("http://connorj.github.io/Data/Phone_Specs.RData"))

# I am trying to put all the phone specs for one phone into one row.
# Example of one phones specs
phone_specs[c(1:37,39:46),]

# Change specs to a character
phone_specs$spec <- as.character(phone_specs$spec)
summary(phone_specs)

# Use Dcast
library(reshape2)
DF <- dcast(phone_specs,phone ~ feature, value.var ="spec")
head(DF)
# The dataframe contains numbers not the characters from the "spec" column.