# NOTE: This script does NOT do the Fast Fourier Transform (FFT). See Github folder called "FFT After LCPC" for the script for doing the FFT.


# Script for LCPC Algorithm (Calculuting Distances Between Points 
# and Summing All Distances That Are on the Same Radial Line)

# By David H. Nguyen, PhD www.TSG-Lab.org



# Start of ReadMe Info

######### HOW TO FORMAT THE DATA FILE FOR THIS SCRIPT ###########
# 
# Note: There are sample data files and sample data outputs in the 
#    Github folder that contains this script. 
#
# 1. The column containing x-coordinates must be titled "x"
# 2. The column containing y-coordinates must be titled "y"
# 3. The column contaning the measure of the angle must be titled "angle"
# 4. The column containing the x-coordinate of the origin must be titled "Xo"
# 5. The column containing the y-coordinate of the origin must be titled "Yo"
# 6. There should be no missing items in the columns for x, y, and angle.
#
# This script will produce two files.
#   1. The first file is called "lcpc_data.csv". This is the file you will use
#      for the FFT script.
#   2. The second file is called "distance_calculation.csv" and is the result 
#      the calculations done by this script, EXCEPT for adding all 
#      distances of the same angle together.
#
# End of ReadMe Info

#####

# Required packages
install.packages('dplyr')
library(dplyr)
install.packages('tidyr')
library(tidyr)


#####
df = read.csv("sample_data.csv")

#View(df)
###### Formatting the data before we do calculations

# Identify the x coordinate of the origin
x.origin = df$Xo[1]
# Identify the y coordinate of the origin
y.origin = df$Yo[1]

# Create vector of length of df's rows, each has the value in x.origin
orig.x.vec = rep(x.origin, times = dim(df)[1]) 
# Create vector of length of df's rows, each has the value in y.origin
orig.y.vec = rep(y.origin, times = dim(df)[1]) 

# Add above vectors as new columns to df
df = cbind(df, orig.x.vec)
df = cbind(df, orig.y.vec)


#########
#########

###### Doing the distance formula for two points

# Calculate distance formula for each coordinate pair in the columns 'x' and 'y' 
#    to the first row of columns 'Xo' and 'Yo' 
#    This is the (x-Xo)^2 term

df = mutate(df, x.part = x - orig.x.vec)
df = mutate(df, x.partSq = x.part * x.part)

df = mutate(df, y.part = y - orig.y.vec)
df = mutate(df, y.partSq = y.part * y.part)

# Sum each row of columns called x.partSq and y.partSq
df = mutate(df, sum.of.sq = x.partSq + y.partSq)

# Sum each row of columns called x.partSq and y.partSq
sq.root = summarise(df, sqrt.of.sum = sqrt(sum.of.sq))

# This function takes the square root of its input
take.sqrt = function (n){
  sqrt(n)
}

# This takes the square root of each row in column df#sum.of.sq
sqrt.of.sum = sapply(df$sum.of.sq, take.sqrt)

# Add sqrt.of.sum as new column to df
df = cbind(df, sqrt.of.sum)


#########
#########
# Isolate each distance in sqrt.of.sum according to it's value in the column 'angle'

# Identify all unique values in df$angle
uniq.angles = unique(df$angle)
uniq.angles


write.commands = function (n){
  paste0("group", n, "= filter(df, angle==", n,")")
}

execute.me = lapply(uniq.angles, write.commands)

final.run = for (i in execute.me){
  eval(parse(text=i))
}


part1 = rep("group", times = length(uniq.angles))
part2 = uniq.angles
uniq.names = paste0(part1,part2)


### The following lines sum all distances in each "groupXX" data frame

summing = function(n){
  paste0(n, " = summarise(", n, ", sum(sqrt.of.sum))")
}
# Example output: "group135 = summarise(group135, sum(sqrt.of.sum))" 

# "container" contains the commands that need to be executed
container = sapply(uniq.names, summing)

for (i in container){
  eval(parse(text=i))
}

###


##########
# create function that rbinds all groups into one data frame

basket = c()

for (i in uniq.names){
  basket = append(basket, i)
}

basket = as.data.frame(basket) # turn header into a data frame
basket = t(basket) # transpose this data frame into one horizontal row
basket = as.data.frame(basket) # make basket into a data frame again b/c t() turned it into a matrix


# merge all items in basket into one cell, separating each item by a comma
combined = unite(basket, "groups", c(colnames(basket)), sep = ",")

header = "bind.them = rbind("
tailer = ")"

text2execute = paste0(header, combined, tailer)

bound.stuff = eval(parse(text = text2execute))

named.bound = cbind(uniq.angles, bound.stuff) # this adds the name of the angles to bound.stuff
names(named.bound) = c("angle", "summedDistance") # rename the column headers
#View(named.bound)


# This object is for adding adding in angle groups that are not
# in bound.stuff. There should be 37 groups total, even if 
# groups are empty. 
all.possible.angles = seq(0, 180, by = 5)

diff.angles = is.element(all.possible.angles, uniq.angles)
#View(diff.angles)

absent.angles.marked = cbind(diff.angles, all.possible.angles)
absent.angles.marked = as.data.frame(absent.angles.marked)

isolated.absents = filter(absent.angles.marked, grepl(0, diff.angles))
#View(isolated.absents)
isolated.absents = isolated.absents[c(2,1)] # rearrange the order of columns
names(isolated.absents) = c("angle", "summedDistance") 

final.df = rbind(isolated.absents, named.bound) # combine

final.df = arrange(final.df, angle)

#View(final.df)


##################################
# Saving the data as files

write.csv(df, "distance_calculation.csv", row.names = F)

write.csv(final.df, "lcpc_data.csv", row.names = F)



