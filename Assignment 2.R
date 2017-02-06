install.packages("ggplot2")
install.packages("plyr")
install.packages("choroplethr")
install.packages("dplyr")

library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)

# To be more general, I copy Karl's code on grasping data from an url. I still grasp all the 
# newest data in order to have more manipulation opions.
# In this case, I am extremely curious about the quality distribution of bridge in the United 
# States and further investigate some details about the best quality state.

# Firstly abstract data from different urls and store it in a tidy data frame.

class.abstract = "https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/AK16.txt"
tmp = read_csv(class.abstract)
classes = sapply(tmp, class) # store indice for reading data below.

states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),] # delete Canadian states.
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")

dest= rep("", 52)
for(i in 1:52){
  dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2016/delimited/", states[i,2],"16.txt", sep = "") 
}
d2016 = ldply(dest, fread, colClasses = classes)
save(d2016, file = "2016_bridges_data.RData") # accelerate loading when running code again.

# Now search out the quality distribution.

D = d2016
keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008", "COUNTY_CODE_003", "SERVICE_LEVEL_005C",
         "LAT_016", "LONG_017", "ADT_029", "YEAR_ADT_030", "YEAR_BUILT_027", 
         "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060", "CHANNEL_COND_061",
         "CULVERT_COND_062")

# create a function that return processable data frame.

clear = function(x) {
  return(which(x>20))
}
bad = is.na(D) %>% colSums %>% clear
D = D[, -bad]

# Transfer data into tibble and select the targeted columns.

D = as.tbl(D)
T = select(D, one_of(keep))
T = filter(T, LAT_016 > 0 & LONG_017 > 0)

# create a transformation function for longtitude and latitude.

tran.lat = function(x) {
  t = as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05
  return(t)
}

tran.long = function(x) {
  t = as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05
  change = which(t < 20)
  x.change = x[change]
  t.change = as.numeric(substr(x.change,1,3)) + as.numeric(substr(x.change,4,9))/6e+05
  t[change] = t.change
  return(t)
}

T$LAT_016 = tran.lat(T$LAT_016)
T$LONG_017 = tran.long(T$LONG_017)

T = filter(T, LAT_016 > 24.3 & LAT_016 < 49.5 & LONG_017 > 66 & LONG_017 < 125)

# Simply generate a U.S Map! But take time.(Inverse the longtitude and you'll get a 
# right shape)

ggplot(data = T) +geom_point(mapping = aes(y = LAT_016, x = -LONG_017))

# make function to rate bridge as NA, good, bad, fail, using 
# good = 5:9
# bad = 2:4
# fail = 0:1
# Copy rate function from Karl.

# Take the minimum number among several index to represent bridge condition.

T = mutate(T, cond = pmin(SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062))

rate = function(cond){
  # gives a good to fail rating for cond.
  rate = rep("good", length(cond))
  rate[cond<5] = "bad"
  rate[cond <2]= "fail"
  return(rate)
}

T$rate = rate(T$cond)

# generate bridge quality distribtion.

pdf("US_Map.pdf",width=6,height=4,paper='special') 
ggplot(data = T) +geom_point(mapping = aes(y = LAT_016, x = -LONG_017, col = cond))
dev.off()

ggplot(data = T) +geom_point(mapping = aes(y = LAT_016, x = -LONG_017, col = rate))

# By observation, bridges' conditions in Minnestoa is the worst. Then I will take it
# out and further investigate it.

min = filter(T, STATE_CODE_001 == 27)

# Minnesota road types distribution

pdf("MIN_road_type_distribution.pdf",width=6,height=4,paper='special') 
ggplot(data = min) +geom_point(mapping = aes(y = LAT_016, x = -LONG_017, col = SERVICE_LEVEL_005C))
dev.off()

ggplot(data = min) +geom_point(mapping = aes(y = log(ADT_029), x = YEAR_BUILT_027, col = rate))

# Road types built during the years

pdf("MIN_road_type_built.pdf",width=6,height=4,paper='special') 
ggplot(data = min) +geom_point(mapping = aes(y = SERVICE_LEVEL_005C, x = YEAR_BUILT_027, col = rate))
dev.off()

# From the 