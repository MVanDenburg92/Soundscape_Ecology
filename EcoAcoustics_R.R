install.packages("soundecology")
install.packages("seewave")
install.packages("tuneR")
install.packages("tidyverse")



library(tidyverse)
library(soundecology)
library(tuneR)
library(seewave)


#https://cran.r-project.org/web/packages/soundecology/vignettes/intro.html
#https://marce10.github.io/2019/01/12/phylo_spectro_function.html
## https://cran.r-project.org/web/packages/soundecology/soundecology.pdf

setwd("D:/RA-withSangermano/MA-Acoustic-DATA/4to6am")


#create single plot spectrogram from wav
sound_raster(wavfile = "CC_20190702_060000.wav",
             max_freq=10000)
## End Plot Spectrogram


#Calcualte Spectrogram for all files in folder
#read all wavs as vector
allwavs1 <- list.files(pattern = "\\.wav$")

#loop through files
for (i in 1:length(allwavs1)) {
  sound_raster(wavfile = allwavs1[i], max_freq=10000)
}
## End Plot Spectrogram for all files



## calculate indices independently using waveobjects  
#Load single file as a waveobject object called soundfile
soundfile <- readWave("CC_20190702_060000.wav", from = 0, to = 60, units = "seconds")

#Run the function on this waveobject and save the results in a new variable called "soundfile.aci"
soundfile.NDSI <- ndsi(soundfile, fft_w = 512, anthro_min = 1000, anthro_max = 2000,
                       bio_min = 2000, bio_max = 11000)
#Print the NDSI value for the left channel of the wav file, stored in soundfile.aci
print(soundfile.NDSI$anthrophony_left)
print(soundfile.NDSI$biophony_left)
#Save all results as csv
write.csv(soundfile.NDSI,'soundfile.ndsi.csv', append = TRUE)
## End Calc indices


# Loop calculate NDSI indices for all files in folder
#read all wavs as vector
allwavs1 <- list.files(pattern = "\\.wav$")

#create initial outputfile

#loop through files
for (i in 1:length(allwavs1)){
  
  #Load all files from vector as a waveobject called soundfile
  soundfile <- readWave(allwavs1[i], from = 0, to = 60, units = "seconds")
  #Calcualte index -- for other indices replace this line of code with notes below
  soundfile.NDSI <- ndsi(soundfile, fft_w = 512, anthro_min = 1000, anthro_max = 2000,
                         bio_min = 2000, bio_max = 11000)
  #write index as csv
  write.table(soundfile.NDSI, file = 'soundfile_ndsi1.csv', append = TRUE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = allwavs1[i], col.names = NA)
  
}
#remove extra col titles

dat <- read.csv("soundfile_ndsi1.csv")
toDelete <- seq(1, nrow(dat), 2)
data<- dat[ toDelete ,]

write.table(data, file = 'soundfile_ndsisp1_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)

## delete columns with NA
cols.dont.want <- c("ndsi_right", "biophony_right", "anthrophony_right") # if you want to remove multiple columns

data <- data[, ! names(data) %in% cols.dont.want, drop = F]
write.table(data, file = 'soundfile_ndsisp1_NA_removed_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)


#Fix the rownames in the dataset using 
#1:the dimensions of the dataset(dim(data))[1]

rownames(data) = 1:dim(data)[1]







#Acoustic Complexity Index - ACI
#soundfile.ACI <- acoustic_complexity(soundfile, fft_w = 512, anthro_min = 1000, anthro_max = 2000,
#bio_min=2000, bio_max = 11000)


# Loop calculate ASI indices for all files in folder
#read all wavs as vector
allwavs2 <- list.files(pattern = "\\.wav$")

#create initial outputfile

#loop through files
for (i in 1:length(allwavs2)){
  
  #Load all files from vector as a waveobject called soundfile
  soundfile <- readWave(allwavs2[i], from = 0, to = 60, units = "seconds")
  #Calcualte index -- for other indices replace this line of code with notes below
  soundfile_ACI <- acoustic_complexity(soundfile, fft_w = 512, min_freq=2000, max_freq = 11000)
  #write index as csv
  write.table(soundfile_ACI, file = 'soundfile_ACI.csv', append = TRUE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = allwavs2[i], col.names = NA)
}
#remove extra col titles

dat2 <- read.csv("soundfile_ACI.csv")
toDelete <- seq(1, nrow(dat2), 2)
dataACI<- dat2[ toDelete ,]

write.table(data, file = 'soundfile_ACIsp1_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)

## delete columns with NA
cols.dont.want <- c("aci_right", "biophony_right", "anthrophony_right") # if you want to remove multiple columns

data2 <- dataACI[, ! names(dataACI) %in% cols.dont.want, drop = F]
write.table(dataACI, file = 'soundfile_ACI_NA_removed_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)


#Fix the rownames in the dataset using 
#1:the dimensions of the dataset(dim(data))[1]

rownames(dataACI) = 1:dim(dataACI)[1]





# 2- Bioacoustic Index - BI      
#  soundfile.BI <- bioacoustic_index(soundfile, fft_w = 512, min_freq=2000, max_freq = 8000)



# Loop calculates BI indices for all files in folder

#create initial outputfile

#loop through files

allwavs2 <- list.files(pattern = "\\.wav$")

for (i in 1:length(allwavs2)){
  
  #Load all files from vector as a waveobject called soundfile
  soundfile <- readWave(allwavs2[i], from = 0, to = 60, units = "seconds")
  #Calcualte index -- for other indices replace this line of code with notes below
  soundfile_BI <- bioacoustic_index(soundfile, fft_w = 512, min_freq=2000, max_freq = 8000)
  #write index as csv
  write.table(soundfile_BI, file = 'soundfile_BI.csv', append = TRUE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = allwavs2[i], col.names = NA)
}


#remove extra row titles

dat3 <- read.csv("soundfile_BI.csv")
toDelete <- seq(1, nrow(dat3), 2)
dataBI<- dat3[toDelete,]

write.table(dataBI, file = 'soundfile_BI_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)

## delete columns with NA
cols.dont.want <- c("right_area") # if you want to remove multiple columns

dataBI_Cleaned <- dataBI[, !names(dataBI) %in% cols.dont.want, drop = F]
write.table(data3, file = 'soundfile_BI_removed_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)


#Fix the rownames in the dataset using 
#1:the dimensions of the dataset(dim(data))[1]

rownames(dataBI_Cleaned) = 1:dim(dataBI_Cleaned)[1]






# 3- Acoustic Diversity Index - ADI
# acoustic_diversity(soundfile, max_freq = 10000, db_threshold = -50, freq_step = 1000, shannon = TRUE)


# Loop calculates BI indices for all files in folder

#create initial outputfile

#loop through files


allwavs2 <- list.files(pattern = "\\.wav$")


for (i in 1:length(allwavs2)){
  
  #Load all files from vector as a waveobject called soundfile
  soundfile <- readWave(allwavs2[i], from = 0, to = 60, units = "seconds")
  #Calcualte index -- for other indices replace this line of code with notes below
  soundfile_ADI <- acoustic_diversity(soundfile, max_freq = 10000, db_threshold = -50, freq_step = 1000, shannon = TRUE)
  #write index as csv
  
  write.table(soundfile_ADI, file = 'soundfile_ADI.csv', append = TRUE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = allwavs2[i], col.names = NA)
}


#remove extra row titles

datADI <- read.csv("soundfile_ADI.csv")
toDelete <- seq(1, nrow(datADI), 2)
dataADI<- datADI[toDelete,]

write.table(dataADI, file = 'soundfile_ADI_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)

## delete columns with NA
cols.dont.want <- c("right_area") # if you want to remove multiple columns

dataADI_Cleaned <- dataADI[, !names(dataADI) %in% cols.dont.want, drop = F]
write.table(dataADI, file = 'soundfile_ADI_removed_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)


#Fix the rownames in the dataset using 
#1:the dimensions of the dataset(dim(data))[1]

rownames(dataADI_Cleaned) = 1:dim(dataADI_Cleaned)[1]









# 4- Acoustic Evenness - AEI
#acoustic_evenness(soundfile, max_freq = 10000, db_threshold = -50, freq_step = 1000)


allwavs2 <- list.files(pattern = "\\.wav$")


for (i in 1:length(allwavs2)){
  
  #Load all files from vector as a waveobject called soundfile
  soundfile <- readWave(allwavs2[i], from = 0, to = 60, units = "seconds")
  #Calcualte index -- for other indices replace this line of code with notes below
  soundfile_AEI <- acoustic_evenness(soundfile, max_freq = 10000, db_threshold = -50, freq_step = 1000)
  #write index as csv
  
  write.table(soundfile_AEI, file = 'soundfile_AEI.csv', append = TRUE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = allwavs2[i], col.names = NA)
}


#remove extra row titles

datAEI <- read.csv("soundfile_AEI.csv")
toDelete <- seq(1, nrow(datAEI), 2)
dataAEI<- datAEI[toDelete,]

write.table(dataAEI, file = 'soundfile_AEI_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)

## delete columns with NA
cols.dont.want <- c("aei_right") # if you want to remove multiple columns

dataAEI_Cleaned <- dataAEI[, !names(dataADI) %in% cols.dont.want, drop = F]
write.table(dataAEI, file = 'soundfile_AEI_removed_clean.csv', append = TRUE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = T, col.names = NA)


#Fix the rownames in the dataset using 
#1:the dimensions of the dataset(dim(data))[1]

rownames(dataAEI_Cleaned) = 1:dim(dataAEI_Cleaned)[1]





#Subsetting the data list of NDSI sites by site of recording

data_BMB_NDSI <- soundfile_ndsi_clean[c(1:99),]
data_BP_NDSI <- soundfile_ndsi_clean[c(100:189),]
data_CC_NDSI <- soundfile_ndsi_clean[c(190:279),]
data_FR_NDSI <- soundfile_ndsi_clean[c(280:351),]
data_L_NDSI <- soundfile_ndsi_clean[c(352:423),]
data_PM_NDSI <- soundfile_ndsi_clean[c(424:512),]
data_RB_NDSI <- soundfile_ndsi_clean[c(513:602),]
data_W_NDSI <- soundfile_ndsi_clean[c(603:674),]
data_WM_NDSI <- soundfile_ndsi_clean[c(675:755),]


#Subsetting the data list of ACI sites by site of recording

data_BMB_ACI <- soundfile_ACI_clean[c(1:99),]
data_BP_ACI <- soundfile_ACI_clean[c(100:189),]
data_CC_ACI <- soundfile_ACI_clean[c(190:279),]
data_FR_ACI <- soundfile_ACI_clean[c(280:351),]
data_L_ACI <- soundfile_ACI_clean[c(352:423),]
data_PM_ACI <- soundfile_ACI_clean[c(424:512),]
data_RB_ACI <- soundfile_ACI_clean[c(513:602),]
data_W_ACI <- soundfile_ACI_clean[c(603:674),]
data_WM_ACI <- soundfile_ACI_clean[c(675:755),]


#Subsetting the data list of ADI sites by site of recording

data_BMB_ADI <- soundfile_ADI_clean[c(1:99),]
data_BP_ADI <- soundfile_ADI_clean[c(100:189),]
data_CC_ADI <- soundfile_ADI_clean[c(190:279),]
data_FR_ADI <- soundfile_ADI_clean[c(280:351),]
data_L_ADI <- soundfile_ADI_clean[c(352:423),]
data_PM_ADI <- soundfile_ADI_clean[c(424:512),]
data_RB_ADI <- soundfile_ADI_clean[c(513:602),]
data_W_ADI <- soundfile_ADI_clean[c(603:674),]
data_WM_ADI <- soundfile_ADI_clean[c(675:755),]

#Subsetting the data list of AEI sites by site of recording

data_BMB_AEI <- soundfile_AEI_clean[c(1:99),]
data_BP_AEI <- soundfile_AEI_clean[c(100:189),]
data_CC_AEI <- soundfile_AEI_clean[c(190:279),]
data_FR_AEI <- soundfile_AEI_clean[c(280:351),]
data_L_AEI <- soundfile_AEI_clean[c(352:423),]
data_PM_AEI <- soundfile_AEI_clean[c(424:512),]
data_RB_AEI <- soundfile_AEI_clean[c(513:602),]
data_W_AEI <- soundfile_AEI_clean[c(603:674),]
data_WM_AEI <- soundfile_AEI_clean[c(675:755),]

#Subsetting the data list of BI sites by site of recording

data_BMB_BI <- soundfile_BI_clean[c(1:99),]
data_BP_BI <- soundfile_BI_clean[c(100:189),]
data_CC_BI <- soundfile_BI_clean[c(190:279),]
data_FR_BI <- soundfile_BI_clean[c(280:351),]
data_L_BI <- soundfile_BI_clean[c(352:423),]
data_PM_BI <- soundfile_BI_clean[c(424:512),]
data_RB_BI <- soundfile_BI_clean[c(513:602),]
data_W_BI <- soundfile_BI_clean[c(603:674),]
data_WM_BI <- soundfile_BI_clean[c(675:755),]




#convert dataframes to numeric


#Code below mass converts every column in the data to factor but removes the title data in the process
# data_NDSI <-mutate_if(data[, 2:4], is.factor, ~ as.numeric(as.character(.x)))
# 
# data_NDSI 
# 
# 
# indx <- sapply(data[, 2:4], is.factor)
# data[indx] <- lapply(data[indx], function(x) as.numeric(as.character(x)))

dataAEI[,'aei_left'] <- as.numeric(as.character(dataAEI[,'aei_left']))

cleaned_Data_AEI <- dataAEI
cleaned_Data_NDSI <- soundfile_ndsi_clean
cleaned_Data_BI <- soundfile_BI_removed_clean



##Create Histograms for datasets

hist(c(data_BMB$ndsi_left))

hist(c(data_BP$ndsi_left))

hist(c(data_CC$ndsi_left))

hist(c(data_FR$ndsi_left))

hist(c(data_L$ndsi_left))

hist(c(data_PM$ndsi_left))

hist(c(data_RB$ndsi_left))

hist(c(data_W$ndsi_left))

hist(c(data_WM$ndsi_left), breaks = )



##Test correlation for the NDSI and Biophony fields

x <- c(data$biophony_left)
y <- c(data$ndsi_left)

cor.test(x, y)



##Test correlation for the NDSI and Anthrophony fields

x <- c(data$anthrophony_left)
y <- c(data$ndsi_left)

cor.test(x, y)




#Plot Code

#Just as we used the mean and standard deviation to summarize a single variable,
#we can summarize the relationship between these two variables by finding the
#line that best follows their association. 
#Use the following interactive function to select the line that you think does
#the best job of going through the cloud of points.



#After running this command, you’ll be prompted to click two points on the plot
#to define a line. Once you’ve done that, the line you specified will be shown
#in black and the residuals in blue. Note that there are 30 residuals,
#one for each of the 30 observations. 
#Recall that the residuals are the difference between the observed values 
#and the values predicted by the line:

#ei=yi−y^i

#The most common way to do linear regression is to select the line that 
#minimizes the sum of squared residuals. To visualize the squared residuals,
#you can rerun the plot command and add the argument showSquares = TRUE.



plot_ss(x = mlb11$at_bats, y = mlb11$runs, showSquares = TRUE))

#Note that the output from the plot_ss function provides you with the slope
#and intercept of your line as well as the sum of squares.


#Linear Model Code: 

m1 <- lm(runs ~ at_bats, data = mlb11)


#The first argument in the function lm is a formula that takes the form y ~ x. 
#Here it can be read that we want to make a linear model of runs as a function 
#of at_bats. The second argument specifies that R should look in the mlb11 data 
#frame to find the runs and at_bats variables.

#The output of lm is an object that contains all of the
#information we need about
#the linear model that was just fit. We can access this information using
#the summary function.

summary(m1)


#Prediction and prediction errors
#Let’s create a scatterplot with the least squares line laid on top.

plot(mlb11$runs ~ mlb11$at_bats)
abline(m1)


#To assess whether the linear model is reliable, we need to check for (1)
#linearity, (2) nearly normal residuals, and (3) constant variability.

#Linearity: You already checked if the relationship between runs and at-bats 
#is linear using a scatterplot. We should also verify this condition with a plot
#of the residuals vs. at-bats. Recall that any code following a 
# is intended to be a comment that helps understand the code but is ignored by R.

plot(m1$residuals ~ mlb11$at_bats)
abline(h = 0, lty = 3)  # adds a horizontal dashed line at y = 0



#Nearly normal residuals: To check this condition, we can look at a histogram

hist(m1$residuals)

#or a normal probability plot of the residuals.

qqnorm(m1$residuals)
qqline(m1$residuals)  # adds diagonal line to the normal prob plot











#2.1.4 Subsetting and replacement with dplyr
#Now that we have seen how to index/subset data.frames, we’ll look at how that 
#is done with dplyr. In a word, it is quite different. First, read about the 
#dplyr grammar, which provides a set of “verbs” that are designed to replace 
#many of the base R approaches for manipulating data.frames, including how you
#index them (you may also wish to read the chapter on data transformation in
#R For Data Science).

#Here we will focus on just indexing and replacement, using a slighly larger
#version of d, noting the dplyr works on data.frames as well as tibble:
  
#   # Chunk 1
#   set.seed(1)
# d <- data.frame(a = letters[1:7], b = 1:7, c = runif(n = 7, min = 0, max = 20))
# d
# #>   a b         c
# #> 1 a 1  5.310173
# #> 2 b 2  7.442478
# #> 3 c 3 11.457067
# #> 4 d 4 18.164156
# #> 5 e 5  4.033639
# #> 6 f 6 17.967794
# #> 7 g 7 18.893505
# #
# # #1
# d %>% filter(a %in% c("a", "e")) %>% select(a, b)
# #>   a b
# #> 1 a 1
# #> 2 e 5
# #
# # #2
# d %>% filter(c > 7 & c < 18) %>% select(-b)
# #>   a         c
# #> 1 b  7.442478
# #> 2 c 11.457067
# #> 3 f 17.967794
# #
# # #3
# d %>% filter(a == "c")
# #>   a b        c
# #> 1 c 3 11.45707
# # 
# # 4
# d %>% slice(c(1:2, 7))
# #>   a b         c
# #> 1 a 1  5.310173
# #> 2 b 2  7.442478
# #> 3 g 7 18.893505

# A bunch of new stuff up there, which is first noticeable in #1:
#


# First, there’s the %>%, which is the “pipe” operator, which dplyr imports from 
# magrittr (a tidyverse package). It passes (or pipes) whatever is on the 
# lefthand side to the operation defined on the right-hand side, which allows one
# to chain together multiple operations in a single command sequence

# We pipe d to dplyr’s filter function, which is used to find rows based on their
# values. We use the same sort of logical indexing syntax as in our previous
# subsetting examples, in this case looking for values “a” and “e” in column a.
# However, one difference is that we don’t have to wrap a in quotes. This is a 
# feature of dplyr functions, which makes coding more efficient

# Having found the matching rows, we then narrow our selection to just columns a 
# and b by using the select function to pull the columns we want. Note that we
# don’t have to wrap a and b in quotes, or within a c()


# In #2 we see how we find values in c that fall between 7 and 18, and then 
# select columns a and c by negative reference on b. Note that dplyr::select 
# the negative reference to be applied right to the column name, which you can’t 
# do in a matrix or data.frame
# 
# In #3 using filter without select simply returns the matching row across all 
# columns. #4 introduces the slice function, which lets us select by row number.












#NDVI

##Correlation between anthrophony and NDVI for each fucking site and buffer in the site


##Correlation between Biophony and NDVI for each fucking site and buffer in the site


##Correlation between NDSI and NDVI for each fucking site and buffer in the site

##Correlation between ACI and NDVI for each fucking site and buffer in the site

##Correlation between Bi and NDVI for each fucking site and buffer in the site

##Correlation between ADI and NDVI for each fucking site and buffer in the site

##Correlation between AEI and NDVI for each fucking site and buffer in the site




##OPTIONAL: Linear Regression NDVI vs NDSI at 3000






#NDBI

##Correlation between anthrophony and NDBI for each fucking site and buffer in the site


##Correlation between Biophony and NDBI for each fucking site and buffer in the site


##Correlation between NDSI and NDBI for each fucking site and buffer in the site

##Correlation between ACI and NDBI for each fucking site and buffer in the site

##Correlation between Bi and NDBI for each fucking site and buffer in the site

##Correlation between ADI and NDBI for each fucking site and buffer in the site

##Correlation between AEI and NDBI for each fucking site and buffer in the site


##OPTIONAL: Linear Regression NDBI vs NDSI at 3000







## End Calculate inidces for all files      


#Note- Indices

# 1- Acoustic Complexity Index - ACI
#soundfile.ACI <- acoustic_complexity(soundfile, fft_w = 512, min_freq=2000, max_freq = 11000)
# 2- Bioacoustic Index - BI      
#  soundfile.BI <- bioacoustic_index(soundfile, fft_w = 512, min_freq=2000, max_freq = 8000)
# 3- Acoustic Diversity Index - ADI
# acoustic_diversity(soundfile, max_freq = 10000, db_threshold = -50, freq_step = 1000, shannon = TRUE)
# 4- Acoustic Evenness - AEI
#acoustic_evenness(soundfile, max_freq = 10000, db_threshold = -50, freq_step = 1000)





##Run Batch Indices Example -- these indices do not save complete output table
## https://cran.r-project.org/web/packages/soundecology/soundecology.pdf

multiple_sounds(directory = "E:/Flor/MA-Acoustic-DATA/BroadMeadowBrook-2/Data",
                resultfile = "E:/Flor/MA-Acoustic-DATA/NDSI_BMB_default.csv",
                soundindex = "ndsi",  fft_w = 1024, anthro_min = 1000, anthro_max = 2000,bio_min = 2000, bio_max = 11000)

multiple_sounds(directory = "E:/Flor/MA-Acoustic-DATA/BroadMeadowBrook-2/Data",
                resultfile = "E:/Flor/MA-Acoustic-DATA/ACI_BMB_2000-12000.csv",
                soundindex = "acoustic_complexity",  fft_w = 1024, max_freq = 12000, min_freq = 2000)

multiple_sounds(directory = "E:/Flor/MA-Acoustic-DATA/4to6am",
                resultfile = "E:/Flor/MA-Acoustic-DATA/4to6am/acoustic_diversity1.csv",
                soundindex = "acoustic_diversity",  from = 0, to = 60, units = "seconds", max_freq = 10000, db_threshold = -50,
                freq_step = 1000, shannon = TRUE)

multiple_sounds(directory = "E:/Flor/MA-Acoustic-DATA/4to6am",
                resultfile = "E:/Flor/MA-Acoustic-DATA/4to6am/biacoustic_index.csv",
                soundindex = "bioacoustic_index",  from = 0, to = 60, units = "seconds", min_freq= 2000, max_freq = 10000, fft_w = 512)

multiple_sounds(directory = "E:/Flor/MA-Acoustic-DATA/4to6am",
                resultfile = "E:/Flor/MA-Acoustic-DATA/4to6am/Entropy_index.csv",
                soundindex = "H",  from = 0, to = 60, units = "seconds")
# Multiple SOunds  
