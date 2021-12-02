# Main Script

# 1 Semantic Priming Data
# -----------------------

# 1.1 Read large Dataset of 430000 prime-target pairs

setwd("C:/Users/sebas/Desktop/Linguistics/Project/Daten")
##pt <- read.csv2("PrimingDaten.csv")
library(dplyr)
# pick 150000 random pairs and save as new csv-file
##pt <- sample_n(pt, 150000)
##write.csv2(pt, "primingDatenSmall.csv")

pt <- read.csv2("primingDatenSmall.csv")


# 1.2 Trimming priming Data

# get rid of irrelevant column an na-rows
pt[,c(1:23,28,29)] <- NULL
pt <- na.omit(pt)
# change prime to lowercase
pt[,1] <- tolower(pt[,1])
# delete RT under 200 and over 1000
pt <- pt[pt$target.RT > 200,]
pt <- pt[pt$target.RT < 1200,]
# delete RT outside Interval of 2 SD
upperLimit <- mean(pt$target.RT + 3 * sd(pt$target.RT))
lowerLimit <- mean(pt$target.RT - 3* sd(pt$target.RT))
pt <- pt[pt$target.RT > lowerLimit,]
pt <- pt[pt$target.RT < upperLimit,]

# 1.3 Get all words:

allWords <- as.factor(c(pt[,1], pt[,3]))
allWords <- levels(allWords)

# 2 Word Embedding Data
# -----------------------

# 2.1 Read Word Embedding Data
library(stringr)
setwd("C:/Users/sebas/Desktop/Linguistics/Project/Models")

emb <- read.table("GensimCBOW2Small.txt", sep = "", header = FALSE, fill = TRUE, row.names = NULL, quote = "")
colnames(emb)[1] <- "word" 

# Change Words from say_VERB to say. So later words can be compared to allWords from pt
x <- emb[,1]
x <- str_replace_all(x,"_......", "")
x <- str_replace_all(x,"_.....", "")
x <- str_replace_all(x,"_....", "")
x <- str_replace_all(x,"_...", "")
x <- str_replace_all(x,"_..", "")
x <- str_replace_all(x,"_.", "")
emb[,1] = x

# Only Rows in Embedding which also occure in prime-target Data
# IMPORTANT: Do this before PCA otherwise computer will crash due to too many words

emb <- emb[emb$word %in% allWords,]

# 2.2 Perform PCA for Dimension reduction. Column 2 seems to have non-numeric 
# value -> convert to numeric

library(MASS)
#emb[,2] <- as.numeric(emb[,2])
M <- as.matrix(emb[,-1])
M <- type.convert(M, "numeric") 
rownames(M) = emb[,1]
emb <- emb[complete.cases(emb),]
m.pca = prcomp(M, scal=TRUE, center=TRUE)

# plot first 80 words in two dimensional space. Choose PC which explain most Variance.
plot(m.pca)
plot(1, type="n", xlab="", ylab="", xlim=c(-7, 4.5), ylim=c(-7, 5))
text(m.pca$x[1:80,1], m.pca$x[1:80,2], rownames(M)[1:80])
a <- c("say", "ask", "tell", "think", "call")
b <- c("government", "service", "complany", "problem", "system")

# create new dataFrame which contains words with two PCs
wordEmb <- m.pca$x[,c(1,2)]

# 2.3 Add the two PCs to the prime-target Dataframe:
# Loop through every word in pt and find PC in embTwoDim
# If an error occurs (because word doesnt exist in embTwoDim) just keep on iterating

for(i in 1:length(pt[,1])){
  tryCatch({
    #get prime/target word
    prime = pt[i,1]
    target = pt[i,3]
    #find embedding in wordEmb and copy it to data
    pt[i,5] = wordEmb[prime,1]
    pt[i,6] = wordEmb[prime,2]
    
    pt[i,7] = wordEmb[target,1]
    pt[i,8] = wordEmb[target,2]
  }, error = function(e){}
  )
}

# 2.4 calculate euclidean distance

pt[,5] <- as.numeric(pt[,5])
pt[,6] <- as.numeric(pt[,6])
pt[,8] <- as.numeric(pt[,8])

pt[,9] <- sqrt((pt[,5]- pt[,7])^2 + (pt[,6]- pt[,8])^2)

# name the columns:
colnames(pt)[c(5,6)] = c("primePC1", "primePC2")
colnames(pt)[c(7,8)] = c("targetPC1", "targetPC2")
colnames(pt)[9] = "dist"

# delete rows with NAs
pt <- pt[complete.cases(pt),]

# turn primecond into factor
pt$primecond <- as.factor(pt$primecond)

# reduce to 111209 words by deleting rows
#pt <- pt[-(1:XXX),]



# 3 Check for Correlation
# -----------------------

# test:
finalTest <- lm(target.RT  ~ dist, pt)
summary(finalTest)

# factor
check <- lm(target.RT ~ primecond, pt)
summary(check)


pt_show <- pt[,-(5:8)]



#plot(summary, which = 1:100)