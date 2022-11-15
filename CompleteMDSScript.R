library(dplyr)

df <-read.csv("/Users/EricElias/Desktop/MDS_R_Code/RawMDSStudy2.csv", header=TRUE)
FaceCodes <- read.csv("/Users/EricElias/Desktop/MDS_R_Code/MDSFaceCodes.csv", header = FALSE)
FaceCodes <- FaceCodes[-c(1,2),]
###Cleaning Phase Begins
###Numbering each trial for each participant
groupedid <-df %>% group_by(id) %>% mutate(idcount = row_number())
df$trial <- groupedid$idcount

###Counting the number of trials for each participant
groupedtrials <- df %>% group_by(id) %>% summarize(total_trials = n_distinct(trial))
df$total_trials <- groupedtrials$total_trials[match(df$id, groupedtrials$id)]

###Finding the mode rating for each participant
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
groupmode <-df %>% group_by(id) %>% mutate(mode=Mode(rating))
df$mode <- groupmode$mode

###Finding how often participant used mode rating and dividing by their total trials
df$rating_mode_match <- ifelse(df$mode==df$rating,1,0)
group_mode_total<- df %>% group_by(id) %>%  summarise(Frequency = sum(rating_mode_match))
df$mode_total <- group_mode_total$Frequency[match(df$id, group_mode_total$id)]
df$mode_percentage <- df$mode_total/df$total_trials
df$junk <-ifelse(df$mode_percentage>=.9,1,0)

###Mean, SD, and z-score for each participant
meansd_df<- df %>% group_by(id) %>% summarise(across(.cols = rating,list(mean = mean, sd = sd)))
df$rating_average <- meansd_df$rating_mean[match(df$id, meansd_df$id)]
df$rating_sd <- meansd_df$rating_sd[match(df$id, meansd_df$id)]
df$rating_z <- (df$rating-df$rating_average)/df$rating_sd

###Variable transformation of target names using FaceCodes df
df$firstFace_rename <- FaceCodes$V2[match(df$firstFace, FaceCodes$V1)]
df$secondFace_rename <- FaceCodes$V2[match(df$secondFace, FaceCodes$V1)]
df$face1 <- FaceCodes$V4[match(df$firstFace, FaceCodes$V1)]
df$face2 <- FaceCodes$V4[match(df$secondFace, FaceCodes$V1)]
df$high <- pmax(df$face1,df$face2)
df$low <- pmin(df$face1,df$face2)

###'clean' is final df from cleaning phase
###If mode rating is used in more than 90% of trials for participant, then participant is dropped
clean <- df %>% filter(junk == "0")

###Similarity Matrix building begins
###Assigning column and row names and populating with mean scores of similarity ratings from c'lean' df
sm <- clean %>% group_by(high,low) %>% summarise(across(.cols = rating, list(mean = mean)))
c <- c(unique(FaceCodes$V2))
rname <- FaceCodes$V2
cname <- FaceCodes$V2
len <-length(rname)
simmat <- data.frame(matrix(ncol = len, nrow = len, dimnames = list(rname,cname)))
for (e in 1:(len-1)){
  for (f in 1:(len-e)){
    l <- e+ f
    k <- sm[which(sm$low == e & sm$high == l), ]
    simmat[e,l] <- c(k[,3])
  }
}
###Populating cells with coincide with matching pairs 
###'highsim' is highest similarity rating possible. 
highsim <- c(9)
for (e in 1:len){
  simmat[e,e] <- highsim
}

###Filling in the remainder of cells with blanks
for (e in 1:(len-1)){
  for (f in 1:(len-e)){
    l <- e+ f
    k <- sm[which(sm$low == e & sm$high == l), ]
    simmat[l,e] <- ""
  }
}