
# Házi feladat 4.
# Programozás I.
# 2016/17. II. félév

# Verba Zoltán
# 2017.04.25.

# II/1. feladat

# A clinton-trump-tweets adatbázis behívása:

tweets <- read.csv2(file = "data/clinton_trump_tweets.csv")
head(tweets)

# A handle változót átkonvertáljuk karakter változóvá:
tweets$handle <- as.character(tweets$handle)

# Kicserélem a neveket a megfelelő formára:
tweets$handle[tweets$handle == "HillaryClinton"] <- "Hillary Clinton"
tweets$handle[tweets$handle == "realDonaldTrump"] <- "Donald Trump"
################################################################################

# II/2. feladat:
# A tweetek gyakoriságának kiszámolása és ábrázolása:

# Először megszámolom a tweeteket az egyes elnökjelölteknél és ezt külön 
# változóba raktározom:
hillary <- nrow(tweets[tweets$handle == "Hillary Clinton",])
trump <- nrow(tweets[tweets$handle == "Donald Trump",])

# A tweetek gyakorisága:
hillary
trump

# segédváltozók az adatbázis elkészítéséhez, amely segítségével fogom elkészíteni
# a barplotot:
Candidate <- c("Hillary Clinton", "Donald Trump")
Tweet <- c(hillary, trump)

# az adatbázis:
df <- data.frame(Candidate, Tweet)
df$Candidate <- as.character(df$Candidate)

# Ábrázolás:
library(ggplot2)
positions <- c("Hillary Clinton", "Donald Trump")

ggplot(df, aes(x = Candidate, y = Tweet, fill = Candidate)) +
  scale_x_discrete(limits = positions)+
  scale_fill_manual(breaks = c("Hillary Clinton", "Donald Trump"), 
                    values=c("red", "blue"))+
  geom_bar(stat = "identity")+
  ggtitle("Candidate Tweets") +
  xlab("")+
  ylab("Tweet frequency")+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line=element_blank(),axis.text.x=element_blank())
ggsave("fig/tweet1.png", width = 7, height = 5, dpi = 100)

################################################################################

# II/3. feladat

# Milyen nyelvű tweetek szerepelnek az adatbázisban?
table(tweets$lang)

# szedjük ki az angol és spanyol nyelvbe soroltakon kívüli tweeteket:
for (i in 1:length(tweets$handle)){
  if (tweets$lang[i] == "da" || tweets$lang[i] == "et" || tweets$lang[i] == "fi" 
      || tweets$lang[i] == "fr" || tweets$lang[i] == "tl"){
    tweets$nyelv[i] <- 1 
  }
  else
    tweets$nyelv[i] <- 0
}

# segédváltozó az ellenrőzésre
df2 <- data.frame(tweets$handle, tweets$text, tweets$lang, tweets$nyelv)
df2 <- df2[tweets$nyelv == 1, ]
df2
# ebből az látszik, hogy valójában ezek angol nyelven vannak. Tehát át kell írni
# angolra a nyelvet
for (i in 1:length(tweets$handle)){
  if (tweets$lang[i] == "da" || tweets$lang[i] == "et" || tweets$lang[i] == "fi" 
      || tweets$lang[i] == "fr" || tweets$lang[i] == "tl"){
    tweets$lang[i] <- "en" 
  }
}

# most számoljuk újra az értékeket:
hillary_nyelv_en <- nrow(tweets[tweets$handle == "Hillary Clinton" & tweets$lang == "en",])
hillary_nyelv_es <- nrow(tweets[tweets$handle == "Hillary Clinton" & tweets$lang == "es",])
trump_nyelv_en <- nrow(tweets[tweets$handle == "Donald Trump" & tweets$lang == "en",])
trump_nyelv_es <- nrow(tweets[tweets$handle == "Donald Trump" & tweets$lang == "es",])

# nézzük, hogy kinek mennyi van az angol és spanyol tweetekből:


# Ábrázolás:
Language <- c("English", "Spanish", "English", "Spanish")
Tweet <- c(hillary_nyelv_en, hillary_nyelv_es, trump_nyelv_en, trump_nyelv_es)
Candidate <- c("Hillary Clinton", "Hillary Clinton", "Donald Trump", "Donald Trump")
# A df3 segéd data frame megalkotása
df3 <- data.frame(Language, Tweet, Candidate)


ggplot(df3, aes(x = Candidate, y = Tweet, fill = Language)) +
  scale_x_discrete(limits = positions)+
  scale_fill_manual(breaks = c("English", "Spanish"), 
                    values=c("darkgrey", "cornflowerblue"))+
  geom_bar(stat = "identity", position = position_dodge())+
  ggtitle("Language of Tweets") +
  xlab("")+
  ylab("Tweet frequency")+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line=element_blank(),axis.text.x=element_blank())
ggsave("fig/tweet2.png", width = 7, height = 5, dpi = 100)
################################################################################

# II/4. feladat

# Függvények elérési útjának megadása:
source("src/homework-04-functions.R")

# A függvény behívása:

# default értékkel:
tweetek()

# rossz név esetén:
tweetek("Zoli", 10)

# rossz szám esetén:
tweetek(szam = 9000)

# Hillary 10 legkedveltebb tweetje:
tweetek("Hillary Clinton", 10)

# Trump 15 legkedveltebb tweetje:
tweetek("Donald Trump", 15)
################################################################################

# III. Feladat
# 3.1. ábra

# dataset behívása
require(fivethirtyeight)
data("hiphop_cand_lyrics")
View(hiphop_cand_lyrics)
as.data.frame(hiphop_cand_lyrics)

# Package-ek behívása
library(ggplot2)
library(gcookbook)
install.packages("cowplot")
devtools::install_github("wilkelab/cowplot")
library("cowplot")
library(gridExtra)


# Név szétbontása
hiphop_cand_lyrics$name2 <- strsplit(as.character(hiphop_cand_lyrics$candidate), split = " ") 
View(hiphop_cand_lyrics)
hiphop_cand_lyrics$name2[[1]][[1]]

for (i in 1:length(hiphop_cand_lyrics$name2)){
  hiphop_cand_lyrics$name2_pri[i] <- hiphop_cand_lyrics$name2[[i]][[2]]
}
View(hiphop_cand_lyrics)


### Külön subseteket készítettem a feladatrészekhez
hiphop_cand_lyrics$num <- c(1)
# 3.1. ábrához
subset1 <- aggregate(hiphop_cand_lyrics$num, by = list(date = hiphop_cand_lyrics$album_release_date, name=hiphop_cand_lyrics$name2_pri), sum)
as.data.frame(subset1)
View(subset1)
# 3.2. ábrához
subset2<-aggregate(hiphop_cand_lyrics$num, by = list(date = hiphop_cand_lyrics$album_release_date, name=hiphop_cand_lyrics$name2_pri, sentiment=hiphop_cand_lyrics$sentiment), sum)
as.data.frame(subset2)
View(subset2)
# 3.3. ábrához
subset3<-aggregate(hiphop_cand_lyrics$num, by = list(date = hiphop_cand_lyrics$album_release_date, name=hiphop_cand_lyrics$name2_pri, theme=hiphop_cand_lyrics$theme), sum)
as.data.frame(subset3)
View(subset3)

### 3.1. ábra
# nem jó a label
subset1$name <- as.factor(subset1$name)
levels(subset1$name)
subset1$name = factor(subset1$name,levels(subset1$name)[c(8, 4, 1,3,6,7,2,5)])
subset1$name <- factor(subset1$name, levels = rev(levels(subset1$name)))
levels(subset1$name)
View(subset1)


plot_1 <- ggplot(subset1[order(subset1$name),], aes(x = subset1$date, y = subset1$x, fill = subset1$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" )) +
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )+
  scale_x_continuous( breaks=c(seq(1990,2016,by=5)),
                      minor_breaks=seq(1990,2016,by=5) 
  )+
  theme(plot.title = element_text(hjust = 0.5))


plot_1 + theme(legend.position="top") +
  ggtitle("Every mention of 2016 primary candidates in hip-hop songs")

ggsave("fig/hiphop1.png", width = 8, height = 5, dpi = 100)

### 3.2. ábra

# Szétbontás érzelmek szerint subsetekre
subset21 <-subset(subset2, subset2$sentiment=="positive")
subset22 <-subset(subset2, subset2$sentiment=="negative")
subset23 <-subset(subset2, subset2$sentiment=="neutral")

View(subset21)
View(subset22)
View(subset23)

### 3.2.1. ábra
subset21$name <- as.factor(subset21$name)
levels(subset21$name)
subset21$name = factor(subset21$name,levels(subset21$name)[c(4, 2, 3,1)])
subset21$name <- factor(subset21$name, levels = rev(levels(subset21$name)))
levels(subset21$name)
View(subset21)

plot_21 <- ggplot(subset21[order(subset21$name),], aes(x = subset21$date, y = subset21$x, fill = subset21$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" )) +
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )+
  theme(plot.title = element_text(hjust = 0.5))

plot_21
plot_21 + theme(legend.position="top") 

### 3.2.2. ábra
subset22$name <- as.factor(subset22$name)
levels(subset22$name)
subset22$name = factor(subset22$name,levels(subset22$name)[c(6, 2, 1,4,5,3)])
subset22$name <- factor(subset22$name, levels = rev(levels(subset22$name)))
levels(subset22$name)
View(subset22)

plot_22 <- ggplot(subset22[order(subset22$name),], aes(x = subset22$date, y = subset22$x, fill = subset22$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" )) +
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )+
  theme(plot.title = element_text(hjust = 0.5))

plot_22
plot_22 + theme(legend.position="top")


### 3.2.3. ábra
subset23$name <- as.factor(subset23$name)
levels(subset23$name)
subset23$name = factor(subset23$name,levels(subset23$name)[c(5,3,1,2,4)])
subset23$name <- factor(subset23$name, levels = rev(levels(subset23$name)))
levels(subset23$name)
View(subset23)

plot_23 <- ggplot(subset23[order(subset23$name),], aes(x = subset23$date, y = subset23$x, fill = subset23$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" ))+
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )+
  theme(plot.title = element_text(hjust = 0.5))

plot_23
plot_23 + theme(legend.position="top") 

# 3.2.1.-3.2.3. egyben
#install.packages("cowplot")
#devtools::install_github("wilkelab/cowplot")
#library("cowplot")
#library(gridExtra)

#list=c("Positive","Negative", "Neutral")

plot_egyben<-plot_grid(plot_21, plot_22, plot_23, ncol=3,
                       labels=c("Positive","Negative", "Neutral"), label_size=12, align="hv")+
  theme(plot.title = element_text(hjust = 0.5))

plot_egyben + theme(legend.position="top") +
  ggtitle("Candidate mentions, by sentiment")

ggsave("fig/hiphop2.png", width = 20, height = 5, dpi = 100)


### 3.3. ábra
# Szétbontás téma szerint subsetekre
subset31 <-subset(subset3, subset3$theme=="money")
subset32 <-subset(subset3, subset3$theme=="hotel")
subset33 <-subset(subset3, subset3$theme=="political")

View(subset31)
View(subset32)
View(subset33)

### 3.3.1 ábra
subset31$name <- as.factor(subset31$name)
levels(subset31$name)
subset31$name = factor(subset21$name,levels(subset21$name)[c(2, 1)])
subset31$name <- factor(subset21$name, levels = rev(levels(subset21$name)))
levels(subset31$name)
View(subset31)

plot_31 <- ggplot(subset31[order(subset31$name),], aes(x = subset31$date, y = subset31$x, fill = subset31$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" )) +
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )

plot_31 + theme(legend.position="top") 


### 3.2.2. ábra
subset32$name <- as.factor(subset32$name)
levels(subset32$name)
subset32$name = factor(subset32$name,levels(subset32$name)[c(1)])
subset32$name <- factor(subset32$name, levels = rev(levels(subset32$name)))
levels(subset32$name)
View(subset32)

plot_32 <- ggplot(subset22[order(subset32$name),], aes(x = subset32$date, y = subset32$x, fill = subset32$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" )) +
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )

plot_32
plot_32 + theme(legend.position="top") 

### 3.2.3. ábra
subset33$name <- as.factor(subset33$name)
levels(subset33$name)
subset33$name = factor(subset33$name,levels(subset33$name)[c(5,2,1,4,3)])
subset33$name <- factor(subset33$name, levels = rev(levels(subset33$name)))
levels(subset33$name)
View(subset33)

plot_33 <- ggplot(subset33[order(subset33$name),], aes(x = subset33$date, y = subset33$x, fill = subset33$name)) +
  geom_bar(stat = "identity", position = position_stack()) +
  scale_fill_manual("", values = c("Trump" = "burlywood1", "Clinton" = "cadetblue2", "Bush" = "coral2", "Huckabee"="antiquewhite1", "Cruz"="deeppink3", "Sanders"="darkseagreen4", "Christie"="darkgoldenrod1", "Carson"="darkolivegreen3" ))+
  theme(
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_line( size=.1, color="grey"),
    panel.grid.minor = element_line( size=.1, color="grey") 
  )

plot_33
plot_33 + theme(legend.position="top") 

# 3.3.1.-3.3.3. Egyben
plot_egyben2<-plot_grid(plot_31, plot_32, plot_33, ncol=3,
                        labels=c("Wealth","Trump Tower", "Politics"), label_size=12, align="hv")


plot_egyben2 + theme(legend.position="top") +
  ggtitle("Candidate mentions, by subject")

ggsave("fig/hiphop3.png", width = 20, height = 5, dpi = 100)

################################################################################

# IV/1. feladat

# Oszlopdiagramok a tweetek szentiment- és emócióelemzésére

# Először a szentimenteket hasonlítom össze:
# Oszlopdiagram:
od1 <- ggplot(tweets, aes(x = tweets$text_sentiment, fill = tweets$handle)) +
  geom_bar(stat = "count", position = position_dodge())+
  ggtitle("Candidate Tweets") +
  xlab("")+
  ylab("Tweet frequency")+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line("white"))

od1 + scale_fill_discrete(name  ="Candidate")

# az idősor változó elkészítése:
tweets$time <- as.character(tweets$time)
for (i in 1: length(tweets$time)){
tweets$time[i] <- as.character(tweets$time[i])
tweets$date[i] <- unlist(strsplit(tweets$time[i], split='T', fixed=TRUE))[1]
}

# Ránézésre azt látjuk, hogy nincsenek nagy eltérések a két jelölt különböző 
# érzelmet sugározó tweetjeik között
hillary_sub <- subset(tweets, tweets$handle == "Hillary Clinton")
hillary_sent <- c(nrow(hillary_sub[hillary_sub$text_sentiment == "negative",]),
                  nrow(hillary_sub[hillary_sub$text_sentiment == "neutral",]),
                  nrow(hillary_sub[hillary_sub$text_sentiment == "positive",]))
hillary_sent <- as.numeric(hillary_sent)

trump_sub <- subset(tweets, tweets$handle == "Donald Trump")
trump_sent <- c(nrow(trump_sub[trump_sub$text_sentiment == "negative",]),
                  nrow(trump_sub[trump_sub$text_sentiment == "neutral",]),
                  nrow(trump_sub[trump_sub$text_sentiment == "positive",]))
trump_sent <- as.numeric(trump_sent)


install.packages("zoo")
library(zoo)
zoo(hillary_sub, order.by=as.Date(as.character(hillary_sub$date), format = '%Y-%m-%d'))

# csinálok subseteket az idősoros ábrák elkészítéséhez:
id1_hillary_sub <- data.frame(table(hillary_sub$date, hillary_sub$text_sentiment))
id1_trump_sub <- data.frame(table(trump_sub$date, trump_sub$text_sentiment))

# az idősoros ábrák:
id1_hillary_plot <- ggplot(id1_hillary_sub,aes(x=id1_hillary_sub$Var1,y=id1_hillary_sub$Freq,
 colour=id1_hillary_sub$Var2,group=id1_hillary_sub$Var2)) + geom_line()
id1_trump_plot <- ggplot(id1_trump_sub,aes(x=id1_trump_sub$Var1,y=id1_trump_sub$Freq,
colour=id1_trump_sub$Var2,group=id1_trump_sub$Var2)) + geom_line()

id1_hillary_plot
id1_trump_plot
# az x tengely labelje nem vmi szép, de az ábrából az látszik, hogy a vizsgált 
# időszak alatt Hillarynél a pozitív tweetek dominálnak

# Statisztikai teszt a tweetek különbözőségére:
t.test(trump_sent, hillary_sent)
t.test(trump_sent, hillary_sent, paired = TRUE)
# Nem tudjuk szignifikánsan elvetni azt a nullhipotézist, hogy a tweetek megegyeznek.

################################################################################

# emotions:

hillary_emo <- c(nrow(hillary_sub[hillary_sub$text_emotion == "joy",]),
                  nrow(hillary_sub[hillary_sub$text_emotion == "disgust",]),
                  nrow(hillary_sub[hillary_sub$text_emotion == "sadness",]),
                  nrow(hillary_sub[hillary_sub$text_emotion == "fear",]),
                  nrow(hillary_sub[hillary_sub$text_emotion == "anger",]),
                  nrow(hillary_sub[hillary_sub$text_emotion == "surprise",]))
hillary_emo <- as.numeric(hillary_emo)

trump_emo <- c(nrow(trump_sub[trump_sub$text_emotion == "joy",]),
                 nrow(trump_sub[trump_sub$text_emotion == "disgust",]),
                 nrow(trump_sub[trump_sub$text_emotion == "sadness",]),
                 nrow(trump_sub[trump_sub$text_emotion == "fear",]),
                 nrow(trump_sub[trump_sub$text_emotion == "anger",]),
                 nrow(trump_sub[trump_sub$text_emotion == "surprise",]))
trump_emo <- as.numeric(trump_emo)

od2 <- ggplot(tweets, aes(x = tweets$text_emotion, fill = tweets$handle)) +
  geom_bar(stat = "count", position = position_dodge())+
  ggtitle("Candidate Tweets") +
  xlab("")+
  ylab("Tweet frequency")+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line("white"))

od2 + scale_fill_discrete(name  ="Candidate")


id2_hillary_sub <- data.frame(table(hillary_sub$date, hillary_sub$text_emotion))
id2_trump_sub <- data.frame(table(trump_sub$date, trump_sub$text_emotion))

# az idősoros ábrák:
id2_hillary_plot <- ggplot(id2_hillary_sub,aes(x=id2_hillary_sub$Var1,y=id2_hillary_sub$Freq,
                                               colour=id2_hillary_sub$Var2,group=id2_hillary_sub$Var2)) + geom_line()

id2_trump_plot <- ggplot(id2_trump_sub,aes(x=id2_trump_sub$Var1,y=id2_trump_sub$Freq,
                                           colour=id2_trump_sub$Var2,group=id2_trump_sub$Var2)) + geom_line()

id2_hillary_plot
id2_trump_plot

# Statisztikai teszt a tweetek különbözőségére:
t.test(trump_emo, hillary_emo)
t.test(trump_emo, hillary_emo, paired = TRUE)
# Nem tudjuk szignifikánsan elvetni azt a nullhipotézist, hogy a tweetek megegyeznek.

################################################################################

# IV/2. feladat

# kiválogatom a Trump által jegyzett android és iphone tweeteket
tweets_writers <- tweets[((tweets$source_url == "http://twitter.com/download/iphone")  |
                         (tweets$source_url == "http://twitter.com/download/android")) &
                         (tweets$handle == "Donald Trump"), ]

# emotions:
writers_emo <- ggplot(tweets_writers, aes(x = tweets_writers$text_emotion,
  fill = tweets_writers$source_url)) + 
  geom_bar(stat = "count", position = position_dodge()) + 
  scale_fill_manual( values = c("blue","red"))

writers_emo

# sentiments:
writers_senti <- ggplot(tweets_writers, aes(x    = tweets_writers$text_sentiment, 
fill = tweets_writers$source_url)) +
geom_bar(stat = "count", position = position_dodge()) +
scale_fill_manual( values = c("blue","red"))

writers_senti

# a statiszikalag szignifikáns különbség ellenőrzése:
emo_iphone  <- table( tweets_writers[ tweets_writers$source_url == 
                "http://twitter.com/download/iphone", c("text_emotion") ] )

emo_android <- table( tweets_writers[ tweets_writers$source_url == 
               "http://twitter.com/download/android", c("text_emotion") ] )

senti_iphone  <- table( tweets_writers[ tweets_writers$source_url == 
              "http://twitter.com/download/iphone", c("text_sentiment") ] )

senti_android <- table( tweets_writers[ tweets_writers$source_url == 
              "http://twitter.com/download/android", c("text_sentiment") ] )

# t-teszt (emotions és sentiments)

t.test(emo_iphone  , emo_android  , paired = TRUE)
t.test(senti_iphone, senti_android, paired = TRUE)

# Nem tudjuk elvetni a nullhipotézist -> nincs szignifikáns különbség egyik 
# esetben sem az andorid és iphone tweetek között.

################################################################################

# IV/3. feladat

# telepítsük a packageket:
install.packages("rmarkdown")
library("rmarkdown")







