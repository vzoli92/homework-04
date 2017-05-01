
# A II/4. feladat függvénye:

# default értékeknek Hillary Clintont és az első tweetet adom meg

tweetek <- function(nev = "Hillary Clinton", szam = 1){
  # a beírt név helyességének ellenőrzése:
    if (!identical(nev, "Hillary Clinton") && !identical(nev, "Donald Trump")) {
      print("Helytelenul beirt nev.")
    }
  # a megadott tweetszám ellenőrzése:
    else if (identical(nev, "Hillary Clinton") && szam > hillary){
      print(paste0("A tweetek szama osszesen: ", hillary))
    }
    else if (identical(nev, "Donald Trump") && szam > trump){
      print(paste0("A tweetek szama osszesen: ", trump))
    }
  # a pontszámok alapján a csökkenő sorrend felállítása:
    else 
      for(i in 1:length(tweets$handle)){
        tweets$score[i] <- (tweets$retweet_count + tweets$favorite_count)
      }
     tweets <- tweets[order(tweets$score, decreasing = TRUE), ]
  # az adott nevekhez tartozó adott számú legkedveltebb tweetek kiíratása:
     if (identical(nev, "Hillary Clinton")){
     tweets_sub <- subset(tweets, tweets$handle == "Hillary Clinton")
     print(paste(nev, szam, "legkedveltebb tweetje: "))
     for (i in 1:szam){
       print(tweets_sub$text[i])
     }
     }
     else if (identical(nev, "Donald Trump")){
       tweets_sub <- subset(tweets, tweets$handle == "Donald Trump")
       print(paste(nev, szam, "legkedveltebb tweetje: "))
       for (i in 1:szam){
         print(tweets_sub$text[i])
       }
     }
}

################################################################################

