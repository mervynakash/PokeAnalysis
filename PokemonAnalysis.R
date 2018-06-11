set.seed(123)
setwd("E:/Kaggle/Pokemon/")

library(dplyr)
library(plyr)
library(bindrcpp)
library(ggplot2)

df <- read.csv("Pokemon.csv",
               header = TRUE,
               na.strings = c(NA,""," ","NA"),
               stringsAsFactors = FALSE)

head(df)

df <- df %>% mutate(TotalAttack = Attack + Sp..Atk, TotalDefense = Defense + Sp..Def)
head(df)

colnames(df)
sapply(df,class)

colSums(is.na(df))

##############################################################################
fun <- function(a){
  df_t = df
  for (i in 1:(nrow(df)-1)){
    index = i
    j = i+1
    for(j in j:nrow(df)){
      if(df_t[j,a] > df_t[index,a]){
        index = j
        index2 = j
      }
    }
    temp = df_t[index,]
    df_t[index,] = df_t[i,]
    df_t[i,] = temp
  }
  return(head(df_t[,"Name"],10))
}

st <- c("HP","Attack","Defense","Sp..Atk","Sp..Def","Speed",
        "TotalAttack","TotalDefense","Total")

df_top <- matrix(nrow = 10, ncol = length(st), byrow = TRUE)*0
for(i in 1:length(st)){
  df_top[,i] <- fun(st[i])
}

df_top <- as.data.frame(df_top, stringsAsFactors = FALSE)
colnames(df_top) <- st

#############################################################################


# Types of Pokemon
PokeType <- union(unique(df[,"Type.1"]), na.omit(unique(df[,"Type.2"])))

pokeSelect <- function(x){
  df_p <- df %>% filter(Type.1 == x | Type.2 == x)
  return(df_p[,"Name"])
}

# Fire Pokemon
Fire_Poke <- pokeSelect("Fire")

# Water Pokemon
Water_Poke <- pokeSelect("Water")

# Grass Pokemon
Grass_Poke <- pokeSelect("Grass")

# Psychic Pokemon
Psychic_Poke <- pokeSelect("Psychic")

# Electric Pokemon
Electric_Poke <- pokeSelect("Electric")

# Bug Pokemon
Bug_Poke <- pokeSelect("Bug")

# Normal Pokemon
Normal_Poke <- pokeSelect("Normal")

# Poison Pokemon
Poison_Poke <- pokeSelect("Poison")

# Ground Pokemon
Ground_Poke <- pokeSelect("Ground")

# Fairy Pokemon
Fairy_Poke <- pokeSelect("Fairy")

# Fighting Pokemon  
Fighting_Poke <- pokeSelect("Fighting")

# Rock POkemon
Rock_Poke <- pokeSelect("Rock")

# Ghost Pokemon
Ghost_Poke <- pokeSelect("Ghost")

# Ice Pokemon
Ice_Poke <- pokeSelect("Ice")

# Dragon Pokemon
Dragon_Poke <- pokeSelect("Dragon")

# Flying Pokemon  
Flying_Poke <- pokeSelect("Flying")

# Steel Pokemon
Steel_Poke <- pokeSelect("Steel")



n_Grass <- length(Grass_Poke)
n_Fire <- length(Fire_Poke)
n_Water <- length(Water_Poke)
n_Bug <- length(Bug_Poke)
n_Normal <- length(Normal_Poke)
n_Poison <- length(Poison_Poke)
n_Electric <- length(Electric_Poke)
n_Ground <- length(Ground_Poke)
n_Fairy <- length(Fairy_Poke)
n_Fighting <- length(Fighting_Poke)
n_Psychic <- length(Psychic_Poke)
n_Rock <- length(Rock_Poke)
n_Ghost <- length(Ghost_Poke)
n_Ice <- length(Ice_Poke)
n_Dragon <- length(Dragon_Poke)
n_Flying <- length(Flying_Poke)
n_Steel <- length(Steel_Poke)



me_an <- function(x,y){
  df_m <- df %>% filter(Type.1 == x | Type.2 == x)
  m <- (mean(df_m[,y]))
  return(m)
}

PokeStatName <- c("Total","HP","Attack","Defense","Sp..Atk","Sp..Def","Speed")

mean_stat <- matrix(nrow = length(PokeType), ncol = (length(PokeStatName)+1),
                    byrow = TRUE)
mean_stat <- mean_stat*0
for(i in 1:length(PokeType)){
  mean_stat[i,1] <- PokeType[i]
  for(j in 2:(length(PokeStatName)+1)){
    mean_stat[i,j] <-  me_an(PokeType[i],PokeStatName[j-1])
  }
}

mean_stat <- as.data.frame(mean_stat, stringsAsFactors = FALSE)
mean_stat[,-1] <- sapply(mean_stat[,-1], as.numeric)


colnames(mean_stat) <- c("Types","Overall_Avg","HP_Avg",
                      "Attack_Avg","Defense_Avg","SpAttack_Avg",
                      "SpDefense_Avg","Speed_Avg")


num_of_poke <- c(n_Grass,n_Fire,n_Water,n_Bug,n_Normal,n_Poison,n_Electric,
                 n_Ground,n_Fairy,n_Fighting,n_Psychic,n_Rock,n_Ghost,n_Ice,n_Dragon,
                 n_Flying,n_Steel)

PokeAnalysis <- data.frame("Types_of_Pokemon"=PokeType)
PokeAnalysis <- cbind(PokeAnalysis,"Number_of_Pokemon" = num_of_poke)

PokeAnalysis <- merge(PokeAnalysis, mean_stat, by.x = "Types_of_Pokemon", by.y = "Types")
#View(PokeAnalysis)



#========================================= EDA =====================================#

# Overall Stats Analysis

PokeAnalysis %>% arrange(- Overall_Avg) %>%  head(10) %>% select(Types_of_Pokemon, Overall_Avg) %>% 
  mutate(Overall_Avg = round(Overall_Avg, 2)) %>% 
  ggplot(aes(x = reorder(Types_of_Pokemon,-Overall_Avg), y = Overall_Avg)) + 
  geom_bar(stat = "identity", aes(fill = Types_of_Pokemon)) + 
  geom_text(aes(label = Overall_Avg), vjust = -0.25) + 
  theme_void() + 
  theme(legend.position = "bottom", axis.title = element_blank(), axis.text.x = element_blank() ,
        axis.ticks = element_blank()) + 
  ggtitle("Types of Pokemon with best overall stats") + 
  scale_fill_manual(values = rainbow(10), name = "Pokemon Type")


# HP Stats Analysis

PokeAnalysis %>% arrange(- HP_Avg) %>%  head(10) %>% select(Types_of_Pokemon, HP_Avg) %>%
  mutate(HP_Avg = round(HP_Avg,2)) %>% 
  ggplot(aes(x = reorder(Types_of_Pokemon,-HP_Avg), y = HP_Avg)) + 
  geom_bar(stat = "identity", aes(fill = Types_of_Pokemon)) + 
  geom_text(aes(label = HP_Avg, vjust = -0.25)) + 
  theme_bw() + 
  theme(legend.position = "bottom", axis.title = element_blank(), axis.text.x = element_blank() ,
        axis.ticks = element_blank()) + 
  ggtitle("Types of Pokemon with best overall stats") + 
  scale_fill_manual(values = rainbow(10), name = "Pokemon Type")


# Attack Stats Analysis

PokeAnalysis %>% arrange(- Attack_Avg) %>%  head(10) %>% select(Types_of_Pokemon, Attack_Avg) %>%
  mutate(Attack_Avg = round(Attack_Avg,2)) %>% 
  ggplot(aes(x = reorder(Types_of_Pokemon,-Attack_Avg), y = Attack_Avg)) + 
  geom_bar(stat = "identity", aes(fill = Types_of_Pokemon)) + 
  geom_text(aes(label = Attack_Avg, vjust = -0.25)) + 
  theme_gray() + 
  theme(legend.position = "bottom", axis.title = element_blank(), axis.text.x = element_blank() ,
        axis.ticks = element_blank()) + 
  ggtitle("Types of Pokemon with best attack stats") + 
  scale_fill_manual(values = rainbow(10), name = "Pokemon Type")

# Defense Stats Analysis 

PokeAnalysis %>% arrange(- Defense_Avg) %>% head(10) %>%  select(Types_of_Pokemon, Defense_Avg) %>% 
  mutate(Defense_Avg = round(Defense_Avg,2)) %>% 
  ggplot(aes(x = reorder(Types_of_Pokemon,-Defense_Avg), y = Defense_Avg)) + 
  geom_bar(stat = "identity", aes(fill = Types_of_Pokemon)) + 
  geom_text(aes(label = Defense_Avg), vjust = -0.25) + 
  theme_gray() + 
  theme(legend.position = "bottom", axis.title = element_blank(), axis.text.x = element_blank() ,
        axis.ticks = element_blank()) + 
  ggtitle("Types of Pokemon with best defense stats") + 
  scale_fill_manual(values = rainbow(10), name = "Pokemon Type")

# Speed Stats Analysis

PokeAnalysis %>% arrange(- Speed_Avg) %>% head(10) %>%  select(Types_of_Pokemon, Speed_Avg) %>% 
  mutate(Speed_Avg = round(Speed_Avg,2)) %>% 
  ggplot(aes(x = reorder(Types_of_Pokemon,-Speed_Avg), y = Speed_Avg)) + 
  geom_bar(stat = "identity", aes(fill = Types_of_Pokemon)) + 
  geom_text(aes(label = Speed_Avg), vjust = -0.25) + 
  theme_gray() + 
  theme(legend.position = "bottom", axis.title = element_blank(), axis.text.x = element_blank() ,
        axis.ticks = element_blank()) + 
  ggtitle("Types of Pokemon with best speed stats") + 
  scale_fill_manual(values = rainbow(10), name = "Pokemon Type")

