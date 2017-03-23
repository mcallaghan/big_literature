rm(list=ls())
library(RPostgreSQL)
library(tm)
library(wordcloud)
source("/home/galm/pg_keys.R")
setwd("/home/galm/projects/big_literature")
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)

drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "tmv_app",
                 host = "localhost", port = 5432,
                 user = pg_user, password = pg_pw)

toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})

BigramTokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
}

years <- c(1995,2000,2005,2010,2015)

yvocabs <- data.frame(
  year = years,
  docs = 0,
  words = 0,
  wpt = 0,
  avfreq = 0
)

procdocs <- function(d) {
  dcorp <- Corpus(VectorSource(d))
  dcorp <- tm_map(dcorp,removeNumbers)
  dcorp <- tm_map(dcorp,removePunctuation)
  dcorp <- tm_map(dcorp, toSpace, "-")
  dcorp <- tm_map(dcorp, removeWords,stopwords())
  dtm <- DocumentTermMatrix(
    dcorp,control=list(
      minWordLength = 3#,
    )
  )
  return(dtm)
}

all_by_freq <- data.frame(prop=numeric(0),n=numeric(0),year=numeric(0))

all_fm <- data.frame(prop=numeric(0),tid=numeric(0),year=numeric(0))

for (y in years) {
  q <- paste0('SELECT * from tmv_app_doc WHERE "PY" =',y)
  docs <- dbGetQuery(con, q)
  dsample <- sample_n(docs,1000)
  sample_dtm <- procdocs(dsample$content)
  dtm <- procdocs(docs$content)
  yvocabs[yvocabs$year==y,]$docs <- length(docs$UT)
  yvocabs[yvocabs$year==y,]$wpt <- sample_dtm$ncol
  yvocabs[yvocabs$year==y,]$words <- dtm$ncol
  
  FreqMat <- data.frame(term = dtm$dimnames$Terms, Freq = slam::col_sums(dtm))
  
  FreqMat$prop <- FreqMat$Freq / sum(FreqMat$Freq)
  
  byfreq <- FreqMat %>%
    filter(Freq>5) %>%
    group_by(prop) %>%
    summarise(n=n()) %>%
    mutate(year = y)
  
  fm <- arrange(FreqMat,-Freq) %>%
    mutate(tid = row_number(),year=y) %>%
    select(prop,tid,year)
  
  
  all_by_freq <- full_join(all_by_freq,byfreq)
  all_fm <- full_join(all_fm,fm)
  
  yvocabs[yvocabs$year==y,]$avfreq <- mean(FreqMat$Freq) / sum(FreqMat$Freq)
}

yvocabs_long <- yvocabs %>%
  rename(wpt = words_per_1000) %>%
  gather(var,value,docs:avfreq)

theme_set(theme_bw())

yvocabs_long$v <- ifelse(yvocabs$var %in% c("docs","words"),"volume/velocity","variety")

yvocabs_long$var <- factor(yvocabs_long$var,levels=c("docs","words","wpt","avfreq"))

ggplot(filter(all_fm,tid>10000)) +
#ggplot(filter(all_fm)) +
  geom_line(aes(tid,prop,colour=factor(year),group=year)) +
  scale_color_manual(values=scale[5:9])

ggplot(yvocabs_long) +
  geom_bar(
    aes(year,value),
    stat="identity"
  ) +
  facet_grid(var~.,scales="free")

scale <- brewer.pal(9,"BuGn")

p <- ggplot(filter(all_by_freq)) +
  geom_line(
    aes(prop,n,colour=factor(year),group=year)
  ) +
  scale_color_manual(values=scale[5:9]) +
  theme_classic() 
p
p +
  ylim(0,2000) +
  xlim(0,0.004)
p +
  ylim(0,1000) +
  xlim(0,0.0008)

  
###############
## do again for all years
years <- seq(1990,2015)

for (y in years) {
  q <- paste0('SELECT * from tmv_app_doc WHERE "PY" =',y)
  docs <- dbGetQuery(con, q)
  dcorp <- Corpus(VectorSource(docs$title))
  dcorp <- tm_map(dcorp,removeNumbers)
  dcorp <- tm_map(dcorp,removePunctuation)
  dcorp <- tm_map(dcorp, toSpace, "-")
  dcorp <- tm_map(dcorp, removeWords,stopwords())
  dtm <- DocumentTermMatrix(dcorp,control=list(
    minWordLength = 3#,
    #tokenize = BigramTokenizer
    )
  )
  yvocabs[yvocabs$year==y,]$docs <- dtm$nrow
  yvocabs[yvocabs$year==y,]$words <- dtm$ncol
  
  FreqMat <- data.frame(term = dtm$dimnames$Terms, Freq = slam::col_sums(dtm))
  yvocabs[yvocabs$year==y,]$avfreq <- mean(FreqMat$Freq) / sum(FreqMat$Freq)
  if (y > 1990) {
    FreqMat$new <- ifelse(FreqMat$term %in% l_f$term,F,T)
    new <- FreqMat[FreqMat$new==T,]
    png(paste0("plots/new_words/new_words_",y,".png"))
    wordcloud(words = new$term, freq = new$Freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    dev.off()
  }
  assign(paste0('f_',y),FreqMat)
  print(dtm$nrow)
  print(dtm$ncol)
  l_f <- FreqMat
  byfreq <- FreqMat %>%
    group_by(Freq) %>%
    summarise(n=n())
  
  ggplot(filter(byfreq)) +
    geom_line(
      aes(Freq,n)
    ) +
    theme_classic()
  
  ggsave(paste0("plots/words_f_",y,".png"))
  
}

######################################################
## Plot the vocab sizes and average freq
yvocabs_long <- yvocabs %>%
  gather(var,value,docs:avfreq)

theme_set(theme_bw())

ggplot(yvocabs_long) +
  geom_bar(
    aes(year,value),
    stat="identity"
  ) +
  facet_grid(var~.)

ggsave("plots/words_docs.png")

ggplot(yvocabs_long) +
  geom_bar(
    aes(year,log(value)),
    stat="identity"
  ) +
  facet_grid(var~.)

ggplot(filter(yvocabs_long,var=="avfreq")) +
  geom_line(
    aes(year,value)
  ) +
  labs(y="Average word proportion")

ggsave("plots/words_frequency.png")











all_docs <- dbGetQuery(con, 'SELECT * from tmv_app_doc')

all_docs$AP <- cut(all_docs$PY,
                          c(0,1985,1990.1,1995.1,2001.1,2007.1,2013.1,Inf),
                          c("Pre","AR1","AR2","AR3","AR4","AR5","AR6")
)

# Get the first number out of each doc abstract
all_docs$numbers <- regmatches(all_docs$content,regexpr('([1-9]+\\.*[1-9]*)',all_docs$content))

n_docs <- all_docs %>%
  filter(grepl('([1-9]+\\.*[1-9]*)',content)) %>%
  mutate(
    numbers = as.numeric(regmatches(content,regexpr('([1-9]+\\.*[1-9]*)',content)))
  )

ggplot(
  filter(
    n_docs,PY>1989,
    numbers < quantile(numbers,0.999)
    )
  ) +
  geom_jitter(
    aes(PY,numbers),
    alpha=0.05,
    size=0.05
  ) +
  theme_classic()

ggplot(
  filter(
    n_docs,PY>1989,
    numbers < quantile(numbers,0.999)
  )
) +
  geom_boxplot(
    aes(PY,numbers,group=PY)
  ) +
  theme_classic()

##############################################
## How many gigatons?

gt_docs <- all_docs %>%
  filter(grepl('([1-9]+\\.*[1-9]*) Gt',content)) %>%
  mutate(
    gt = regmatches(content,regexpr('([1-9]+\\.*[1-9]*) Gt',content)),
    gtn = as.numeric(regmatches(gt,regexpr('([1-9]+\\.*[1-9]*)',gt)))
  )

ggplot(
  filter(
    gt_docs,PY>1989#,
    #numbers < quantile(numbers,0.999)
  )
) +
  geom_jitter(
    aes(PY,gtn),
    alpha=0.5,
    size=0.5
  ) +
  theme_classic()

ggplot(
  filter(gt_docs,gtn < quantile(gtn,0.99)),
  aes(gtn)
  ) +
  geom_histogram(
    aes(y=..density..),
    binwidth=10, colour="black", fill="white",size=0.1
  ) +
  theme_bw() +
  geom_density(alpha=.2, fill="#FF6666") +
  facet_grid(
    AP~.
  )


dai <- dbGetQuery(con, 'SELECT * FROM tmv_app_doc  INNER JOIN tmv_app_docauthors 
                  ON (tmv_app_doc."UT" = tmv_app_docauthors.doc_id) WHERE tmv_app_doc."PY" = 1990')


dai <- dbGetQuery(con, 'SELECT * from scoping_docauthinst')
au_af <- dai %>%
  group_by(AU) %>%
  mutate(AF_variants = length(unique(AF)))


