rm(list=ls())
library(RMongo)
library(ggplot2)
library(dplyr)

mongo <- mongoDbConnect("mongoengine_documents", "localhost", 27017)
output <- dbGetQuery(mongo, "similarity", '{"jaccard": {"$gt": 0.1 }}',0,limit=10000)
output_do <- dbGetQuery(mongo, "similarity", '{"jaccard": {"$gt": 0.1 }, "scopus_do": true , "wos_do": true}',0,limit=10000)
matches <- dbGetQuery(mongo, "match", '{}',0,limit=350000)

########
## What's the jaccard similarity of matches?
ggplot(matches) +
  geom_histogram(
    aes(jaccard)
  )

#######
## What's the PY_diff of matches? 
ggplot(filter(matches,py_diff!=0)) +
  geom_bar(
    aes(py_diff)
  )

matches$py_diff_b <- ifelse(matches$py_diff!=0,T,F)

ggplot(filter(matches)) +
  geom_bar(
    aes(py_diff_b)
  )

print(sum(matches$py_diff_b))


########
## How many documents compared? (Remember docs are compared in parallel 8s) 
length(unique(output$scopus_id))

########
## How many cases do we have in each combination of doi availability
ggplot(output) +
  geom_bar(
    aes(interaction(scopus_do,wos_do))
  )

######
## Where are all the missing DOs coming from?
scop_results <- dbGetQueryForKeys(mongo,"scopus_doc", '{}','{"PY":1,"DO":1}',0,1000000)
scop_results$DOT <- ifelse(scop_results$DO=="",F,T)

ggplot(filter(scop_results,PY>1980)) +
  geom_bar(
    aes(PY,fill=DOT)
  )

########
## Does similarity differ by availability of dois?
ggplot(output) +
  geom_boxplot(
    aes(interaction(scopus_do,wos_do),jaccard)
  )



output_do$py_diff_b <- ifelse(output_do$py_diff!=0,T,F)


ggplot(output_do) +
  geom_boxplot(
    aes(do_match,jaccard)
  ) +
  geom_jitter(
    aes(do_match,jaccard,colour=py_diff_b)
  )


scop_results_PY <- scop_results %>%
  group_by(PY) %>%
  summarise(n=n())



