rm(list=ls())
library(RMongo)
library(ggplot2)
library(dplyr)

mongo <- mongoDbConnect("mongoengine_documents", "localhost", 27017)

scop_results <- dbGetQueryForKeys(mongo,"scopus_doc", '{}','{"PY":1,"DO":1, "References":1}',0,1000)

scop_refs <- dbGetQueryForKeys(mongo,"scopus_doc", '{"References": {$exists: true }}','{"PY":1,"DO":1}',0,100000000)

refs <- dbGetQuery(mongo,"scopus_ref", '',0,100000000)


