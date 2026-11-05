lo <- read.csv("data/tmp/sentence_annotations_20250316_212317.csv")
cam <- readRDS("data/clean/annotator_1.rds")
etienne <- readRDS("data/clean/annotator_2.rds")

mean(lo$manual_score)
mean(cam$manual_sentiment_cam)
mean(etienne$manual_sentiment_etienne)

hist(lo$manual_score)
hist(cam$manual_sentiment_cam)
hist(etienne$manual_sentiment_etienne)


