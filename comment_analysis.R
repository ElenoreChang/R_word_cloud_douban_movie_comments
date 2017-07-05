library(stringr)
library(ggplot2)
library(ropencc)
library(Rwordseg)
library(tm)
library(wordcloud2)
library(Matrix) 
library(htmlwidgets)


setwd("~/midnight_food_store_comment_analysis")
##read data##
# comments <- read.csv(file = "comments.csv",
#                      header = TRUE,
#                      stringsAsFactors = FALSE)


##arrange data##
# comments <- comments[, -7]
#
# comments$comment_time <- str_trim(comments$comment_time)
# comments$comment_time <- as.Date(comments$comment_time)
#
# comments$comment <- str_trim(comments$comment)
# t2sConverter <- converter(T2S)
# comments$comment <- t2sConverter[comments$comment]
#
# comments$usefule_num <- as.integer(comments$usefule_num)
#
# comments$ranking <- factor(comments$ranking,
#                            levels = c("力荐", "推荐", "还行", "较差", "很差", ""),
#                            labels = as.character(5: 0))

# save(comments, file =  "comments.RData")
load("comments.RData")

##plot##
p_ranking <- ggplot(comments) +
  geom_bar(aes(x = ranking, fill = ranking))
p_ranking

p_date <- ggplot(comments) +
  geom_bar(aes(x = comment_time, fill = ranking))
p_date


##clean text##
comment <- gsub("[a-zA-Z]", "", comments$comment)
comment <- gsub("//.", "", comment)
comment <- comment[!is.na(comment)]
comment <- comment[!nchar(comment) < 2]

##import dictionary##
installDict("dictionary/中华美食.scel", "food")
installDict("dictionary/本地明星.scel", "names")
installDict("dictionary/深夜食堂【官方推荐】.scel", "foodstore")


my_dic <- read.csv(file = "dictionary/mydict.csv",
                   header = TRUE,
                   stringsAsFactors = FALSE)

my_dic <- str_trim(my_dic[, 1])
insertWords(my_dic)

##word seg##
seg <- segmentCN(comment)

##convert to corpus
comment.seg <- as.vector(seg)
comment.corpus <- Corpus(VectorSource(comment.seg))

##remove stopwords
stopwords <- read.table(
  "dictionary/stopwords.txt",
  header = FALSE,
  quote = "",
  sep = "\n",
  stringsAsFactors = FALSE
)

stopwords <- stopwords[, 1]

comment.corpus <- tm_map(comment.corpus, removeWords, stopwords)


##convert to TermDOcumentMatrix##
tdm <-
  TermDocumentMatrix(comment.corpus, control = list(wordLengths = c(2, Inf)))

# save(tdm, file =  "term_document_matrix.RData")
# inspect(tdm[1:10, 1:2])

##convert to matrix, stored as sparse matrix##
mat <- sparseMatrix(
  i = tdm$i,
  j = tdm$j,
  x = tdm$v,
  dims = c(tdm$nrow, tdm$ncol)
)

v <- rowSums(mat)
d <- data.frame(
  word = tdm$dimnames$Terms,
  freq = v,
  stringsAsFactors = FALSE
)


##plot word cloud and save##
graph <- wordcloud2(
  d,
  size = 1,
  color = "darkblue",
  figPath = "Mr_Huang.bmp",
  widgetsize = c(428, 921)
)

saveWidget(graph, "tmp.html", selfcontained = FALSE)
