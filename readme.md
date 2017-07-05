[上篇文章](http://zhangxiaohan.me/index.php/2017/07/03/scrapyspider_douban_movie_comment_spider/)从豆瓣爬取了《黄小厨和他的明星朋友们》又名《深夜食堂华语版》的影评，共 47650 条。这篇文章用这些数据来绘制一个词云。首先是我使用的工具和环境：
>**工具和环境**
>1. 系统： ubuntu 16.04
>2. 语言：R version 3.2.3
>3. IDE:  RStudio 1.0.136

本来这篇文章是想用 R Markdown 来写的，可是在 Ubuntu 系统里面 RStudio 有不支持中文输入的问题，而且一直没有找到有效的解决办法，所以只能放弃。而且从下面的代码里能看到，我的注释都是英文的…而且在上篇写爬虫的时候还遇到 sublime text 3  不能使用中文输入法的问题，同样也没有比较好的解决方法，真是气死人了。
### 文本挖掘的通用流程
做文本挖掘的一个比较通用的流程通常是这样的：
#### 获取文本
通常是通过爬虫来获取，或是找一些别人已经爬好的数据直接使用。
#### 文本的预处理
从网页爬取下来的数据通常会包含很多不必要的信息，比如会有很多空格、 "\n" 之类的，在这一步把真正有用的信息提取出来。还有如果爬取的数据有需要结构化的字段，在这一步也一并进行。比如日期字段、数值字段的处理。
#### 导入自定义词典
这一步是为下一步分词做基础。因为现在的中文分词算法通常只包含一些常用词汇词典，对于包含一些人名、网络用语、专业用语的文本，只使用算法自带的词典往往不能准确的分类。
#### 分词
顾名思义，分词就是将一段文本拆分成多个词，英文的分词相对来说比较简单，通常词和词之间是以空格分隔的。相对来说中文的分词就复杂得多，但现在针对这项技术的研究也已经比较深入了，因此我们有许多现成的算法可供选择使用。在 R 语言中，比较常用的是 `Rwordseg` 和 `JiebaR` 两个包。
#### 去除停用词
在分词的结果中包含大量的词汇，但是有些词是出现非常频繁但提供的信息量基本为0的，比如“的”、“了”等词。因此在这一步需要把这些词统统去除掉，以免影响后续的分析。目前比较常用的停用词表是哈工大停用词表、四川大学机器智能实验室停用词库和百度停用词列表，在此基础上，你还可以针对你分析的文本添加进你认为没有信息价值的词语。
#### 词性标注
对分词结果进行词性的标注（是名词、动词、形容词还是副词），但我认为这一步不是对所有类型的文本挖掘都必要。
#### 文本的数据化
这一步的目的是将上面获得的结果转化为结构性的数据，这样我们才能使用现有的数据挖掘、机器学习等方法来进行进一步的分析。比较简单的数据化方法是建立术语-文本矩阵，即术语和文本间基于频率的关系，以表格的形式表现，行表示术语，列表示文本，术语和文本间的频率以整数形式填在每个格里。其他还有将 TF-IDF（词频反文档频率）或 PMI （点互信息）作为权重的数据化方法。
#### 数据挖掘
经过上面的步骤我们将文本转化为了矩阵，接下来就可以利用各种数据挖掘、机器学习进行分析，比如进行情感分析、话题跟踪、文本相似度分析、聚类分析等等。

### 使用 R 语言绘制《深夜食堂》影评词云图
利用上面的步骤（其实没有用到最后一步），在 R 语言里面实现绘制《深夜食堂》词云图的代码如下：
```R
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
installDict("dictionary/中华美食.scel","food")
installDict("dictionary/本地明星.scel","names")
installDict("dictionary/深夜食堂【官方推荐】.scel","foodstore")


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
stopwords <- read.table("dictionary/stopwords.txt",
                         header = FALSE,
                         quote = "",
                         sep = "\n",
                         stringsAsFactors = FALSE)

stopwords <- stopwords[, 1]

comment.corpus <- tm_map(comment.corpus, removeWords, stopwords)


##convert to TermDOcumentMatrix##
tdm <- TermDocumentMatrix(comment.corpus, control = list(wordLengths = c(2, Inf)))

# save(tdm, file =  "term_document_matrix.RData")
# inspect(tdm[1:10, 1:2])

##convert to matrix, stored as sparse matrix##
mat <- sparseMatrix(i=tdm$i, j=tdm$j, x=tdm$v,
                    dims=c(tdm$nrow, tdm$ncol))

v <- rowSums(mat)
d <- data.frame(word = tdm$dimnames$Terms, freq = v, 
                stringsAsFactors = FALSE)


##plot word cloud and save##
graph <- wordcloud2(d, size = 1, 
                    color = "darkblue",
                    figPath = "深夜食堂.bmp", widgetsize = c(428, 921)) 

saveWidget(graph,"tmp.html",selfcontained = FALSE)
```

不得不说 `wordcloud2` 这个包真是一个[有情怀的包](https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html)，为什么这么说呢，来看我最后画出来的词云：
![](http://i650.photobucket.com/albums/uu224/dearelenore/wordcloud_zpstcpvdcad.png)
为了防止你们看不出来这是谁：
![](https://img3.doubanio.com/view/photo/photo/public/p2355982221.webp)

### 中间遇到的问题及解决方案
别看最后只是出来了一个词云的结果，中间遇到了好多好多问题，还好都解决掉了。在这里记录一下：
#### 繁简中文转换 
看来豆瓣上面不止是有大陆的网友，也有不少港澳台同胞。我爬下来的影评数据里面还有不少是繁体字的。为了实现繁简中文转换，我使用了 `ropencc` 包，github 项目地址[戳这里](https://github.com/qinwf/ropencc)，这个包目前还没有发布到 CRAN ，需要使用 `devtools` 通过 Github 安装。具体教程在[这篇文章](http://cn.qinwenfeng.com/ropencc/)有比较详细的介绍。

#### 获取外部词典
`Rwordseg` 包里面提供了一个 `installDict()` 方法，可以导入 txt 或者 scel 格式的词典，正好[搜狗细胞词库](http://pinyin.sogou.com/dict/)中提供 scel 格式的词库下载。为什么我的脚本可以准确地分出“泡面三姐妹”呢，因为搜狗上面竟然有一个官方推荐的深夜食堂词库！
可是遇到一些很高频出现的新演员人名，比如“吴昕”，很难找到相关的输入法词库，所以我又写了一个爬虫去[百度百科](http://baike.baidu.com/item/%E6%B7%B1%E5%A4%9C%E9%A3%9F%E5%A0%82/18664160#3)爬取整部电视剧的出演人员人名。。也是陷得有点深了。爬虫程序的代码如下：

```python
# -*- coding: utf-8 -*-
from scrapy import Request
from scrapy.spiders import Spider
from movie_actors.items import MovieActors
import re


class MFSCommentSpider(Spider):

    name = "midnight_food_store_actors" 

   
    headers = {
        "User-Agent":"Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/49.0.2623.221 Safari/537.36 SE 2.X MetaSr 1.0"
    }


    start_urls =  "http://baike.baidu.com/item/%E6%B7%B1%E5%A4%9C%E9%A3%9F%E5%A0%82/18664160#3_1"

    def start_requests(self):

        yield Request(self.start_urls, callback=self.parse, 
                      headers=self.headers)

 
    def parse(self, response):

        item = MovieActors()

        actors = response.xpath('//ul[@class="actorList"]/li[@class="listItem"]/dl/dt')
        

        for actor in  actors:
            print actor

            item['actor_name'] = actor.xpath('./a/text()').extract()

            if not item['actor_name']:
                actor_role = actor.xpath('./text()').extract()[0].encode('UTF-8')
                item['actor_name'] = actor_role.split('饰')[0]

            yield item

```

#### 术语-文本矩阵转为矩阵内存不足
分词之后建立了一个术语-文本矩阵（Term Document Matrix），这个矩阵的维数是 16350 × 47215，在画图前需要把它转换成一个普通的矩阵，可以说是理所当然地报了一个错：
```R
> as.matrix(tdm)
Error: cannot allocate vector of size 5.8 G
```
这个矩阵是一个非常非常稀疏的矩阵，大部分都是 0，转成普通矩阵要给它 5.8 G内存，真是想得美。于是我用了 `Matrix` 包把它用稀疏矩阵的形式存储，而且使用 `sparseMatrix()` 建立的这个对象可以进行正常矩阵的各种运算，可以说是非常方便了。

#### wordcloud2 词云无法按照输入图片展示
`wordcloud2` 可以使用用户自定义的图片形状来生成文字图云，它需要的图片形式以及生成的效果大概是这样的：
![](http://cos.name/wp-content/uploads/2016/08/20.png)
将图片的地址传给 `wordcloud2()` 中的 `figPath` 参数即可。可是在使用中却一直不能出现按照黄老师形象生成的词云图，后来查看了这个函数的源代码之后，发现是需要把 `widgetsize` 参数赋值为该图片的大小，这样问题就解决了。

#### `segmentCN()`的冲突
```R
> seg <- segmentCN(comment)
Error in segmentCN(comment) : Package "jiebaR" is required!
```
这个问题是我之前在工作的时候做文本挖掘遇到的，如果你同时用了`tmcn` 和 `Rwordseg`，没有用 `jiebaR` 的话，在调用 `segmentCN()` 函数的时候就会报这个错误。原因是 `jiebaR` 和 `Rwordseg`包中都有一个名为 `segmentCN()` 的函数，而`tmcn` 包提供了一个调用 `jiebaR` 中该函数的接口，因此在运行这个函数的时候就起了冲突。我的解决方法是在运行该函数前先把 `tmcn` 包 detach 掉。这次代码中我没有用 `tmcn` 这个包，也就少了这个麻烦。不过 R 中应该有其他方法可以解决不同包中重名函数的问题，只是我目前还没有发现。
`
