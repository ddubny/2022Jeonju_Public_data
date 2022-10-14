library(rvest)    # to use read_html(), html_nodes, html_text()     #package for Web Pages scraping
library(dplyr)
library(data.table)
library(tidyverse)



code_url <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EC%A0%84%EB%B6%81%EB%8C%80%EB%B3%91%EC%9B%90&sort=0&photo=0&field=0&pd=0&ds=&de=&cluster_rank=47&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:all,a:all&start="
code_url <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EB%B3%91%EC%9B%90%20%EC%A0%84%EC%9B%90%20%2B%20%EA%B3%A8%EB%93%A0%ED%83%80%EC%9E%84&sort=0&photo=0&field=0&pd=0&ds=&de=&cluster_rank=17&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:all,a:all&start="


code_url <- "https://search.naver.com/search.naver?where=news&sm=tab_pge&query=%EC%A0%84%EB%B6%81%EB%8C%80%EB%B3%91%EC%9B%90%20%2B%20%EC%9D%98%EB%A3%8C&sort=0&photo=0&field=0&pd=0&ds=&de=&cluster_rank=24&mynews=0&office_type=0&office_section_code=0&news_office_checked=&nso=so:r,p:all,a:all&start="


urls <- NULL
for(page in 0:1){
  urls <- c(urls, paste(code_url,page*10+1,sep=""))   
}


contents <- NULL
for (link in urls){
  posthtml <- read_html(link,encoding="utf-8")
  contents <- c(contents, posthtml %>% html_nodes(".api_subject_bx") %>%
                  html_text())
}

contents

all <- NULL
for (i in 0:1){
  each <- contents[i]
  kk <- gsub("[\n\t]", "", each)
  kk <- gsub("문서 저장하기  Keep에 저장 Keep 바로가기","",kk)
  kk <- gsub("언론사 선정언론사가 선정한 주요기사 혹은 심층기획 기사입니다. ","",kk)
  all <- c (all, kk )
}


contents <- paste(all,collapse = "")

library(KoNLP)      
library(wordcloud)  

# KoNLP>세종사전을 사용함수
useSejongDic()


nouns = extractNoun(contents) #명사만 추출

nouns <- nouns[nchar(nouns)>=2]   #명사 중에서 2글자 이상만 남기기
nouns <- gsub("네이버뉴스","", nouns) 


# 데이터 분석
wordFreq <-table(nouns)    #빈도 계산
sort.wordFreq = sort(wordFreq, decreasing=TRUE)
sort.wordFreq = sort.wordFreq[1:10]

# 막대그래프
par(mfrow=c(1,1), mar=c(3,3,3,3))
barplot(sort.wordFreq, horiz = TRUE, names.arg=, cex.names=0.6, las=1)

# 워드 클라우드
pal <- brewer.pal(6, "Dark2")
windowsFonts(malgun=windowsFont("맑은 고딕"))

par(mfrow=c(1,1), mar=c(1,1,1,1))  # plot 환경설정 
wordcloud(words=names(wordFreq), freq=wordFreq, colors=pal, min.freq=1, random.order=F, family="malgun")


