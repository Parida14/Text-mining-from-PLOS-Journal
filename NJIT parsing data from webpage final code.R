install.packages("RCurl")
install.packages("XML")
library(RCurl)
library(XML)

start <- "2014-12-15"
end <- "2015-01-01"
input = paste0("http://journals.plos.org/ploscompbiol/search?filterStartDate=",start, "&filterEndDate=",end , "&q=&sortOrder=RELEVANCE&filterJournals=PLoSCompBiol")

html <- getURL(input, followlocation=TRUE)
doc = htmlParse(html, asText=TRUE)
links = c()
suppressWarnings(if(max(as.numeric(xpathSApply(doc, "//*[@id='article-pagination']/a/@data-page"))) == -Inf)
{page = max(as.numeric(xpathSApply(doc, "//*[@id='article-pagination']/a/@data-page")))}else {page = 1})

for(i in 1 : page){
  input = paste0("http://journals.plos.org/ploscompbiol/search?filterStartDate=",start, "&filterEndDate=",end , "&q=&sortOrder=RELEVANCE&filterJournals=PLoSCompBiol","&page=",i)
  
  html <- getURL(input, followlocation=TRUE)
  doc = htmlParse(html, asText=TRUE)
  links = c(links, xpathSApply(doc, "//dt/a/@href"))
}

url= "http://journals.plos.org"
k=length(links)
for(i in 1:k){
  input[i]=paste0(url,links[i])
}

lis=list()

for (i in 1:k){
  html[i]=getURL(input[i], followlocation=TRUE)
  doc= htmlParse(html[i], asText=TRUE)
  
  Title = c(xpathSApply(doc, "//*[@id='artTitle']", xmlValue))
  
  Authors = xpathSApply(doc, "//*[@id='floatAuthorList']", xmlValue)
  Authors = gsub("\n", " ", Authors)
  Authors = c(strsplit(Authors,'\\s*,\\s*'))
  
  
  PubDate = xpathSApply(doc, "//*[@id='artPubDate']", xmlValue)
  PubDate = c(gsub('Published:\\s*', '', PubDate))
  
  
  AuthorAffiliations = xpathSApply(doc,"//*[starts-with(@id,'authAffiliations-')]", xmlValue)
  AuthorAffiliations = c(gsub("\n", " ", AuthorAffiliations))
  
  
  CorrespondingAuthor_mail = c(xpathSApply(doc,"//*[starts-with(@id,'authCorresponding')]", xmlValue))
  
  
  Abstract = c(xpathSApply(doc, "//*[@class='abstract toc-section']/*[@title='Abstract']/../p", xmlValue))
  
  
  Fulltext = xpathSApply(doc,"//*[@id='artText']", xmlValue)
  Fulltext = c(gsub("\n", " ", Fulltext))
  
  
  CorrespondingAuthor = xpathSApply(doc,"//*[@class='author-name']/span[@class='email']/..", xmlValue)
  CorrespondingAuthor = gsub("\n", " ", CorrespondingAuthor)
  CorrespondingAuthor = c(strsplit(CorrespondingAuthor,'\\s*,\\s*'))
  
  lis[[i]]<-list(Title,Authors,PubDate,AuthorAffiliations,CorrespondingAuthor_mail,Abstract,Fulltext,CorrespondingAuthor)
}

#drf<-as.data.frame(lis)
dput(lis, file="project_output.R")
