library(rvest)
library(dplyr)
library(httr)
library(stringr)
# Comparing the first 120 most expensive PC's in each website
df<- data.frame()
link <- "https://www.tunisianet.com.tn/301-pc-portable-tunisie?page=2&order=product.price.desc"
page<-read_html(link)
Product_links <- page %>% 
  html_nodes("h2 > a") %>% html_attr("href")
page1<-read_html(Product_links[1])
Product_informationsNameREF <- page1 %>% html_nodes(".name") %>% html_text()
Product_informationsNameREF <- Product_informationsNameREF[1:8]

for(i in 1:5){
  regular_link <- paste("https://www.tunisianet.com.tn/301-pc-portable-tunisie?page=",as.character(i),sep="")
  sorted_link<-paste(regular_link,"&order=product.price.desc",sep="")
  page <-read_html(sorted_link)
  Product_links <- page %>% 
    html_nodes("h2 > a") %>% html_attr("href")
  Product_links
  
  for(link in Product_links){
    page1<-read_html(link)
    Product_informationsName <- page1 %>% html_nodes(".name") %>% html_text()
      Product_informationsValue <- page1 %>% html_nodes(".value") %>% html_text()
      Product_informationsValue <- Product_informationsValue[1:8]
      Product_name<- page1 %>% html_nodes("#main h1") %>% html_text()
      Product_name <-sapply(strsplit(Product_name,split='/'),'[',1)
      Product_price <- page1 %>% html_nodes(".current-price span") %>% html_attr("content")
      Product_price <- as.numeric(Product_price)
      
      Product_informationsValue <- append(Product_informationsValue,Product_name)
      Product_informationsValue <- append(Product_informationsValue,Product_price)
      if(grepl("Non",Product_informationsValue[1])==FALSE | is.numeric(Product_price)==TRUE){
        df<-rbind(df,Product_informationsValue)  
      }
  
  }
  
  colnames(df) <- Product_informationsNameREF
}
colnames(df)[1] <- "SysExp"
