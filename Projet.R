library(rvest)
library(dplyr)
library(httr)

# Comparing the first 120 most expensive PC's in each website
TunisiaNetPCs<- data.frame()
link <- "https://www.tunisianet.com.tn/301-pc-portable-tunisie?page=2&order=product.price.desc"
page<-read_html(link)
Product_links <- page %>% 
  html_nodes("h2 > a") %>% html_attr("href")
page1<-read_html(Product_links[1])
Product_informationsNameREF <- page1 %>% html_nodes(".name") %>% html_text()
Product_informationsNameREF <- Product_informationsNameREF[1:8]

for(i in 1:22){
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
      Product_image<-page1 %>% html_nodes(".product-cover img") %>% html_attr("src")
      Product_manufacturer <- page1 %>% html_nodes(".product-manufacturer img") %>% html_attr("src")
      
      Product_informationsValue <- append(Product_informationsValue,Product_name)
      Product_informationsValue <- append(Product_informationsValue,Product_price)
      Product_informationsValue <- append(Product_informationsValue,Product_image)
      Product_informationsValue <- append(Product_informationsValue,Product_manufacturer)
      
      TunisiaNetPCs<-rbind(TunisiaNetPCs,Product_informationsValue)  
  
  }
  
  colnames(TunisiaNetPCs) <- Product_informationsNameREF
}
colnames(TunisiaNetPCs)[1] <- "SysExp"
colnames(TunisiaNetPCs)[2] <- "Processeur"
colnames(TunisiaNetPCs)[3] <- "RefProcesseur"
colnames(TunisiaNetPCs)[9] <- "NomDuProduit"
colnames(TunisiaNetPCs)[10] <- "Prix"
colnames(TunisiaNetPCs)[11] <- "ImageDuProduit"
colnames(TunisiaNetPCs)[12] <- "Fabricant"

new_col <- vector()
for(price in TunisiaNetPCs$Prix){
  if(grepl(" ",price)){
    price <- "NA"
  }
  new_col <- append(price, new_col)
}
new_col2 <- vector()
for(cell in TunisiaNetPCs$RefProcesseur){
  
  new_col2 <- append(gsub(",","",cell), new_col2)
}
new_col2
new_col3 <- vector()
for(Pc in TunisiaNetPCs$Fabricant){
  if(Pc == "https://www.tunisianet.com.tn/img/m/29.jpg"){
    new_col3 <- append("Dell",new_col3)
  }
  else if(Pc =="https://www.tunisianet.com.tn/img/m/6.jpg"){
    new_col3 <- append("HP", new_col3)
  }
  else if (Pc =="https://www.tunisianet.com.tn/img/m/3.jpg"){
    new_col3 <- append("Lenovo", new_col3)
  }
  else if (Pc =="https://www.tunisianet.com.tn/img/m/339.jpg"){
    new_col3 <- append("MSI", new_col3)
  }
  else if (Pc =="https://www.tunisianet.com.tn/img/m/79.jpg"){
    new_col3 <- append("Asus", new_col3)
  }
  else if (Pc =="https://www.tunisianet.com.tn/img/m/13.jpg"){
    new_col3 <- append("Acer", new_col3)
  }
  else if (Pc =="https://www.tunisianet.com.tn/img/m/165.jpg"){
    new_col3 <- append("Huawei", new_col3)
  }
}
new_col3
TunisiaNetPCs$NomDuFabricant <- rev(new_col3)
TunisiaNetPCs$RefProcesseur <- new_col2
TunisiaNetPCs$Prix <- rev(as.numeric(new_col))
TunisiaNetPCs <- subset( TunisiaNetPCs, grepl("Windows",SysExp) | grepl("FreeDos",SysExp))
TunisiaNetPCs <- subset( TunisiaNetPCs, grepl("www.tunisianet.com",ImageDuProduit))
TunisiaNetPCs
library(writexl)
write_xlsx(TunisiaNetPCs,"Tunisianet.xlsx")
write.csv(TunisiaNetPCs,"Tunisianet.csv", row.names = FALSE)

