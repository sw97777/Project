#Bring In Libraries
library(rvest)
library(dplyr)
library(plyr)
library(data.table)
install.packages("XML")
library(XML)
install.packages("Rcmdr", dependencies = TRUE)
library(xml_child)
install.packages("xml_child")

#### First Grab Pages From 0-9 to Z and Firms on Each Page ####
pages<-c("0-9", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N",
         "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")

#Creating empty data set
all_firms<-data.frame(Firm_Name=character())

for(i in 1:length(pages)){
  #Website to scrap
  theurl<-paste0("https://markets.businessinsider.com/index/components/s&p_500/",pages[i]) 
  
  #Download/Read the html
  html<- read_html(theurl)
  
  #Use SelectorGadget to figure out what table to read
  get_firm<-html_nodes(html,".site-content__col--left") 
  
  #Make previous object into a table
  table_f<-html_table(get_firm) 
  
  #Keep only firms in this table
  firms<-data.frame(table_f[[1]][["Name"]])
  firms_dt<-data.table(table_f[[1]][["Name"]])
  firms<-firms %>% dplyr::rename(Firm_Name=1)
  
  #Build Container for links
  all_links<-data.frame(html=character()) 
  for(j in 1:nrow(firms)){
    #This is the link of each firm to grab their stats
    get_links<-html_nodes(html, paste0("tr:nth-child(",j,") a")) %>% html_attr("href")
    link<-data.frame(get_links)
    link<-link %>% dplyr::rename(html=1)
    all_links<-rbind(all_links, link)
  }
  
  #Bind columns of firm data and links
  firms<-cbind(firms, all_links)
  
  all_firms<-rbind(all_firms, firms)  #Bind all firms into one table
  print(paste0("Finished page: ", pages[i])) #See the process
}


#### Next, I am going to grab specified data ####
firms<-all_firms$Firm_Name

path<-all_firms$html

#Create Blank Tables
stats<-data.frame(Firm_be_analyzed=character(),Date=character(), Analyst_Company=character(),Opinions=character(), Price=character(),Attitude=character())


#Create URL of Analyst Opinions
theurl<-paste("https://markets.businessinsider.com",path, sep="")

######Create Loop#####
for(i in 1:nrow(all_firms)){
  tryCatch({
    html<- read_html(theurl[i])
    opinions<-html_node(html,".grid--ld-vertical-scrolling, .grid--m-vertical-scrolling, .grid--s-vertical-scrolling, .grid--vertical-scrolling")
    opinions_table<-html_table(opinions)
    new_opinions_table<-opinions_table[,c(1,3,5,6,7)]
    
    #Rename Column
    names(new_opinions_table)[1]<-"Date"
    names(new_opinions_table)[2]<-"Analyst_Company"
    names(new_opinions_table)[3]<-"Opinions"
    names(new_opinions_table)[4]<-"Price"
    names(new_opinions_table)[5]<-"Attitude"
    new_opinions_table$Firms<-firms[i]
    new_opinions_table<-new_opinions_table[,c(6,1,2,3,4,5)]
    stats<-rbind(stats, new_opinions_table)
    
    
    print(paste0("Finished with: ", firms[i]))
  }, error=function(e){cat(conditionMessage(e))})
  
}
write.table(stats,file="Exported_Midterm_Project.csv",sep=",")

###Use the stats to see performance of the firms through opinions from analyst companies###
stats_3m<-stats[stats$Firms=="3M Co.",]

nrow(stats_3m)
stats_3m_buy<-stats_3m[stats_3m$Opinions=="Maintained Buy",]
nrow(stats_3m_buy)
