a<-readLines('gisa.txt')
# xi<-grep('[Xx]i[Xx]',a) # %>% grep('[^co]',.)
# co<-grep('[cC][oô]c',a)
# xico<-intersect(xi,co)
# xid<-setdiff(xi,xico)
# cod<-setdiff(co,xico)
da<-grep('[0-9]{1,2}/',a)
dates<-a[da];
#aux<-grep('[0-9]{1,2}/[6-7]$',dates);dates[aux]<-paste0(dates[aux],'/17')
# datesx<-as.Date(dates,"%d/%m/%y")
# idx<-order(datesx)
# dat_ordered<-dates[idx]
library(tm)
library(stringr)
library(plyr)
library(reshape2)
ccount<-function(i){
  i1<-grep(paste0('^',dates[i]),a)
  i2<-grep(paste0('^',dates[i+1]),a)
  #print(c(i,i1,i2,i2-i1))
  nco<-sum(str_count(a[i1:i2], "[cC][oô]c"))
  nxi<-sum(str_count(a[i1:i2], '[Xx]i[Xx]'))
  return(c(nco,nxi,as.character(dates[i])))
}
rs<-lapply(1:(length(dates)-1),ccount)
rsd<-ldply(rs);colnames(rsd)<-c('nxi','nco','date')
rsdm<-melt(rsd,id.vars = 'date');colnames(rsdm)<-c('date','id','number_ocurrences')

# Grouped Bar Plot
counts <- table(mtcars$vs, mtcars$gear)
df2$dose <- as.numeric(as.vector(df2$dose))
ggplot(data=rsdm, aes(x=date, y=number_ocurrences,fill=id)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

p<-ggplot(data=rsdm, aes(x=date, y=number_ocurrences)) + geom_boxplot()
p + facet_grid(id ~ .)