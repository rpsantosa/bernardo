#### important: to draw a geom_line, was necessary to define the group!!###
# echo "# bernardo" >> README.md
# git init
# git add README.md
# git commit -m "first commit"
# git remote add origin https://github.com/rpsantosa/bernardo.git
# git push -u origin master


a<-readLines('gisa.txt')
# xi<-grep('[Xx]i[Xx]',a) # %>% grep('[^co]',.)
# co<-grep('[cC][oô]c',a)
# xico<-intersect(xi,co)
# xid<-setdiff(xi,xico)
# cod<-setdiff(co,xico)
da<-grep('[0-9]{1,2}/',a)
dates<-a[da];datesx<-dates
aux<-grep('[0-9]{1,2}/[6-7]$',dates);datesx[aux]<-paste0(dates[aux],'/17')
datesx<-as.Date(datesx,"%d/%m/%y")
# idx<-order(datesx)
# dat_ordered<-dates[idx]
library(tm)
library(stringr)
library(plyr)
library(reshape2)
library(ggplot2)
ccount<-function(i){
  i1<-grep(paste0('^',dates[i]),a)
  i2<-grep(paste0('^',dates[i+1]),a)
  print(c(i,i1,i2,i2-i1))
  nco<-sum(str_count(a[i1:i2], "[cC][oô]c"))
  nxi<-sum(str_count(a[i1:i2], '[Xx]i[Xx]'))
  return(c(nco,nxi,dates[i]))
}
rs<-lapply(1:(length(dates)-1),ccount)
rsd<-ldply(rs);colnames(rsd)<-c('nxi','nco','date');rsd$date<-datesx[-length(datesx)]
rsdm<-melt(rsd,id.vars = 'date');colnames(rsdm)<-c('date','id','number_ocurrences')
rsdm$date<-factor(rsdm$date)

p<-ggplot(data=rsdm, aes(x=date, y=number_ocurrences,colour=id,group=id)) + geom_point() + 
  geom_line() +   
  scale_fill_brewer(palette="Paired")+
  theme_bw() + 
  ggtitle('bernardo health')+  
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggsave('bernardo.pdf',device = 'pdf')

