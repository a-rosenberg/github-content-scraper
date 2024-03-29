## ----load_libraries------------------------------------------------------
# devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)

tt_data<-tt_load(2019,8)
tt_data


## ----visualize-----------------------------------------------------------
delta<-function(x,index){
  delta<-c(NA,(x[seq(2,length(x))]-x[seq(1,length(x)-1)]))
  delta[abs(delta)==Inf]<-0
  delta
}
total_delta<-function(x,index){
  x_sum<-vector("numeric",length(unique(index)))
  names(x_sum)<-as.character(unique(index))

  for(i in unique(index)){
    x_vals<-x[index==i]
    if(all(is.na(x_vals))){
      x_sum[as.character(i)]<-NA
    }else{
      x_sum[as.character(i)]<-sum(x_vals,na.rm = TRUE)
    }
  }
  MinYear<-min(names(x_sum)[!is.na(x_sum)])
  MaxYear<-max(names(x_sum)[!is.na(x_sum)])
  x_sum[MaxYear]-x_sum[MinYear]
}
cumsum_alt<-function(delta,dirval){
  #get first non-na location
  loc<-which(!is.na(dirval))[1]
  baseval<-dirval[loc]
  delta[loc]<-ifelse(is.na(delta[loc]),baseval,delta[loc]+baseval)
  cumsum(delta)
}
wrap_header <- function(text) {
  wtext <- paste(strwrap(text,width=30),collapse=" \n ")
  return(wtext)
}

tt_data$phd_by_field%>%
  #calculate values by field
  group_by(field)%>%
  mutate(delta_phds=delta(n_phds,year),
         delta_direction=ifelse(delta_phds>0,"blue","red"),
         totalSum=cumsum_alt(delta_phds,n_phds),
         total_delta_phds=total_delta(n_phds,year))%>%
  ungroup()%>%
  filter(total_delta_phds%in%sort(unique(total_delta_phds),decreasing = TRUE)[1:10])%>%
  mutate(field_alt=sapply(field,wrap_header))%>%
  arrange(desc(total_delta_phds))%>%
  ggplot()+
  geom_segment(aes(x=year,     xend = year,
                   y=totalSum, yend = totalSum-delta_phds,
                   color=I(delta_direction)),
               size=3) +
  facet_wrap(field_alt~.,
             scales = "free",
             strip.position = "top",
             ncol = 5) +
  scale_x_continuous(breaks = c(2010,2015),
                     minor_breaks = seq(2008,2017))+
  ggtitle(label = "Graduation Rates",
          subtitle = "Fields with Greatest Increase in Graduates") +
  ylab("Number of PhD Graduates") +
  xlab("Year")+
  theme_linedraw()

ggsave("2019-02-19/PhD_Grad_Rates.png")


