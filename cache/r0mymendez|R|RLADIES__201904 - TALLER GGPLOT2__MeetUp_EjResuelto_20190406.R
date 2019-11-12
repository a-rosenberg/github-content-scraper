## ----include=FALSE-------------------------------------------------------
library(ggplot2)
library(reshape2)
library(emojifont)
library(plotly)



## ------------------------------------------------------------------------

df.tips=tips
ggplot(      df.tips  ,       aes(x=total_bill  ,  y=tip)    )



## ------------------------------------------------------------------------

ggplot(      df.tips  ,       aes(x=total_bill  ,  y=tip)    ) +
  geom_point()



## ------------------------------------------------------------------------
ggplot(      df.tips  ,       aes(x=total_bill  ,  y=tip)    ) +
  geom_point() +
  labs ( 
        title='Mi primer gráfico', 
       subtitle='El gráfico fue realizado con ggplot2 y reshape2',
       x='Total de Factura', 
       y='Propinas',
       caption='Este gráfico fue realizado en el meetup de RLadies')




## ------------------------------------------------------------------------
ggplot( df.tips  , aes(x=total_bill , y=tip, color=sex ) ) +
  geom_point( size=3 ) +
  labs ( 
         title='Mi primer gráfico', 
         subtitle='El gráfico fue realizado con ggplot2 y reshape2',
         x='Total de Factura', 
         y='Propinas',
         caption='Este gráfico fue realizado en el meetup de RLadies')


## ------------------------------------------------------------------------
ggplot(tips,aes(x=total_bill,y=tip, color=sex) ) +
  geom_point ( size=3 ) + 
  geom_line ( color='blue' ) +
  labs(title='Mi primer gráfico', 
       subtitle='El gráfico fue realizado con ggplot2 y reshape2',
       x='Total de Factura', 
       y='Propinas',
       caption='Este gráfico fue realizado en el meetup de RLadies')



## ------------------------------------------------------------------------
ggplot(tips,aes(x=total_bill,y=tip, color=sex) ) +
  geom_point ( size=3 ) +  geom_line ( color='blue') +
  labs(title='Mi primer gráfico', 
       subtitle='El gráfico fue realizado con ggplot2 y reshape2',
       x='Total de Factura', 
       y='Propinas',
 caption='Este gráfico fue realizado en el meetup de RLadies') +
theme_minimal() 



## ------------------------------------------------------------------------
ggplot(tips,aes(x=total_bill,y=tip, color=sex) ) +
  geom_point ( size=3 ) +  geom_line ( color='blue') +
  labs(title='Mi primer gráfico', 
       subtitle='El gráfico fue realizado con ggplot2 y reshape2',
       x='Total de Factura', 
       y='Propinas',
 caption='Este gráfico fue realizado en el meetup de RLadies') +
theme_minimal ()  +
facet_wrap (.~day)


## ------------------------------------------------------------------------

search_emoji('smile')


## ------------------------------------------------------------------------

emoji('smile')


## ------------------------------------------------------------------------

  df.tips=tips
  df.tips$emoji=emoji("heart")


## ------------------------------------------------------------------------

  ggplot(tips,aes(x=total_bill,y=tip,color=sex,label=emoji))+
    geom_point()+ 
    geom_text(family="EmojiOne", size=12,show.legend = F)+
    labs(title='Mi primer gráfico con emojifont', 
         x='Total de Factura', 
         y='Propinas') +
    theme_bw()


## ------------------------------------------------------------------------
  df.tips=tips
  df.tips$emoji=ifelse(df.tips$sex=='Female', emoji('girl'),emoji('boy') )

  ggplot(df.tips,aes(x=total_bill,y=tip,color=sex,label=emoji))+
    geom_point()+ 
    geom_text(family="EmojiOne", size=14,show.legend = F)+
    labs(title='Mi primer gráfico con emojifont', 
         x='Total de Factura', 
         y='Propinas') +
    theme_bw()

