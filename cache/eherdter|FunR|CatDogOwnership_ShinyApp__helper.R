percent_map <- function(bigdata, var, color) {
  
  
  #make chloropleth template map
  
  #fill will be the variable to change
  ggplot(bigdata,
              aes(x = long, y = lat,
              group = group, fill = var)) + 
  
              geom_polygon(color = "gray90", size = 0.1) +
              coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
              labs(title = "", fill = NULL) + 
                theme_map()+
              labs(fill = "Percent")+
              scale_fill_gradient(low = "white", high = color)
  
}

