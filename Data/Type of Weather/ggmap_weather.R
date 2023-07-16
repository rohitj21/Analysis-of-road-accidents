library(maptools)
library(ggplot2)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(sf)
library(dplyr)
library(viridis)
library(ggrepel)
library(ggthemes)
library(plotly)
shp0 = read_sf("IND_adm0.shp")
shp1 = read_sf("IND_adm1.shp")
shp2 = read_sf("IND_adm2.shp")
shp3 = read_sf("IND_adm3.shp")

#outline
p11 = ggplot(shp0)+
      geom_sf(fill = "#E8442A" , color = "#F53214")+
      ggthemes::theme_map()+
      labs(title = "India Country Map")

#statewise
p12 = ggplot(shp1)+
      geom_sf(aes(fill = NAME_1))+
      ggthemes::theme_map()+
      theme(legend.position = "none")+
      labs(title = "India States Map")+
      scale_fill_viridis_d()


read = read.csv("State-UT-Wise Accidents Classified According To Type Of Weather Condition During 2018.csv")
save(read , file = "Weather 2018.RData")
colnames(read) = c("S_no","States","Sunny_Total_Accidents" , "Sunny_Total_Killed" , "Sunny_Total_Injured" ,"Rainy_Total_Accidents" , "Rainy_Total_Killed" , "Rainy_Total_Injured" , "Foggy_Total_Accidents" , "Foggy_Total_Killed" , "Foggy_Total_Injured","Hail_Total_Accidents" , "Hail_Total_Killed" , "Hail_Total_Injured")
colnames(shp1)[5] <- "State"



# we make a function which plots the map given a type of weather and a type of incedent
plotMap <- function(weather, incident)
#Sunny Season
#Total Accidents
{
if(weather == "Sunny" & incident == "Accidents" )
{
shp1$sunny_total_accidents = read$Sunny_Total_Accidents
`Total Accidents` = shp1$sunny_total_accidents

ps1 = ggplot(shp1,aes(label = State,fill = `Total Accidents`))+
      geom_sf()+
      ggthemes::theme_map()+
      theme(legend.position = "right")+
      labs(title = "Accidents in Clear Weather")+
      theme(plot.title=element_text(hjust=0.5))+
      scale_fill_gradient(low = "#FEFBBD", high = "#DD7722") +  labs(fill = "Accidents" )
return ( (ps1))
}
else if(weather == "Sunny" & incident == "Injured" )
  {
     shp1$sunny_total_injured = read$Sunny_Total_Injured
     `Total Injured` = shp1$sunny_total_injured

      ps2 = ggplot(shp1,aes(label = State,fill = `Total Injured`))+
      geom_sf()+
      ggthemes::theme_map()+
      theme(legend.position = "right")+
      labs(title = "Injured Cases in Clear Weather")+
      theme(plot.title=element_text(hjust=0.5))+
      scale_fill_gradient(low = "#FEFBBD", high = "#DD7722") + labs(fill = "Injured")
    return( (ps2))
  }
  else if (weather == "Sunny" & incident == "Deaths")
  {
    shp1$sunny_total_killed = read$Sunny_Total_Killed
  `Total Killed` <- shp1$sunny_total_killed

  ps3 = ggplot(shp1,aes(label = State, fill = `Total Killed`))+
    geom_sf()+
    ggthemes::theme_map()+
    theme(legend.position = "right")+
    labs(title = "Fatalities in Clear Weather")+
    theme(plot.title=element_text(hjust=0.5))+
    scale_fill_gradient(low = "#FEFBBD", high = "#DD7722") +  labs(fill = "Killed" )
  return( (ps3))
  }



#Total Killed


#Rainy Season
#Total Accidents
else if(weather == "Rainy" & incident == "Accidents")
{
shp1$rainy_total_accidents = read$Rainy_Total_Accidents
`Total Accidents` = shp1$rainy_total_accidents

pr1 = ggplot(shp1,aes(label = State , fill = `Total Accidents`))+
      geom_sf()+
      ggthemes::theme_map()+
      theme(legend.position = "right")+
      labs(title = "Accidents in Rainy Weather")+
      theme(plot.title=element_text(hjust=0.5))+
      scale_fill_viridis(option = "mako" , direction = -1) + labs(fill = "Accidents")
return ( (pr1))
}
else if(weather == "Rainy" & incident == "Injured")

  {
      shp1$rainy_total_injured = read$Rainy_Total_Injured
     `Total Injured` = shp1$rainy_total_injured

      pr2 = ggplot(shp1,aes(label = State , fill = `Total Accidents`))+
      geom_sf(aes(fill = rainy_total_injured))+
      ggthemes::theme_map()+
      theme(legend.position = "right")+
      labs(title = "Injured Cases in Rainy Weather")+
      theme(plot.title=element_text(hjust=0.5))+
      scale_fill_viridis(option = "mako" , direction = -1) + labs(fill = "Injured")
      return ( (pr2))
  }
  else if(weather == "Rainy" & incident == "Deaths")
  {
      shp1$rainy_total_killed = read$Rainy_Total_Killed
     `Total Killed` = shp1$rainy_total_killed

      pr3 = ggplot(shp1,aes(label = State))+
      geom_sf(aes(fill = rainy_total_killed , fill = `Total Killed`))+
      ggthemes::theme_map()+
      theme(legend.position = "right")+
      labs(title = "Fatalities in Rainy Weather")+
      theme(plot.title=element_text(hjust=0.5))+
      scale_fill_viridis(option = "mako" , direction = -1) + labs(fill = "Killed")
      return( (pr3))
  }

#Total Injured

#Total Killed

#Foggy Weather
#Total Accidents
else if(weather == "Foggy" & incident == "Accidents")
{
shp1$foggy_total_accidents = read$Foggy_Total_Accidents
`Total Accidents` = shp1$foggy_total_accidents

pf1 = ggplot(shp1,aes(label = State , fill = `Total Accidents`))+
      geom_sf()+
      ggthemes::theme_map()+
      theme(legend.position = "right")+
      labs(title = "Accidents in Foggy Weather")+
      theme(plot.title=element_text(hjust=0.5))+
      scale_fill_gradient(low = "#ADADC9", high = "#3E3D53") +  labs(fill = "Accidents" )
return( (pf1))
}
else if(weather == "Foggy" & incident == "Injured" )
  {
     shp1$foggy_total_injured = read$Foggy_Total_Injured
    `Total Injured` = shp1$foggy_total_injured

      pf2 = ggplot(shp1,aes(label = State , fill = `Total Injured`))+
      geom_sf()+
      ggthemes::theme_map()+
      theme(legend.position = "right")+
      labs(title = "Injured Cases in Foggy Weather")+
      theme(plot.title=element_text(hjust=0.5))+
      scale_fill_gradient(low = "#ADADC9", high = "#3E3D53") +  labs(fill = "Injured" )
      return( (pf2))
  }
  else if(weather == "Foggy" & incident == "Deaths" )
  {
      shp1$foggy_total_killed = read$Foggy_Total_Killed
     `Total Killed` = shp1$foggy_total_killed

      pf3 = ggplot(shp1,aes(label = State , fill = `Total Killed`))+
      geom_sf()+
      ggthemes::theme_map()+
      theme(legend.position = "right")+
      labs(title = "Fatalities in Clear Weather")+
      theme(plot.title=element_text(hjust=0.5))+
      scale_fill_gradient(low = "#ADADC9", high = "#3E3D53") +  labs(fill = "Accidents" )
      return( (pf3))
  }




#Total Killed

#Hail Season
#Total Accidents
else if(weather == "Haily" & incident == "Accidents")
{
    shp1$hail_total_accidents = read$Hail_Total_Accidents
   `Total Accidents` = shp1$hail_total_accidents

    ph1 = ggplot(shp1,aes(label = State , fill = `Total Accidents`))+
    geom_sf()+
    ggthemes::theme_map()+
    theme(legend.position = "right")+
    labs(title = "Accidents in Hail Season")+
    theme(plot.title=element_text(hjust=0.5))+
    scale_fill_gradient(low = "#EFEAD8", high = "#00FFAB") +  labs(fill = "Accidents" )
    return ( (ph1))
}
else if(weather == "Haily" & incident == "Injured")

  {
     shp1$hail_total_injured = read$Hail_Total_Injured
     `Total Injured` = shp1$hail_total_injured

      ph2 = ggplot(shp1,aes(label = State , fill = `Total Injured`))+
      geom_sf()+
      ggthemes::theme_map()+
      theme(legend.position = "right")+
      labs(title = "Injured Cases in Hail Season")+
      theme(plot.title=element_text(hjust=0.5))+
      scale_fill_gradient(low = "#EFEAD8", high = "#00FFAB") +  labs(fill = "Injured" )+
      return ( (ph2))
  }
  else if(weather == "Haily" & incident == "Deaths")

  {
      shp1$hail_total_killed = read$Hail_Total_Killed
     `Total Killed` = shp1$hail_total_killed

      ph3 = ggplot(shp1,aes(label = State , fill = `Total Killed`))+
      geom_sf()+
      ggthemes::theme_map()+
      theme(legend.position = "right")+
      labs(title = "Fatalities in Hail Season")+
      theme(plot.title=element_text(hjust=0.5))+
      scale_fill_gradient(low = "#EFEAD8", high = "#00FFAB") +  labs(fill = "Killed" )
      return( (ph3))
  }
}

save(plotMap,read, shp0,shp1, shp2, shp3, file = "Weather 2018.RData")
plotMap("Sunny", "Accidents")
