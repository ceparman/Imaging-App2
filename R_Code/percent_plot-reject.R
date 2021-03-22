

#plot
library(ggplot2)

percent_plot_reject <- function(plot_data)
{
  
compound_percent_data <- plot_data$compound_percent_data


percent_data <- plot_data$percent_data
range <- plot_data$range
summary_data2  <- plot_data$summary_data2
plot_comment <- plot_data$plot_comment
  
 var_data <- summary_data2 %>% filter( is.na(control)) %>%
  mutate( x_label =  paste(concentration,units)) %>%
  mutate( text_label  = stars(p_value)) %>%
  merge(range)



title <- paste0(percent_data$var_compound[ is.na(percent_data$control) ][1], " CRC with ",
                stringr::str_split(percent_data$content[ is.na(percent_data$control) ][1],":")[[1]][1] )
colors <- c("red","blue","orange","purple")



rejected_data <- compound_percent_data %>% filter(rejected == 1) 


pp<- ggplot(compound_percent_data,aes(x=x,y=y,color=var_compound)) + 

  geom_point(aes(fill=var_compound),shape=1,size=3) +
  

  labs(y= "% GFP(+) Cell Area", x="Concentration") +
  theme_minimal()+
  theme(legend.position = "right",legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  expand_limits(y = 0)+
  scale_color_manual(values=c("green",colors[1:length(unique(compound_percent_data$compound))]))
##############################

####################
   
     if(!is.null(plot_comment)) {
         
                                 if( plot_comment == "" ) {pp <- pp +  ggtitle(title)
                                 } else {
                                  pp <- pp +  ggtitle(label = title,subtitle = plot_data$plot_comment )
                                 }
   } else {pp <- pp +  ggtitle(title)}
  
  
  ############  



pp<- pp + geom_point(data=compound_percent_data[compound_percent_data$rejected == 1,], shape=4,color="red",size=3)

pp <- pp +  geom_text(data = var_data,
  aes(label = text_label, y = maxp+5,x=x_label),
  position = position_dodge(.1),
  vjust = 0 ,color="black"
) 


pp



}
