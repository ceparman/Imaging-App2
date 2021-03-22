
bar_plot<-function(scaled,summary_data,compound,ratio,plot_comments)
{
plot_data <- scaled %>% ungroup()

plot_data <- plot_data %>% mutate( maxy = pmax( (mean_all_area + std_all_area), (mean_gfp_area + std_gfp_area),
                                                mean_all_area, mean_gfp_area , na.rm = TRUE))


plot_data <- plot_data %>% filter(var_compound == compound |!is.na(control))  

if(is.null(plot_comments)){
  plot_comment <- ""
} else {
  
  if(compound %in% names(plot_comments))  {
    plot_comment <-   plot_comments[[compound]]
  } else plot_comment <- ""
}


#stack the data

gfp_data <- plot_data %>% select(mean_gfp_area, std_gfp_area,concentration,units,control,mean_percent,maxy,p_value,p_value_counts,p_value_area ) %>% 
  mutate(target = "Total GFP+ Area")%>% mutate ( mean = mean_gfp_area, std = std_gfp_area ) %>% 
  select(-mean_gfp_area, -std_gfp_area)

all_data <- plot_data %>% select(mean_all_area, std_all_area,concentration,units,control, mean_percent,maxy,p_value,p_value_counts, p_value_area ) %>% 
  mutate(target = "Total Cell Area")%>% mutate ( mean = mean_all_area, std = std_all_area ) %>% 
  select(-mean_all_area, -std_all_area)




bar_data <- bind_rows(gfp_data,all_data)


bar_data <-  bar_data %>% mutate( x_label = ifelse( is.na(units), concentration, paste(stars(p_value_area),concentration,units) ))

suppressWarnings(
  bar_data <-  bar_data %>% mutate(plotorder = ifelse(is.na(control), (as.numeric(concentration,rm.na=TRUE)*10000 +10), as.numeric(control,rm.na=TRUE))) 
)






title <- paste0(plot_data$var_compound[ is.na(plot_data$control) ][1], " CRC with ",
                stringr::str_split(plot_data$content[ is.na(plot_data$control) ][1],":")[[1]][1] )









#need to remove ever other mean_percent  

bar_data <- bar_data %>% mutate( bar_label = ifelse ( target == "Total GFP+ Area",
                                                         paste0(as.character(round(mean_percent,1)),"%",
                                                                stars(p_value)),""))




options(scipen=10000)

#bardata <- bar_data[order(bar_data$target),]

bp<- ggplot(bar_data ,aes(fill= factor(target, levels=c("Total GFP+ Area", "Total Cell Area")),y=mean,x=reorder(x_label,plotorder))) +
  
  
  geom_errorbar(aes(ymin=mean-std,ymax=mean+std),  width=0.25,position = position_dodge(.9)) +
  
  geom_bar(position="dodge", stat="identity" )+
  
  scale_fill_manual(values=rep(c("green","red"),nrow(bar_data)/2))+
  
  scale_y_continuous(sec.axis = sec_axis(~.*(1/ratio),name = "Total Cell Area")) +
  labs(y= "Total GFP(+) Cell Area", x="") 
  
####################

if(!is.null(plot_comment)) {
  
  if( plot_comment == "" ) {bp <- bp +  ggtitle(title)
  } else {
    bp <- bp +  ggtitle(label = title,subtitle = plot_comment )
  }
} else {bp <- bp +  ggtitle(title)}


############  

  bp <- bp +
  
  geom_text(
    aes(label = bar_label, y = maxy*1.05),
    position = position_dodge(.1),
    vjust = 0
  ) +
  
  
  theme_classic()+
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 16), legend.position = "top",legend.title=element_blank()) +
  theme(legend.text = element_text(size= 14)) +
  theme(axis.text.y = element_text(size =16))+
  theme(axis.text.y.right = element_text(size =16))+
  theme(axis.title.y = element_text(colour = "green",size =14),
        axis.title.y.right = element_text(color = "red")) +
  theme(axis.title.y.right = element_text(angle = 270, size = 14)) 

bp


}
