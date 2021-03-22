

#plot


percent_plot <- function(well_data,summary_data,test_compound)
{
  

summary_data <- summary_data %>% filter(var_compound == test_compound    | !is.na(control))
  
percent_data <- well_data %>% filter(var_compound == test_compound    | !is.na(control))  


compound_percent_data <- percent_data %>% mutate( concentration = ifelse( is.na(control),concentration,0) ) %>% 
                          group_by(content) %>% mutate( x_label = ifelse( is.na(units), "Controls", paste(concentration,units) )) %>%
                         mutate( cat = ifelse( is.na(units),  "green","red")) %>%
                           mutate(plotorder = ifelse(is.na(control), (as.numeric(concentration,rm.na=TRUE)*10000 +10), as.numeric(control,rm.na=TRUE)))
                        

range <- compound_percent_data %>% summarise( maxp = max(percent,na.rm = TRUE) , minp = min(percent,na.rm = TRUE),
                                              concentration = max(concentration))

var_data <- summary_data %>% filter( is.na(control)) %>%
  mutate( x_label =  paste(concentration,units)) %>%
  mutate( text_label  = stars(p_value)) %>%
  merge(range)



title <- paste0(percent_data$var_compound[ is.na(percent_data$control) ][1], " CRC with ",
                stringr::str_split(percent_data$content[ is.na(percent_data$control) ][1],":")[[1]][1] )
colors <- c("red","blue","orange","purple")

pp<- ggplot(compound_percent_data,aes(x=reorder(x_label,plotorder),y=as.numeric(percent),color=var_compound)) + 
 
  geom_point(aes(fill=var_compound),shape=1,size=3)    + # geom_line() +

  labs(y= "% GFP(+) Cell Area", x="Concentration") +
  theme_minimal()+
  theme(legend.position = "right",legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle(title) + expand_limits(y = 0)+
 
 scale_color_manual(values=c("green",colors[1:length(unique(compound_percent_data$compound))]))
##############################



pp <- pp +  geom_text(data = var_data,
  aes(label = text_label, y = maxp+5,x=x_label),
  position = position_dodge(.1),
  vjust = 0 ,color="black"
) 


pp



}
