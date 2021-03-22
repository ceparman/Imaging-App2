plot.save <- function(plot, 
                      width = 800, 
                      height = 500, 
                      text.factor = 1, 
                      filename = paste0(
                        format(
                          Sys.time(), 
                          format = '%Y%m%d-%H%M%S'), '-Rplot.png'
                      )
) {
  
  dpi <- text.factor * 100
  width.calc <- width / dpi
  height.calc <- height / dpi
  # # 
   ggsave(filename = filename,
          dpi = dpi,
          width = width.calc,
          height = height.calc,
          units = 'in',
          plot = plot)
   
   
  
  
}