### Create Empty Plate reference


makePlate <- function()  # Makes an empty plate
  
{
  plate <- ""
  
  for (r in LETTERS[1:8])
  {
    for (c in 1:12)
    {
      plate <-c(plate,  paste0(r,c))
    }
  }
  plate <- data.frame(index = 1:96,Well=plate[2:97],stringsAsFactors = FALSE)
  
  plate <- plate[order(plate$index),]
  
  plate
}
