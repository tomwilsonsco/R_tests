library(dplyr)
library(ggplot2)


valuesDate = read.csv('s1DatesAllSamplesMedianEdge.csv', stringsAsFactors = FALSE)

valuesDate$date <- as.Date(valuesDate$date, format = "%d-%m-%Y")


pixels_sum = valuesDate %>% group_by(date, TYPE) %>%
  summarise(VH = mean(VH), VV = mean(VV))

pixels_broadleaved = pixels_sum %>% filter(TYPE == 'broadleaved')

ggplot()+
  geom_smooth(data = pixels_broadleaved, aes(x=date, y=VH), colour = 'red') +
  geom_line(data = pixels_broadleaved, aes(x=date, y=VV), colour = 'blue')


randPick = function(type){
  subset = valuesDate %>% filter(TYPE == type)
  vals = min(subset$UNIQUEID):max(subset$UNIQUEID)
  v = sample(vals, 1)
  return (valuesDate %>% filter(UNIQUEID == v))
}

test_poly = randPick('felled')

ggplot()+
  geom_line(data = test_poly, aes(x=date, y=VH), colour = 'red')+
  geom_line(data = test_poly, aes(x=date, y=VV), colour = 'blue')
