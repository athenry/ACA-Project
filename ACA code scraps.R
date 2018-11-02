## Remove periods from initials

authorList$First.initial.s. <- str_remove_all(authorList$First.initial.s., "[.]")

authorList$Middle.initial.s. <- str_remove_all(authorList$Middle.initial.s., "[.]")

## map1 <- canada +
##    geom_point(aes(x=lng, y=lat), 
##               data = grantsperLoc)

## canada <- ggplot() + borders(database = "worldHires", "Canada", colour = "gray80", fill = "gray85") + theme_map()

## map1 <- canada +
##geom_point(aes(x=lon, y=lat), 
##           data = grantsperLoc,
##           colour = 'green', alpha = .5)