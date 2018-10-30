## Remove periods from initials

authorList$First.initial.s. <- str_remove_all(authorList$First.initial.s., "[.]")

authorList$Middle.initial.s. <- str_remove_all(authorList$Middle.initial.s., "[.]")

## map1 <- canada +
##    geom_point(aes(x=lng, y=lat), 
##               data = grantsperLoc)