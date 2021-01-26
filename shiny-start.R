# gabors-data-analysis

install.packages('rsconnect')
library(rsconnect)

rsconnect::setAccountInfo(name='gabors-data-analysis',
                          token='99969DF648E0532FDBAFC8040C025E4C',
                          secret='<SECRET>')

library(rsconnect)
rsconnect::deployApp('path/to/your/app'