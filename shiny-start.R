# gabors-data-analysis

install.packages('rsconnect')
library(rsconnect)

rsconnect::setAccountInfo(name='gabors-data-analysis',
                          token='99969DF648E0532FDBAFC8040C025E4C',
                          secret='rGF1D4lQ3rtKYaqWP+pNtHFGJ0gbq28rjdG3wj0c')


library(rsconnect)
rsconnect::deployApp('path/to/your/app'