install.packages("rtweet")
install.packages("tidytext")
install.packages("httpuv")

library(rtweet) ;  library(httpuv)
library(tidyverse) ; library(tidytext)


# Need to have a twitter account open. Browser will pop up and ask for approval. 
# Approve, close window, and rerun quiery. 
tweets2 <- search_tweets(q = "#Rstats", 
                        n = 10000,
                        retryonratelimit = TRUE,
                        include_rts = FALSE,
                        lang = "en")
# Todo, longer period




















# ???
create_token(
  app = "GaborBekes",
  consumer_key,
  consumer_secret,
  access_token = NULL,
  access_secret = NULL,
  set_renv = TRUE
)
