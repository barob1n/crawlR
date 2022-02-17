
devtools::document()
devtools::install(upgrade='never')

# Create Seed List
seeds <- c("https://www.cnn.com", "https://www.npr.org")

this_dir<-"C:/Users/brian/Documents/crawl/news/fetch_20220119215224"
log_file<-NULL
out_dir<-"~/crawl/news/"
work_dir<-"~/crawl/"

max_concurr = 50
max_host = 5
timeout = Inf
external_site = F
crawl_delay=1
max_size = 4e6
regExOut = NULL
regExIn = NULL
depth = 1
queue_scl = 1
topN=10
max_urls_per_host = 10
parser = crawlR::parse_content
max_depth<-2
seeds_only<-2

# Crawl all seeds on 1st iteration, but only follow links
library(crawlR)
# Run Crawler.
crawlR(seeds = seeds,
       work_dir="~/crawl/",
       out_dir = "~/crawl/news/",
       max_concurr = 50,
       max_host = 5,
       timeout = Inf,
       external_site = F,
       crawl_delay=1,
       max_size = 4e6,
       regExOut = NULL,
       regExIn = NULL,
       depth = 1,
       queue_scl = 1,
       topN=10,
       max_urls_per_host = 10,
       parser = crawlR::parse_content)
