
devtools::document()
devtools::install(upgrade='never')

library(crawlR)

# Create Seed List
seeds <- c("https://www.cnn.com", "https://www.npr.org")

this_dir<-"C:/Users/brian/Documents/crawl/news/fetch_20220216212032"
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
timeout_request<-30
score_func<-NULL
min_score<-0
lvl<-1

regExIn = NULL
regExOut = NULL
depth = 1
max_depth=3
queue_scl = 1
topN=NULL
max_urls_per_host = 10
parser = crawlR:::parse_content
score_func=NULL
min_score=0.0
log_file = NULL
seeds_only = F
readability_content=F
overwrite = F
write_log<-crawlR::write_log
max_concurr_host=1
comments<-""
curl_opts=list(
  "User-Agent"="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.117 Safari/537.36",
  "Accept-Language" = "en;q=0.7",
  "Connection" = "close",
  "CURLOPT_DNS_CACHE_TIMEOUT" = "3600")
status_print_interval<-1
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
