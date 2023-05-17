# Create Seed List
seeds <- c("https://www.cnn.com", "https://www.npr.org")

work_dir="E:/crawl/"
out_dir = "E:/crawl/news/"
max_concurr = 50
max_concurr_host = 1
timeout = Inf
timeout_request = 30
external_site = F
crawl_delay = 10
max_size = 1e+07
regExIn = NULL
regExOut = NULL
depth = 1
max_depth = 10
queue_scl = 1
topN = 1000000
max_urls_per_host = 1000
parser = crawlR:::parse_content
score_func = NULL
min_score = 0
log_file = 'E:/crawl/crawl_log.txt'
seeds_only = F
readability_content = F
overwrite = T


library(crawlR)

# Run Crawler.
crawlR(seeds = seeds,
       work_dir="E:/crawl/",
       out_dir = "E:/crawl/news/",
       max_concurr = 50,
       max_concurr_host = 1,
       timeout = Inf,
       timeout_request = 30,
       external_site = F,
       crawl_delay = 10,
       max_size = 1e+07,
       regExIn = NULL,
       regExOut = NULL,
       depth = 1,
       max_depth = 10,
       queue_scl = 1,
       topN = 1000000,
       max_urls_per_host = 1000,
       parser = crawlR:::parse_content,
       score_func = NULL,
       min_score = 0,
       log_file = 'E:/crawl/crawl_log.txt',
       seeds_only = F,
       readability_content = F,
       overwrite = T)


