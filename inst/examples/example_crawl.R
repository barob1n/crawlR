# Create Seed List
seeds <- c("https://www.cnn.com", "https://www.npr.org")

library(crawlR)

# Run Crawler.
crawlR(seeds = seeds,
       work_dir="~crawl/",
       out_dir = "~crawl/news/",
       max_concurr = 50,
       max_host = 5,
       timeout = Inf,
       external_site = F,
       crawl_delay=5,
       max_size = 4e6,
       regExOut = NULL,
       regExIn = NULL,
       depth = 1,
       queue_scl = 1,
       topN=10,
       max_urls_per_host = 10,
       parser = crawlR::parse_content)


