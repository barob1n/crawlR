# `crawlR`: CrawlR - Crawl, Parse, and Index

## Description


 Utilizes the curl package to crawl through a
 list of user supplied websites to given depth.
 Unlesss overriden, crawlR observes robots.txt.


## Usage

```r
crawlR(seeds = NULL, work_dir = NULL, out_dir = NULL,
  max_concurr = 2, max_host = 1, timeout = Inf, external_site = F,
  sitemaps = F, crawl_delay = 10, max_size = 4e+06, regExIn = NULL,
  regExOut = NULL, depth = 4, queue_scl = 1, topN = NULL,
  max_urls_per_host = 10, n_threads = 1, parser = parseR)
```


## Arguments

Argument      |Description
------------- |----------------
```seeds```     |     Seed URL's. If NULL, then the work_dir must containg a linkDB.  If additional seeds are provided after inital seeding, the new seed URL's will be added to linkDB and fetched - keeping the original seeds/fetched pages.
```work_dir```     |     (Required) Working to store results.
```out_dir```     |     Directory to store results. If NULL defaults to work directory.
```max_concurr```     |     Max. total concurrent connections open at any given time.
```max_host```     |     Max. total concurrent connections per host at any given time.
```timeout```     |     Total (as in all url's in seed list) time per each iteration (for each depth).
```external_site```     |     If true, crawler will follow external links.
```crawl_delay```     |     time (in seconds) for calls to the same host. Only applies if the  time is not specified by the host's robots.txt.
```max_size```     |     Max size of file or webpage to download and parse.
```regExIn```     |     URL's matching this regular expression will be used.
```regExOut```     |     URL's matching this reg-ex  will be filtered out, including URL's that match regExIn.
```depth```     |     Crawl depth - A value of 1 only crawls the seed pages, 2 crawls links found on seeds, etc..
```queue_scl```     |     (Deprecated) max_concur * queue_scl gives que.
```topN```     |     Top num links to fetch per per link depth iteration.
```max_urls_per_host```     |     Maximum URL's from each host when creating fetch list for each link depth.
```n_threads```     |     Only applies to parsing.
```parser```     |     Parsing function to use.

## Details


 After each iteration of crawling, the crawled
 pages are read from disk, parsed, and writen
 back to disk. The read/parse phase is done in
 parrallel using the future package


## Examples

```r 
 
 # Create Seed List
 seeds <- c("https://cran.r-project.org/", "https://www.wikipedia.org/")
 
 # Set up URL filters.
 url_filter <- list()
 
 # No filter for first iteration.
 url_filter[[1]] <- NA
 
 # Crawl all seeds on 1st iteration, but only follow links
 # containing "news" or "announce"  for 2nd/3rd iteration.
 
 url_filter[[2]] <- ".*news.*|.*announc.*"
 url_filter[[3]] <- ".*news.*|.*announc.*"
 
 # Run Crawler.
 crawlR(seeds = seeds,
 work_dir="~/crawl/",
 out_dir = "~/crawl/news/",
 max_concurr = 100,
 max_host = 1,
 timeout = Inf,
 external_site = F,
 sitemaps = F,
 crawl_delay=10,
 max_size = 4e6,
 regExOut = url_filter,
 regExIn = NULL,
 depth = 4,
 queue_scl = 1,
 topN=20000,
 max_urls_per_host = 10,
 n_threads=1,
 parser = parseR)
 
 
 # Run again with  a differnt URL filter and  save
 # to different directory. The same directory could
 # have been used - previous results WOULD NOT
 # have been written over.
 
 url_filter[[1]] <- ".*financial*|.*invest.*"
 url_filter[[2]] <- ".*financial*|.*invest.*"
 
 crawlR(seeds = NULL,
 work_dir= "~/crawl/",
 out_dir = "~/crawl/invest/",
 max_concurr = 100,
 max_host = 1,
 timeout = Inf,
 external_site = F,
 sitemaps = F,
 crawl_delay=10,
 max_size = 4e6,
 regExOut = url_filter,
 regExIn = NULL,
 depth = 2,
 queue_scl = 1,
 topN=20000,
 max_urls_per_host = 10,
 n_threads=1,
 parser = parseR)
 
 
 # Run a third time, providing some new/additional seeds.
 
 new_seeds <- c("https://ge.com", "https://www.ford.com")
 
 crawlR(seeds = new_seeds,
 work_dir= "~/crawl/",
 out_dir = "~/crawl/",
 max_concurr = 100,
 max_host = 1,
 timeout = Inf,
 external_site = F,
 sitemaps = F,
 crawl_delay=10,
 max_size = 4e6,
 regExOut = NULL,
 regExIn = NULL,
 depth = 2,
 queue_scl = 1,
 topN=20000,
 max_urls_per_host = 10,
 n_threads=1,
 parser = parseR)
 
 
 
 
 ``` 

