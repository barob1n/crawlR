# `crawlR`: Web Crawler for R

## Description


 Batch based web crawler that utilizes the asynchronous features of R's curl package to crawl through a
 list of user supplied websites.  
 
 Basic process is:
 1. *Inject* seeds into linkDB
 2. *Generate* fetch list from linkDB
 3. *Fetch* links
 4. *Update* linkDB
 4. *Repeat*

 

## Usage

```r
crawlR(
    seeds = NULL,
    work_dir=NULL,
    out_dir = NULL,
    max_concurr = 50,
    max_host = 1,
    timeout = Inf,
    timeout_request=30,
    external_site = F,
    crawl_delay=30,
    max_size = 10e6,
    regExIn = NULL,
    regExOut = NULL,
    depth = 1,
    max_depth=3,
    queue_scl = 1,
    topN=NULL,
    max_urls_per_host = 10,
    parser = crawlR:::parse_content,
    score_func=NULL,
    min_score=0.0,
    log_file = NULL,
    seeds_only = F,
    crawl_int=NULL,
    readability_content=F,
    overwrite = F)
```


## Arguments

Argument      |Description
------------- |----------------
```seeds```     |     Seed URL's. If NULL, then the work_dir must containg a linkDB.  If additional seeds are provided after inital seeding, the new seed URL's will be added to linkDB and fetched.
```work_dir```     |     (Required) Working to store results.
```out_dir```     |     Directory to store results. If NULL defaults to work directory.
```max_concurr```     |     Max. total concurrent connections open at any given time.
```max_host```     |     Max. total concurrent connections per host at any given time.
```timeout```     |     Total (as in all url's in fetch list) time per each iteration (for each depth).
```timeout_request```	| Per url timeout.
```external_site```     |     If true, crawler will follow external links.
```crawl_delay```     |     time (in seconds) for calls to the same host. Only applies if the  time is not specified by the host's robots.txt.
```max_size```     |     Max size of file or webpage to download and parse.
```regExIn```     |     URL's matching this regular expression will be used.
```regExOut```     |     URL's matching this reg-ex  will be filtered out, including URL's that match regExIn.
```depth```     |     Crawl depth for this crawl - A value of 1 only crawls the seed pages, 2 crawls links found on seeds, etc..
```max_depth```     |     Where as the 'depth' variable determines the depth of the current crawl, 'max_depth' sets a maximum overall depth so that no link with depth higher than this value will be selected for crawling during the generate phase.
```queue_scl```     |     (Deprecated) max_concur * queue_scl gives que.
```topN```     |     Top num links to fetch per per link depth iteration.
```max_urls_per_host```     |     Maximum URL's from each host when creating fetch list for each link depth.
```parser```     |     Parsing function to use.
```score_func```	|	URL Scoring Function.
```min_score```	|	Minimum score during generate for urls.
```log_file```	|	Name of log file. If null, writes to stdout().
```seeds_only```	|	If true, only seeds will be pulled from linkDB.
```readability_content```	|	Process content using readability python module.
```overwrite```	|	If true, data for url will be overwritten in crawlDB.


## Details


 After each iteration of crawling, the crawled
 pages are read from disk, parsed, and writen
 back to disk. 


## Examples

```r 
 devtools::install_github("barob1n/crawlR")
 
 ## Create Seed List
 seeds <- c("https://www.cnn.com", "https://www.npr.org")
 
 ## Creates crawlDB, inject seeds, and crawl 
 
 crawlR(seeds = seeds,
  work_dir="~/crawl",
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
  
 
## Crawl again, this time using filters.
 
filter_in=paste0("police,shooting,gun control")
filter_out=paste0("sports,weather")
 
 crawlR(seeds = NULL,       # no seeds - will query crawlDB
  work_dir= "~/crawl/",
  out_dir = "~/crawl/news/",
  max_concurr = 50,
  max_host = 5,
  timeout = Inf,
  external_site = F,
  crawl_delay=1,
  max_size = 4e6,
  regExOut = filter_out,    # filter out these
  regExIn = filter_in,      # url's must have these 
  depth = 1,
  queue_scl = 1,
  topN=10,
  max_urls_per_host = 10,
  parser = crawlR::parse_content)
  
 
 ## Run a third time, providing some new/additional seeds.
 
 new_seeds <- c("https://ge.com", "https://www.ford.com")
 
 crawlR(seeds = new_seeds,  # seeds will be added to crawlDB 
 work_dir= "~/crawl/",
 out_dir = "~/crawl/auto/",
 max_concurr = 50,
  max_host = 5,
  timeout = Inf,
  external_site = F,
  crawl_delay=1,
  max_size = 4e6,
  regExOut = filter_out,
  regExIn = filter_in,    
  depth = 1,
  queue_scl = 1,
  topN=10,
  max_urls_per_host = 10,
  parser = crawlR::parse_content)
 
 
 
 
 ``` 

