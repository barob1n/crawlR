#' CrawlR - Async Web Crawler for R
#'
#' @description
#' Batch based web-crawler utilizing the asynchronous features of R's curl
#' package to crawl through a list of user supplied websites to given depth.
#'
#' Each iteration consists of injecting seeds (if given), generating a
#' fetch list, fetching pages to disk, parsing pages, and then updates
#' the links in the crawlDB.
#'
#' After initial seeding, subsequent iterations query the crawlDB to generate a
#' fetch list.  Additional seeds can be added at any time. Re-seeding with
#' previously given seeds will re-crawl those seeds.
#'
#' @details
#' Each phase of the process is contained within a function of the *crawlR* package:
#'   1. *injectR* - Inject seeds into crawlDB.
#'   2. *generateR* - Generate fetch list from crawlDB.
#'   3. *fetchR* - Fetch links in fetch list.
#'   4. *parseR* - Parse fetched pages.
#'   5. *updateR* - Update crawlDB.
#'
#' These can be called individually or using the all-in-one *crawlR* function.
#'
#' @md
#'
#' @param seeds Seed url's. If null, then the work_dir must contain a linkDB.  If
#'     additional seeds are provided after inital seeding, the new seed url's
#'     will be added to linkDB and fetched.
#' @param work_dir (Required) Main working directory.
#' @param out_dir  Directory to store crawled and parsed html If null defaults to work directory.
#' @param max_concurr Max. total concurrent connections open at any given time.
#' @param max_concurr_host Max. total concurrent connections per host at any given time.
#' @param timeout Total (as in all url's in seed list) time per each iteration (for each depth).
#' @param timeout_request per url timeout.
#' @param external_site If true, crawler will follow external links.
#' @param crawl_delay  time (in seconds) for calls to the same host.
#'     Only applies if the  time is not specified by the host's robots.txt.
#' @param max_size Max size of file or webpage to download and parse.
#' @param regExIn url's matching this regular expression will be used.
#' @param regExOut  url's matching this reg-ex  will be filtered out, including url's that match regExIn.
#' @param depth Crawl depth for this crawl - A value of 1 only crawls the seed pages, 2 crawls links found on seeds, etc..
#' @param max_depth Where as the 'depth' variable determines the depth of the current crawl, 'max_depth' sets a maximum
#'    overall depth so that no link with depth higher than this value will be selected for crawling during the generate phase.
#' @param queue_scl (deprecated) max_concur * queue_scl gives que.
#' @param topN Select the 'topN' links based on score for crawling.
#' @param max_urls_per_host Maximum url's from each host when creating fetch list for each link depth.
#' @param parser Parsing function for page content to use.
#' @param score_func URL Scoring Function.
#' @param min_score minimum score during generate for urls
#' @param log_file Name of log file. If null, writes to stdout().
#' @param seeds_only If true, only seeds will be pulled from linkDB.
#' @param readability_content process content using readability
#' @param overwrite If true, data for url will be overwritten in linkDB.
#' @import curl
#' @import xml2
#' @importFrom magrittr  %>%
#'
#' @export
#'
#' @examples
#'
#' ## SETUP --------------------------------------------------------------------
#'
#' devtools::install_github('barob1n/crawlR)
#'
#' library(crawlR)
#'
#' # Create Seed List
#' seeds <- c("https://www.cnn.com", "https://www.npr.org")
#'
#' library(crawlR)
#'
#' # Run Crawler.
#' crawlR(seeds = seeds,
#'        work_dir="~/crawl/",
#'        out_dir = "~/crawl/news/",
#'        max_concurr = 50,
#'        max_concurr_host = 5,
#'        timeout = Inf,
#'        external_site = F,
#'        crawl_delay=5,
#'        max_size = 4e6,
#'        regExOut = NULL,
#'        regExIn = NULL,
#'        depth = 1,
#'        queue_scl = 1,
#'        topN=10,
#'        max_urls_per_host = 10,
#'        parser = crawlR::parse_content)
#'


crawlR <- function(
    seeds = NULL,
    work_dir=NULL,
    out_dir = NULL,
    max_concurr = 50,
    max_concurr_host = 1,
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
    readability_content=F,
    overwrite = F){

    tryCatch({
      val<-'Error message.'
      if(is.null(work_dir)) stop('Output directory not provided.')
      if(is.null(out_dir)) out_dir<-work_dir
      if(!(dir.exists(work_dir))) dir.create(work_dir)
      if(!(dir.exists(out_dir))) dir.create(out_dir)
      if(!grepl('/$',out_dir)) out_dir<-paste0(out_dir,'/')
      if(!grepl('/$',work_dir)) work_dir<-paste0(work_dir,'/')

      ## get input parameters to write to log file
      for(n in names(formals(crawlR::crawlR))){
        v<-get(n)
        if(n=='seeds') v<-NROW(v)
        if(class(v)=='function') v<-paste(n,'<- function()') ## use name if function
        write_log(paste0('    -',n,paste(rep(' ',30-nchar(n)),collapse=""),v), log_file)
      }

      ## log status and parameters

      write_log(paste0(rep('-',80),collapse=''), log_file)
      write_log(paste0(rep('-',80),collapse=''), log_file)
      write_log(paste0('crawlR: ',Sys.time(),' - Entering Crawler'), log_file)
      write_log(paste0('crawlR: Work Directory - ',work_dir), log_file)
      write_log(paste0('crawlR: Out Directory - ',out_dir), log_file)
      write_log(paste0('crawlR: Parameters:'), log_file)
      write_log(paste0(rep('-',80),collapse=''), log_file)
      write_log(paste0(rep('-',80),collapse=''), log_file)

      dir_check <- c(list.files(work_dir),"")
      if(is.null(seeds) & !file.exists(paste0(work_dir,'crawlDB.sqlite'))) stop('Seed list not provided.')

      ## Inject Seed list if provided
      if(!is.null(seeds)){
        write_log(paste('crawlR: Injecting',length(seeds), 'seed URLs -', Sys.time()), log_file)
        val <- injectR(out_dir=out_dir,work_dir=work_dir,seeds=seeds,log_file = log_file)
        if(class(val)=='error') stop(paste(val))
      }

      ## array of start and end times for depth level loop
      st_arr <- rep(NA,depth)
      et_arr <- rep(NA,depth)

      for(lvl in 1:depth){

        write_log(paste('crawlR:  Depth - ',lvl,'of', depth), log_file)
        write_log(paste('crawlR: Generating Fetch List -', Sys.time()), log_file)

        ## start time for loop
        st_arr[lvl]<-as.numeric(Sys.time())

        ## call generateR to generate a fetch list
        val<-crawlR::generateR(out_dir=out_dir,
                       work_dir=work_dir,
                       regExOut=regExOut,
                       regExIn=regExIn,
                       max_depth=max_depth,
                       topN = topN,
                       external_site=external_site,
                       max_urls_per_host = max_urls_per_host,
                       crawl_delay=crawl_delay,
                       log_file = log_file,
                       seeds_only=seeds_only,
                       min_score=min_score)

        if(class(val)=='error') stop(paste(val))

        write_log(paste('crawlR: Finished Fetch List -', Sys.time()), log_file)

        ## this_dir - directory for current crawl iteration
        this_dir <- paste0(find_last_dir(out_dir=gsub('/$','',out_dir)),"/")
        # out_dir <- this_dir
        # if(!file.exists(paste0(this_dir,'fetch_list.rda'))){
        #   write_log(paste('crawlR: Nothing to fetch.\n'), log_file)
        #   break
        # }

        write_log(paste('crawlR: Starting Fetcher -', Sys.time()), log_file)

        ## fetchR_parseR - fetch links output by generateR
        val<-fetchR_parseR_edit(out_dir=this_dir,
                    work_dir=work_dir,
                    fetch_list=NULL,
                    crawl_delay=crawl_delay,
                    max_concurr=max_concurr,
                    max_concurr_host=max_concurr_host,
                    timeout=timeout,
                    timeout_request=timeout_request,
                    queue_scl=queue_scl,
                    comments=paste0('Depth: ',lvl ),
                    log_file = log_file,
                    readability_content=readability_content,
                    parser=parser)

        if(class(val)=='error') stop(paste(val))

        write_log(paste('crawlR: Finished Fetching -', Sys.time()), log_file)
        write_log(paste('crawlR: Starting Parser -',Sys.time()), log_file)

        ## parseR - parse data from fetchR
        val <- parseR(this_dir = this_dir, parser=parser,log_file = log_file)
        if(class(val)=='error') stop(paste(val))

        write_log(paste('crawlR: Finihsed Parsing -',Sys.time()), log_file)
        write_log(paste('crawlR: Starting Updater -', Sys.time()), log_file)

        ## update crawlDB with output from parseR
        val<-updateR(work_dir=work_dir,
                     out_dir=this_dir,
                     log_file = log_file,
                     overwrite = overwrite,
                     score_func=score_func)

        if(class(val)=='error') stop(paste(val))

        write_log(paste('crawlR: Finished Updating -', Sys.time()), log_file)
        write_log(paste('crawlR: End of Depth Level:',lvl,'-', Sys.time()), log_file)

        ## end time for loop
        et_arr[lvl] <- as.numeric(Sys.time())

        write_log(paste('crawlR: Time for depth -',(et_arr[lvl]-st_arr[lvl]) ), log_file)
      }

      ## log status
      write_log(paste('crawlR: DONE -', Sys.time()), log_file)
      write_log(paste('crawlR: Average Time per Depth -',mean(et_arr-st_arr) ), log_file)
      write_log(paste0(rep('-',80),collapse=''), log_file)
      write_log(paste0(rep('-',80),collapse=''), log_file)
    },
    error = function(e){
      write_log(paste('crawlR: ',e), log_file)
      paste(val)
      #stop(paste(val))
    },
    finally = {
    }
  )

}






