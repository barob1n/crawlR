#' Fetch a List of Url's.
#'
#' @description
#' Fetches list of URL's created by the generateR() function.
#'
#' @md
#'
#' @param out_dir (Required) Current output directory.
#' @param work_dir (Required) Current working directory.
#' @param fetch_list (Required) Created by generateR.R.
#' @param crawl_delay time (in seconds) for calls to the same host.
#' @param max_concurr Max. total concurrent connections open at any given time.
#' @param max_concurr_host Max. total concurrent connections per host at any given time.
#' @param timeout Total (all requests) timeout
#' @param timeout_request per request timeout
#' @param queue_scl Scaler
#' @param comments Some comments to print while running.
#' @param log_file Name of log file. If null, writes to stdout().
#' @param readability_content T
#' @param parser parse func
#' @param writer placeholder to allow custom output functions
#' @param status_print_interval num urls fetched between crawler status outputs
#' @param curl_opts list of curl options
#' @export
#'

fetchR_parseR_edit<- function(
   out_dir=NULL,
   work_dir=NULL,
   fetch_list=NULL,
   crawl_delay=NULL,
   max_concurr=NULL,
   max_concurr_host=NULL,
   timeout=Inf,
   timeout_request=NULL,
   queue_scl=1,
   comments="",
   log_file=NULL,
   readability_content=F,
   parser=crawlR::parse_content,
   writer=NULL,
   status_print_interval=500,
   curl_opts=list(
     "User-Agent"="Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.117 Safari/537.36",
     "Accept-Language" = "en;q=0.7",
     "Connection" = "close",
     "CURLOPT_DNS_CACHE_TIMEOUT" = "3600")){

  log_con<-set_log_file(log_file)

  tryCatch({

    if(is.null(out_dir)){stop(' no output directory provided.')}

    ## python readability plugin
    if(readability_content){
      readability <- import("readability") # pip install readability-lxml
    }else{
      readability<-NULL
    }

    ## Main Fetching function - based on crawl example in curl package.
    do_fetch <- function(url,depth,json_out){
      h <- curl::new_handle(CONNECTTIMEOUT = 10)
      curl::handle_setopt(h, .list=list(timeout=timeout_request)) ## per request timeout
      curl::handle_setheaders(h,.list=curl_opts)
       
      curl::curl_fetch_multi(url, handle = h, pool = pool, done = function(res){
        
        ## values used later in processing flow
        res$url <- url
        res$depth <- depth
        res$hash_name <- paste0('crawlR_', openssl::md5(res$url), collapse='')
        res$headers <- rawToChar(res$headers)
        queue$tot_fetched <- queue$tot_fetched + 1
        queue$tot_batch_fetched <- queue$tot_batch_fetched + 1
        
        ## if the request is good, process result otherwise set as null
        if(res$status >=200 & res$status<300){
          
          ## hand off to content to supplied parser.  
          tryCatch({
            res$content <- parser(res,readability=NULL,readability_content=F)
            writeLines(jsonlite::toJSON(list(ename=res)), con=json_out)
          },
          error = function(e) {
            writeLines(paste('fetchR:', url,'-',substr(e,1,100)),con = log_con)
          })

        }else{
          res$content <- NULL
        }

        update_queue(queue)
      }, 
      fail = function(errmsg){
        writeLines(paste('fetchR:',errmsg),con = log_con)
      
        update_queue(queue)
      })
    }
    
    
   
    ## update the queue
    update_queue<-function(queue){
      
      ## update counters
      queue$tot_fetched <- queue$tot_fetched + 1
      queue$tot_batch_fetched <- queue$tot_batch_fetched + 1
      
      ## print current status.
      if(queue$tot_fetched %% status_print_interval ==0){
        write_status(queue,log_con)
      }
    
      ## check if more url's in fetch list. if so, add one, else check if 
      ## any url's are still pending in the pool.  if not queue another batch, 
      ## else do nothing.
      if(NROW(queue$fetch_list)>0){
        do_fetch(url=queue$fetch_list,queue$depth[i], out_dir)
        queue$fetch_list<-queue$fetch_list[-1]
      }else if(sum(queue$status$pending) == 0 & queue$batch<queue$batch_count){
        Sys.sleep(crawl_delay)
        queue$tot_batch_fetched<-0
        queue$batch_time <- Sys.time()
        queue$fetch_list<-queue_batch(fetch_db,batch=queue$batch)
      }else{
        ## do nothing
      }
    }

    ## print current crawler status
    write_status<-function(queue,log_con){
      tot_left <- queue$links_in_batch -  1
      thisDt   <- as.numeric(as.numeric(Sys.time())- as.numeric(queue$batch_time))
      writeLines(
        paste('\nBatch Num:', queue$batch, 'of', queue$batch_count,
              '\nLinks Left in Batch:', queue$links_in_batch - queue$links_in_queue,
              '\nNum Done in Batch: ', queue$tot_batch_fetched,
              '\nTot Num Done: ',queue$tot_fetched,
              '\nPending in pool: ', length(curl::multi_list(pool)),
              '\nRun Time for Batch: ', round(thisDt), 'seconds',
              '\nTotal Num Links: ', queue$tot_urls,
              '\nRate: ', round(queue$tot_fetched/thisDt,1), 'Pages/Sec.',
              '\nComments:', comments,
              '\nDate/Time:', Sys.time(),
              '\n\n'),
        con = log_con)
    }

    ## get number urls in batch
    get_num_urls<-function(fetch_db,batch){
      DBI::dbGetQuery(
        fetch_db,
        paste("select count(*) from fetch_list
               where batch = ",batch))
    }

    ## load a batch of urls from fetch list and sets queue values
    queue_batch<-function(fetch_db,batch=1,offset=0,limit=100000){
      
      q<-paste("select * from fetch_list 
               where batch = ",batch,"limit",limit,"offset",offset)
      
      fetch_list<-DBI::dbGetQuery(fetch_db,q) 
      
      ## update batch number, depth, and fetch list
      queue$batch<-batch
      queue$fetch_list<-fetch_list$url
      queue$depth <- fetch_list$depth

      #queue$depth<-fetch_list$depth
      ## push into fetcher
      for(i in 1:(min(queue$queue_size,NROW(queue$fetch_list)))){
        do_fetch(url=queue$fetch_list[1],depth=queue$depth[i],json_out=json_out)
        queue$fetch_list <- queue$fetch_list[-1]
        queue$depth <- queue$depth[-1]
      }
    }
    

    ## the queue...
    queue <- new.env()
    queue$init_time <- Sys.time()
    queue$batch_time <- Sys.time()
    queue$pending <- length(curl::multi_list(pool))
    queue$tot_fetched <- 0
    queue$tot_batch_fetched <- 0
    queue$queue_size <- max_concurr*queue_scl
    queue$urls<-c()
    queue$batch_count <-  DBI::dbGetQuery(
      fetch_db,paste("select count(distinct batch) batch_count from fetch_list"))$batch_count
    queue$fetch_list<-list()
    
    
    ## initialize a batch of urls
    queue_batch(fetch_db,batch=1)

    ## Start timer and run queue
    queue$init_time <- Sys.time()
    queue$status$pending<-NULL
    queue$status<-curl::multi_run(pool = pool, timeout = timeout)
     

  },
  error = function(e){
    writeLines(paste('fetchR:',e), con=log_con)
    e<-paste('fetchR:',e)
    class(e) <- 'error'
    return(e)

  },
  finally = {

    rm(list=ls(queue),envir=queue)

    close(json_out)

    writeLines(paste('fetchR: Done Saving Page Data.'),con = log_con)

    if(class(log_con)[1]=="file") close(log_con)
    return(list(fetched_pg=as.list(fetched_pg)))
  })
}


