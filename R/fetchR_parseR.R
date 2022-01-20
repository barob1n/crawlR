#' Fetch a List of Url's.
#'
#' Based on the curl package (a wrapper for libcurl). The fetch list
#' of urls is organized into batches, with each batch containing one
#' url from one host. Provides a convienent way to avoid
#' hitting a server too often.  A delay also kicks in if a host is
#' being queried too quickly.
#'
#' @param out_dir (Required) Current output directory.
#' @param work_dir (Required) Current working directory.
#' @param fetch_list (Required) Created by generateR.R.
#' @param crawl_delay time (in seconds) for calls to the same host.
#' @param max_concurr Max. total concurrent connections open at any given time.
#' @param max_host Max. total concurrent connections per host at any given time.
#' @param timeout Total (all requests) timeout
#' @param timeout_request per request timeout
#' @param queue_scl Scaler
#' @param comments Some comments to print while running.
#' @param log_file Name of log file. If null, writes to stdout().
#' @param readability_content T
#' @param parser parse func
#' @return None.
#' @export
#'

fetchR_parseR<- function(
   out_dir=NULL,
   work_dir=NULL,
   fetch_list=NULL,
   crawl_delay=NULL,
   max_concurr=NULL,
   max_host=NULL,
   timeout=Inf,
   timeout_request=NULL,
   queue_scl=1,
   comments="",
   log_file=NULL,
   readability_content=F,
   parser=crawlR::parse_content_fetch){

  log_con<-set_log_file(log_file)
  
  tryCatch({

    fetch_list_env    <- new.env()

    # Fetched page data stored here.
    fetched_pg    <- new.env();

    json_out <- gzfile(paste0(out_dir,'fetched.json.gz'),open="a")

    if(readability_content){
      readability <- import("readability") # pip install readability-lxml
    }else{
      readability<-NULL
    }

    #if(!return & !save_to_disk){stop('return==F and save_to_disk==F - this will not return or save any data.')}
    if(is.null(out_dir)){stop(' no output directory provided.')}

    tika_ext <- rtika:::tika_mimetype

    ## writer for a batch of pages
    write_batch<-function(fetched_pg=NULL){
        if(is.null(fetched_pg)) return()
        for(i in (names(fetched_pg))){
          res<-fetched_pg[[paste(i)]]
          type <- strsplit(strsplit(res$type,';')[[1]] ,split='/')[[1]]
          if(is.na(type[1])) type<-c("","")
          if(type[1]=='text'){
            #res$content<-parse_content_fetch(res,readability,readability_content,map_meta)
            res$content<-parser(res,readability,readability_content)
          }
          writeLines(jsonlite::toJSON(list(ename=res)), con=json_out)
          fetched_pg[[paste(i)]]<-NULL
          rm(list=paste(i),envir=fetched_pg)
        }
      }


    print_cnt  <-0
    tot_cnt    <-0

    add_links_or_wait <- function( out_dir,json_out ){
      print_cnt<<- print_cnt + 1
      tot_cnt  <<- tot_cnt+1
      tot_left <- queue$links_in_batch - queue$links_in_queue
      pending  <- length(curl::multi_list(pool))

      ## Do not add links until necessary
      if((tot_cnt %% 500)==0){
        tot_left <- queue$links_in_batch -  print_cnt
        thisDt   <- as.numeric(as.numeric(Sys.time())- as.numeric(print_time))

        writeLines(
          paste('\nBatch Num:', queue$batch, 'of', queue$batch_count,
                '\nLinks Left in Batch:', tot_left,
                '\nNum Done in Batch: ', print_cnt,
                '\nTot Num Done: ',tot_cnt,
                '\nPending in pool: ', length(curl::multi_list(pool)),
                '\nRun Time for Batch: ', round(thisDt), 'seconds',
                '\nTotal Num Links: ', queue$tot_urls,
                '\nRate: ', round((print_cnt)/thisDt,1), 'Pages/Sec.',
                '\nComments:', comments,
                '\nDate/Time:', Sys.time(),
                '\n\n'),
         con = log_con)
      }

      ## adding links is expensive - only add when necessary
      if(pending > max_concurr) return()

      ## grab entire new batch when true
      new_batch<-F

      ## Load a new batch once current batch is done.
      if(queue$links_in_queue == queue$links_in_batch) {
        print_cnt <<-0
        new_batch <-T

        ## no more batches to do - write results and exit
        if(queue$batch==queue$batch_count){
          write_batch(fetched_pg)
          return()
        }

        load_batch((queue$batch + 1), queue,fetch_list_env$df)

        ## compute possible delay - delay if necessary
        dt <- as.numeric(Sys.time()) - as.numeric(queue$init_time)
        if(queue$delay[1] > dt){
          writeLines(paste('fetchR: Waiting for Batch: ', queue$batch,'- URLs Pending: ', pending ), con = log_con)

          Sys.sleep(queue$delay[1]  - dt)
        }
      }

      ## Determin number of links to add and then add them to pool.
      curr_cnt <- queue$links_in_queue
      curr_rem <- queue$links_in_batch - curr_cnt

      ## Grab new links
      getThisMany <- max(max_concurr*(queue_scl*2) - length(curl::multi_list(pool)),1)
      getThisMany <- min(curr_rem, getThisMany)
      getThese <- (curr_cnt+1):(curr_cnt+getThisMany)

      for(i in getThese){
        queue$links_in_queue<- queue$links_in_queue+1
        do_fetch(url=queue$urls[i],queue$depth[i], out_dir,json_out)
      }

      ## write out batch
      write_batch(fetched_pg)

      ## check if new batch is needed
      if(new_batch){
        print_time <<- Sys.time()
        new_batch <- F
      }
    }

    ave_rate=3

    ## Main Fetching function - based on crawl example in curl package.
    do_fetch <- function(url,depth,out_dir, json_out){
      out_dir <- out_dir
      h <- curl::new_handle(CONNECTTIMEOUT = 10); #verbose = TRUE
      curl::handle_setheaders(h,"Accept-Language" = "en;q=0.7")
      curl::handle_setheaders(h,"Connection" = "close")
      curl::handle_setheaders(h,"CURLOPT_DNS_CACHE_TIMEOUT" = paste(3600))
      curl::handle_setheaders(h,"User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.117 Safari/537.36")
      curl::handle_setopt(h, .list=list(timeout=round(timeout_request)))
      curl::curl_fetch_multi(url, handle = h, pool = pool, done = function(res){
        if(res$status >=200 & res$status<300){

          ## URL's are not usually valid filenames-hashes of them are.
          tryCatch({
            res$hash_name <- paste0('crawlR_', openssl::md5(res$url), collapse='')
            res$headers <- rawToChar(res$headers)
            type <- strsplit(strsplit(res$type,';')[[1]] ,split='/')[[1]]
            res$url<-url
            res$depth<-depth
            ## check for valid type
            if(is.na(type[1])) type<-c("","")
            if(type[1]=='text'){
              res$content<-rawToChar(res$content)
              if(res$url!="") fetched_pg[[res$url]] <-res
            }else if(type[2]=='pdf'){
              # fname <- paste0(out_dir, res$hash_name, ".pdf", collapse = "")
              # fh <- file(fname, open = "wb")
              # writeBin((res$content), con = fh)
              # close(fh)
              # res$content<-fname
              # fetched_pg[[res$url]] <-res
            }
          },error = function(e) {
            writeLines(paste('fetchR:', url,'-',substr(e,1,50)),con = log_con)
          })

        }else{
          res$content <- NA
        }
        add_links_or_wait(out_dir=out_dir,json_out)
      },fail = function(errmsg){
        writeLines(paste('fetchR:',errmsg),con = log_con)
        add_links_or_wait(out_dir=out_dir,json_out)
      })
    }

    # The pool of links
    pool  <- curl::new_pool(total_con = max_concurr, host_con = max_host)


    # list of links to be fetched - created by inject/generate
    if(!grepl('/$',out_dir)) out_dir<-paste0(out_dir,'/')
    if(is.null(fetch_list)) load(paste0(out_dir,'fetch_list.rda'))

    # use max_host to group in batches if max_host >1
    fetch_list$batch<-floor(fetch_list$batch/(max_host+0.1))+1

    fetch_list_env$df <- fetch_list
    rm(fetch_list);

    ## the queue....
    queue <- new.env()
    queue$batch_count <- max(fetch_list_env$df$batch)
    queue$init_time <- Sys.time()


    ## URL's loaded into queue.
    load_batch(batch=1,queue,fetch_list_env$df)

    ## Push initial urls into pool
    lapply(1:min(max_concurr*queue_scl, length(queue$urls)), function(i){
      do_fetch(queue$urls[i],queue$depth[i],out_dir,json_out)
      queue$links_in_queue <- queue$links_in_queue+1
    })

    ## Start timer and run queue
    print_time <- Sys.time()
    queue$init_time <- Sys.time()
    out <- curl::multi_run(pool = pool, timeout = timeout)

  },
  error = function(e){
    writeLines(paste('fetchR:',e), con=log_con)
    e<-paste('fetchR:',e)
    class(e) <- 'error'
    return(e)

  },
  finally = {

    rm(list=ls(fetch_list_env),envir=fetch_list_env)
    rm(list=ls(queue),envir=queue)


    close(json_out)


    writeLines(paste('fetchR: Saving Page Data.'), con = log_con)
    writeLines(paste('fetchR: Done Saving Page Data.'),con = log_con)

    if(class(log_con)[1]=="file") close(log_con)
    return(list(fetched_pg=as.list(fetched_pg)))
  })
}


