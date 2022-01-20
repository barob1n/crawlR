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
#' @param timeout Timeout time
#' @param queue_scl Scaler
#' @param comments Some comments to print while running.
#' @param save_to_disk Save output to disk or not.
#' @param return Return output or not.
#' @param log_file Name of log file. If null, writes to stdout().
#' @return None.
#' @export
#'

fetchR<- function(
   out_dir=NULL,
   work_dir=NULL,
   fetch_list=NULL,
   crawl_delay=NULL,
   max_concurr=NULL,
   max_host=NULL,
   timeout=Inf,
   queue_scl=1,
   comments="",
   save_to_disk=T,
   return=F,
   log_file=NULL){

  tryCatch({

    log_con<-set_log_file(log_file)

    if(!return & !save_to_disk){stop('return==F and save_to_disk==F - this will not return or save any data.')}
    if(is.null(out_dir) & save_to_disk){stop('save_to_disk==TRUE, but no output directory provided.')}

    tika_ext <- rtika:::tika_mimetype

    print_cnt  <-0
    tot_cnt    <-0
    write_count<-0
    # out_plot <- (data.frame(Pages_Per_Second = numeric(),
    #                        Links_Fetched = numeric(),
    #                        Total_Links = numeric(),
    #                        Time=character(),stringsAsFactors = F))

    # Tosses new links into the pool once the links pending drops below max_concurr.
    add_links_or_wait <- function( out_dir,json_out ){

      # pending <- length(curl::multi_list(pool))
      # remain <- queue$links_in_batch - queue$links_in_queue

      print_cnt<<- print_cnt + 1
      tot_cnt  <<- tot_cnt+1
      tot_left <- queue$links_in_batch - queue$links_in_queue
      pending  <- queue$links_in_queue - print_cnt
      pending  <- length(curl::multi_list(pool))

      ## Do not add links until necessary
      if( (tot_cnt %% 1000)==0){
        tot_left <- queue$links_in_batch -  print_cnt
        thisDt   <- as.numeric(as.numeric(Sys.time())- as.numeric(print_time))

        writeLines(paste('\nBatch Num:', queue$batch, 'of', queue$batch_count,
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
        # out_plot <- rbind(out_plot,
        #                   (data.frame(Pages_Per_Second = round((print_cnt)/thisDt,1),
        #                               Links_Fetched = tot_cnt,
        #                               Total_Links = queue$tot_urls,
        #                               Time=paste(Sys.time()),stringsAsFactors = F)))
        #
        # tmp<-htmlwidgets::saveWidget( DT::datatable(out_plot, options = list(pageLength = 100)),file='crawl_status')
        # DT::datatable(out_plot, options = list(pageLength = 100))
      }

      if(pending > max_concurr) return()


      new_batch<-F

      ## Load a new batch once current batch is done.
      if(queue$links_in_queue == queue$links_in_batch) {
        print_cnt<<-0
        new_batch<-T


        if(queue$batch==queue$batch_count) return() ## no batched left

        load_batch((queue$batch + 1), queue,fetch_list_env$df)
        dt <- as.numeric(Sys.time()) - as.numeric(queue$init_time)


        if(queue$delay[1] > dt){
          writeLines(paste('fetchR: Waiting for Batch: ', queue$batch,'- URLs Pending: ', pending ), con = log_con)
          Sys.sleep(queue$delay[1]  - dt)
        }
      }

      ## Determin number of links to add and then add them to pool.
      curr_cnt <- queue$links_in_queue
      curr_rem <- queue$links_in_batch - curr_cnt

      getThisMany <- max(max_concurr*(queue_scl*2) - length(curl::multi_list(pool)),1)
      getThisMany <- min(curr_rem, getThisMany)
      getThese <- (curr_cnt+1):(curr_cnt+getThisMany)

      for(i in getThese){
        queue$links_in_queue<- queue$links_in_queue+1
        do_fetch(url=queue$urls[i], out_dir,json_out)
      }
      gc()
      if(new_batch){
        print_time <<- Sys.time()
        new_batch <- F
      }
    }

    ## Main Fetching function - based on crawl example in curl package.
    do_fetch <- function(url,out_dir, json_out){
      out_dir <- out_dir
      h <- curl::new_handle(); #verbose = TRUE
      curl::handle_setheaders(h,"Accept-Language" = "en;q=0.7")
      curl::handle_setheaders(h,"Connection" = "close")
      curl::handle_setopt(h, .list=list(timeout=round(max_concurr*(1+queue_scl)/5)))
      curl::curl_fetch_multi(url, handle = h, pool = pool, done = function(res){
        if(res$status >=200 & res$status<300){
          if(save_to_disk){
            ## URL's are not usually valid filenames-hashes of them are.
            tryCatch({

              res$hash_name <- paste0('crawlR_', openssl::md5(res$url), collapse='')
              res$headers <- rawToChar(res$headers)
              type <- strsplit(strsplit(res$type,';')[[1]] ,split='/')[[1]]
              res$url<-url
              ## check for valid type
              if(is.na(type[1])) type<-c("","")

              if(type[1]=='text'){

                #res$content<-iconv(rawToChar(res$content), "latin1", "ASCII", sub="")

                #Doing parse step in callback
                res$content <- rawToChar(res$content)
                res$links   <- get_links(res)
                res$content <- crawlR::parse_content(res$content)
                res$content <- stringr::str_replace(res$content,stringr::fixed('"'),'\"')
                res$content <- stringr::str_replace(res$content,stringr::fixed("'"),"\'")
                base::writeLines(jsonlite::toJSON(res), con = json_out)

              }else if(type[2]=='pdf'){
                res$content   <- writeR(out_dir=out_dir,res=res,compress=T, extLookUp=tika_ext)
                writeLines(jsonlite::toJSON(list(ename=res)), con=json_out)
              }
            },error = function(e){
              writeLines(paste('fetchR:', url,'-',substr(e,1,50)),con = log_con)
            })
          }
          #fetched_pg[[url]]<-res
        }else{
          res$content <- NA
          #fetched_pg[[url]] <- res
        }
        #rm(list=url, envir=h_list)
        #rm(h,res,type)
        add_links_or_wait(out_dir=out_dir,json_out)
      },fail = function(errmsg){
        writeLines(paste('fetchR:',errmsg),con = log_con)
        #rm(h,res)
        add_links_or_wait(out_dir=out_dir,json_out)
      })
    }

    # The pool of links
    pool  <- curl::new_pool(total_con = max_concurr, host_con = max_host)

    # Fetched page data stored here.
    fetched_pg    <- new.env(); #listenv::lisenv()
    fetched_links <- new.env();

    # list of links to be fetched - created by inject/generate
    if(is.null(fetch_list)) load(paste0(out_dir,'/fetch_list.rda'))
    fetch_list_env    <- new.env()
    fetch_list_env$df <- fetch_list
    rm(fetch_list);

    fetched_list<- new.env();

    ## the queue....
    queue <- new.env()
    queue$batch_count <- max(fetch_list_env$df$batch)
    queue$init_time <- Sys.time()

    ## output to single file append
    file.create(paste0(out_dir,'/fetched.json.gz'))
    json_out <- gzfile(paste0(out_dir,'/fetched.json.gz'),open="a")
    #json_out <- NULL

    ## URL's loaded into queue.
    load_batch(batch=1,queue,fetch_list_env$df)

    ## Push initial urls into pool
    lapply(1:min(max_concurr*queue_scl, length(queue$urls)), function(i){
      do_fetch(queue$urls[i],out_dir,json_out)
      queue$links_in_queue <- queue$links_in_queue+1
    })

    ## Start timer and run queue
    print_time <- Sys.time()
    queue$init_time <- Sys.time()
    out <- curl::multi_run(pool = pool, timeout = timeout)

    fetched_pg   <-as.list(fetched_pg)
    fetched_links<-as.list(fetched_links)


  }, error = function(e){
    writeLines(paste('fetchR:',e), con=log_con)
    e<-paste('fetchR:',e)
    class(e) <- 'error'
    return(e)

  }, finally = {

    close(json_out)

    if(save_to_disk){
      writeLines(paste('fetchR: Saving Page Data.'), con = log_con)
      save(fetched_pg, file=paste0(out_dir,'fetched_pg.rda'))
      writeLines(paste('fetchR: Done Saving Page Data.'),con = log_con)
    }
    if(class(log_con)[1]=="file") close(log_con)
    if(return)  return(list(fetched_pg=as.list(fetched_pg),fetched_links=as.list(fetched_links)))
  })

}


