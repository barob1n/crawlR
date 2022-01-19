#' Log Out
#'
#' @description
#' Sets the out file for logs.
#'
#' @param log_file If null, defaults to stdout().
#' @export
#'
set_log_file <- function(log_file){
  if(is.null(log_file)){
    log_con <- stdout()
  }else{
    log_con <- file(log_file,open="a")
    #log_con <- file(paste0(work_dir,log_file),open="a")
  }
  return(log_con)
}

#' Score urls
#'
#' @description
#' Function to score URL's
#'
#' @param paths url paths to score
#' @param terms terms to use in scoring
#' @param scores values associated with terms
#' @export
#'
score_urls <- function(paths=NULL,
                       terms=c(),
                       scores=c(rep(1,  6),
                                rep(1, 16),
                                rep(1, 5))){

  if(is.null(paths)) return(0)
  if(NROW(terms)==0) return(0)
  these_matches<-lapply(paths,function(this_path){
    these_terms<-unlist(lapply(terms,function(this_term){
      grepl(this_term, this_path,ignore.case=T)
    }))
    sum(scores[these_terms])+1
  })
  return(unlist(these_matches))
}


#' Write to log
#'
#' @description
#' Writes to log file.
#'
#' @param log_file If null, defaults to stdout().
#' @param message  If null, defaults to stdout().
#' @export
#'
write_log <- function(message, log_file){
  log_con <- set_log_file(log_file)
  writeLines(message, con=log_con)
  if(class(log_con)[1]=="file") close(log_con)
}



#' Extract Links Found on Webpage.
#'
#' @description
#' Extracts links from html.
#'
#' @param res Return value from curl.
#' @return Returns character vector of links.
#' @export
#'
get_links <- function(res){
  tryCatch({
    doc <- xml2::read_html(res$content)
    nodes <- xml2::xml_find_all(doc, "//a[@href]")
    links <- xml2::xml_attr(nodes, "href")
    links <- xml2:::url_absolute(links, res$url)
    links <- grep("^https://|^http://", links, value = TRUE)
    links <- sub("#.*", "", links)
    links
  }, error = function(e){
    return()
  })
}


#' Get file extension from Content-Type
#'
#' @description
#' Uses lookup provided to get file extension.
#' content-type: text/html would return ".html" file extension, and
#' content-type: application/pdf would return ".pdf".
#'
#' @param type content-type of crawled page.
#' @param extLookUp file extension lookup.
#' @return extention as string
#' @export
#'
parseExt <- function(type, extLookUp){
  ext <- extLookUp$file_extension[match(type, extLookUp$type)]
  if(length(ext)>0){
    thisType <- ext
  }else{
    thisType <- '.NA'
  }
  return(thisType)
}


#' Convert String to hash
#'
#' @description
#' uses openssl::md5 to convert url's to md5-hashes.
#'
#' @param url Url of crawled page.
#' @return Returns character vector of links, or error message.
#' @export
#'
makeHash = function(url){
  return(paste0('crawlR_', openssl::md5(url), collapse=''))
}


#' Base Output Writer (depricated)
#'
#' Write web page to disk. Return file name of output.
#'
#' @param out_dir Directory where output is written.
#' @param res Crawled page data.
#' @param compress If true, text/html is compressed.
#' @param extLookUp lookup file extension
#' @return Returns filename of output.
#'
#'
writeR <- function(out_dir=NULL,res=NULL,compress=T,extLookUp=NULL){
  stopifnot(!is.null(out_dir)|!is.null(res$type))

  ## grepping text|html is too slow
  type1<-strsplit(res$type,';')[[1]][1]
  type2<-strsplit(type1,'/')[[1]][1]

  if(is.na(type1)|is.null(type1)|type1=="")return()

  ## URL's are not usually valid filenames-hashes of them are.
  # fname <-paste0(out_dir,res$hash_name,parseExt(type1,extLookUp), collapse='')

  if(type2 == 'text' & compress){
    fname <-paste0(out_dir,res$hash_name,".html", collapse='')
    fname <- paste0(fname,".gz")
    fh <- suppressWarnings(gzfile(fname, "wb"))
  }else if(type2 == 'text'){
    fname <-paste0(out_dir,res$hash_name,".html", collapse='')
    fh <- file(fname, "wb")
  }else{
    fname <-paste0(out_dir,res$hash_name,".pdf", collapse='')
    fh  <- file(fname,open="wb")
  }
  didWrite <- try({writeBin((res$content), con = fh)})
  close(fh)

  if(class(didWrite)!='try-error') return(fname)

  return(NA)
}


#' Normalize Url's
#'
#' @description
#' Converts url's to lowercase, removes any trailing back slashes,
#' appends 'http://' to front if missing, and removes everything
#' following #-marks.
#'
#' @param url url to normalize
#' @return URLS beginning with (http/https), followed by ://www..
#' @export
#'
normalize_url <- function(url){

  url <- tolower(url)
  url <- gsub("/$","",url)
  url <- gsub('#.*','',url)
  url <- ifelse(!grepl('^https?:',url),paste0('http://',url),url)
  url<-na.omit(url)
  return(url)

}

#' Creates a Fetch List
#'
#' @description
#' Creates/formats fetch list from links queried during the generateR function.
#' Output fetch list is used by the fetchR functions.
#'
#'
#' @param fetch_list linkDB
#' @param crawl_delay delay between successive requests to server.
#' @return fetchable list
#' @export
#'
create_fetch_list <- function(fetch_list,crawl_delay=30){

  fetch_list<-split(fetch_list,fetch_list$server)
  fetch_list<-lapply(fetch_list,function(x) {
    x$batch<-1:NROW(x);
    x$crawl_delay<-crawl_delay
    x#[,c('server','url','batch','crawl_delay','depth','crawl_int')]
    })


  fetch_list<-do.call(rbind,fetch_list)
  if(NROW(fetch_list)==0) return(NULL)

  fetch_list$tot_delay<-fetch_list$batch*fetch_list$crawl_delay
  fetch_list <- fetch_list[order(fetch_list$tot_delay),]
  fetch_list$batch <- match(fetch_list$tot_delay,unique(fetch_list$tot_delay))
  return(fetch_list)
}



#' Queue a Batch of URL's
#'
#'
#' @param batch Batch Number to Load. Should start at 1.
#' @param queue Batch is loaeded into queue.
#' @param fetch_list Stores url's and their batch number.
#' @return fetchable list
#' @export
#'
load_batch <- function(batch,queue,fetch_list){
  queue$urls  <- unlist(fetch_list$url[fetch_list$batch == batch])
  queue$delay <- unlist(fetch_list$tot_delay[fetch_list$batch == batch])
  queue$depth <- unlist(fetch_list$depth[fetch_list$batch == batch])
  queue$batch <- batch
  queue$links_in_batch <- length(queue$urls)
  queue$links_in_queue <-0
  queue$tot_urls <- NROW(fetch_list)
}



#' Get Last Directory
#'
#'
#' @param this this directory
#' @param out_dir workind dir
#' @return URLS beginning with (http/https), followed by ://www..
#' @export
#'
find_last_dir <- function(this = "fetch_", out_dir){
  last_dir<-file.info(list.dirs(out_dir),extra_cols = F)
  last_dir<-last_dir[order(last_dir$ctime,decreasing = T), ]
  last_dir<-rownames(last_dir[last_dir$isdir & grepl(this, rownames(last_dir)),])
  return((last_dir[1]))
}




#' General Parser
#'
#' Extracts the title, headers, span, and p tags from a page.
#'
#' @param res Return value from curl.
#' @param readability readability reticulate object.
#' @param readability_content T of F - if true use readability.
#' @param map_meta meta data header mappings.
#' @return Returns character vector of links, or error message.
#' @export
#'

parse_content_fetch <- function(res, readability, readability_content=F, map_meta=NULL){
# url<-"https://www.automotivelogistics.media/digital-technology/bmw-dingolfing-developing-industry-40-technology-for-logistics/40180.article"
# url<- "https://www.zamilsteel.com/ssd/en/2017-06-07-zamil-structural-steel-wins-sar-34m-contract-ethane-deep-recovery-facility-project"
# res<-curl::curl_fetch_memory(url)
# res$content<-rawToChar(res$content)

these_meta <- c('content-language','description','keywords','twitter:card',
                'twitter:title','twitter:site','twitter:site:id','twitter:description',
                'og:locale','og:type','og:title','og:url','og:name','og:description',
                'article:modified_time','article:section','article:modified_time')

grep_meta<-'content-language|description|keywords|twitter:card|twitter:title|twitter:site|twitter:site:id|twitter:description|og:locale|og:type|og:title|og:url|og:name|og:description'

  tryCatch({

    doc <- xml2::read_html(res$content)
    vals <- list()
    vals[['meta']]<-list()



    ## process meta data
    meta_doc<- doc %>% rvest::html_nodes('meta')
    meta_content<-rvest::html_attr(meta_doc, "content")
    meta_name<-rvest::html_attr(meta_doc, "name")

    ## looking for any dates
    #idx<-stringr::str_detect(meta_prop, 'date') & !is.na(meta_prop)
    #vals[['meta']][['meta_dates']]<- meta_content[idx]

    ## looking for any dates
    idx<-stringr::str_detect(meta_name, 'date') & !is.na(meta_name)
    vals[['meta']][['meta_dates']]<- c(meta_content[idx],vals[['meta']][['meta_dates']])

    ## grab all meta
    idx<-which(!is.na(meta_name))
    vals[['meta']][meta_name[idx]]<-meta_content[idx]
    idx<-which(!is.na(meta_prop))
    vals[['meta']][meta_prop[idx]]<-meta_content[idx]

    ## general date elements
    vals[['span']]     <- doc %>% rvest::html_nodes('span')# %>% rvest::html_text()
    pub_i <-vals[['span']] %>% rvest::html_attr(name='class') %>% stringr::str_which(pattern='date')
    vals[['dates']]<- vals[['span']][pub_i] %>% rvest::html_text()
    if(NROW(vals[['dates']])==0){
      vals[['dates']]<- doc %>% rvest::html_nodes('p')
      pub_i <-vals[['dates']] %>% rvest::html_attr(name='class') %>% stringr::str_which(pattern='date')
      vals[['dates']]<- vals[['dates']][pub_i] %>% rvest::html_text()
    }
    vals[['span']]<-vals[['span']] %>% rvest::html_text()

    if(readability_content){
      read_doc<-(readability$Document(res$content))
      vals[['title']]   <- read_doc$title()

      ## grab these outside of readability since often ommited
      vals[['time']]     <- doc %>% rvest::html_nodes('time') %>% rvest::html_text()


      ## readability grabs main article
      vals[['content']]  <- read_doc$summary()%>% rvest::html()
      vals[['h1']]       <- vals[['content']] %>% rvest::html_nodes('h1') %>% rvest::html_text()
      vals[['content']]  <- vals[['content']] %>% rvest::html_text()
    }else{
      vals[['content']]  <- doc %>% rvest::html_nodes('p')  %>% rvest::html_text()
      vals[['title']]    <- paste(rvest::html_nodes(doc, "title") %>% rvest::html_text(),collapse= ' ')
      vals[['span']]     <- doc %>% rvest::html_nodes('span')  %>% rvest::html_text()
      vals[['h1']]       <- doc %>% rvest::html_nodes('h1')  %>% rvest::html_text()
      vals[['time']]     <- doc %>% rvest::html_nodes('time')%>% rvest::html_text()
    }

    vals$links <- doc %>% rvest::html_nodes('a') %>% rvest::html_attr('href')
    vals$links <- xml2:::url_absolute(vals$link ,res$url)
    vals$links <- grep("^https://|^http://", vals$links, value = TRUE)
    vals$links <- sub("#.*", "", vals$links)
    #   }
    # })
  return(vals)

  }, error = function(e){return(NA);print(e)} )

}

#' General Parser for HTML
#'
#' @description
#' Extracts the title, h1 headers, major meta-tags, and body tags from a page.
#'
#' @param res Return value from curl.
#' @param readability not used
#' @param readability_content not used
#' @param map_meta not used
#' @return Returns character vector of links, or error message.
#' @export
#'

parse_content <- function(res, readability, readability_content=F, map_meta=NULL){
  # url<-"https://www.automotivelogistics.media/digital-technology/bmw-dingolfing-developing-industry-40-technology-for-logistics/40180.article"
  # url<- "https://www.zamilsteel.com/ssd/en/2017-06-07-zamil-structural-steel-wins-sar-34m-contract-ethane-deep-recovery-facility-project"
  # url<-"https://www.cnn.com"
  # res<-curl::curl_fetch_memory(url)
  # res$content<-rawToChar(res$content)
  filter_tags <- function(tag){
    tag_docs <- rvest::html_nodes(doc, tag ) %>% rvest::html_text()
    return(tag_docs)
  }

  map_meta <- list()
  map_meta[['content-language']]= 'meta[http-equiv=content-language]'
  map_meta[['description']]='meta[name=description]'
  map_meta[['keywords']]='meta[name=keywords]'
  map_meta[['twitter:card']]='meta[property=twitter\\:card]'
  map_meta[['twitter:title']]='meta[property=twitter\\:title]'
  map_meta[['twitter:site']]='meta[property=twitter\\:site]'
  map_meta[['twitter:site:id']]='meta[property=twitter\\:site\\:id]'
  map_meta[['og:locale']]='meta[property=og\\:locale]'
  map_meta[['og:type']]='meta[property=og\\:type]'
  map_meta[['og:title']]='meta[property=og\\:title]'
  map_meta[['og:url']]='meta[property=og\\:url]'
  map_meta[['og:name']]='meta[property=og\\:name]'

  parse_meta <- function(doc, map_meta=map_meta){
    meta<-lapply(map_meta, function(m){
      doc %>% rvest::html_nodes(m) %>% rvest::html_attr('content')
    })

    return(meta=meta)
  }

  tryCatch({
    doc <- rvest::html(res$content)

    vals <- list()
    vals[['title']]   <- paste(rvest::html_nodes(doc, "title") %>% rvest::html_text(),collapse= ' ')
    vals[['content']] <- filter_tags("body")
    vals[['h1']] <- filter_tags("h1")
    vals$meta <- parse_meta(doc,map_meta=map_meta)
    vals$links <- doc %>% rvest::html_nodes('a') %>% rvest::html_attr('href')
    vals$links <- xml2:::url_absolute(vals$link ,res$url)
    vals$links <- grep("^https://|^http://", vals$links, value = TRUE)
    vals$links <- sub("#.*", "", vals$links)
    return(vals)

  }, error = function(e){return(NA)} )
}



#' Parse Processor
#'
#' @description
#' Handles parseing of extracted links and page content.  Page content
#' is parsed using 'parser' function passed in as variable.
#'
#' @param this_dir Fetch directory to parse.
#' @param parser Function that will be used for parseing.
#' @param log_file Log file name. If null, defaults to stdout().
#' @return URLS beginning with (http/https), followed by ://www..
#' @export
#'

parseR <- function(this_dir=NULL,
                     parser=parse_content,
                     log_file = NULL){

  process_links<-function(x){
    if(is.null((x))){return()}
    if(is.null((x$links))){return()}
    if(NROW((x$links))==0){return()}
    if(is.null(x$port)) x$port<-""
    x$port[is.na(x$port)]<-''
    this_depth<-0
    if(!is.null(x$depth)) this_depth<-x$depth
    links<-xml2::url_parse(unlist(x$links))
    links$url <- paste0(links$scheme,"://",links$server,links$path)
    links$url <- ifelse(links$query !="", paste0(links$url,'?',links$query),links$url)
    links$url <- normalize_url(links$url)
    links$crawled<-0
    links$is_seed<-0
    links$next_crawl<-as.numeric(Sys.Date())
    links$depth<-this_depth + 1
    links$crawl_int<-1
    return(links)
  }

  ## set logging
  log_con<-crawlR:::set_log_file(log_file)

  ## big try catch for passing back to crawlR
  tryCatch({

    ## file to place links
    fh_links<-suppressWarnings(gzfile(paste0(this_dir,'fetched_links.json.gz'),open='a'))

    ## read crawled data
    chunk_con = suppressWarnings(gzfile(paste0(this_dir,'fetched.json.gz'),open='rb'))

    tot <- 0
    while ( TRUE ) {

      ## since file could be huge, rather than read the
      ## entire file into memory, it is read line by line
      f_chunk<-suppressWarnings(readLines(chunk_con,n=1))

      ## track number of lines read
      tot<-tot + NROW(f_chunk)

      ## break if no more data
      if (NROW(f_chunk) == 0 ){break;}

      ## le' counter
      if(tot %% 1000 == 0){writeLines(paste('parseR: Total Done:',tot), con = log_con)}

      ## sometimes parse fails
      pg <- tryCatch({jsonlite::fromJSON(f_chunk)$ename}, error = function(e) e )

      ## must parse, must have url, and must have content
      if(inherits(pg, "error")) next
      if(is.null(pg)) next
      if(sum(is.na(pg))==NROW(pg)) next
      if(is.null(pg$url) | is.null(pg$content)) next
      if(is.na(pg$url[1]) | is.na(pg$content[1]))next

      output<- tryCatch({
         ## headers same for any type
         pg$headers<-curl::parse_headers_list(pg$headers)

         ## if text/html
         if(grepl('text',tolower(pg$type))){
           if(length(pg$content)>0){
             processed_links<-process_links(list(url=pg$url,links=pg$content$links,depth=pg$depth))
             writeLines(jsonlite::toJSON(processed_links), con = fh_links )
           }
         }else if(grepl('pdf',tolower(pg$type))){
           next
         }else {
           next
         }
       },
       error = function(e){
         writeLines(paste('parseR: Recoverable Error',e), con=log_con)
       })

    }## End of while loop.

  },
  error = function(e){

    writeLines(paste('parseR:',e), con=log_con)
    e<-paste('parseR:',e)
    class(e) <- 'error'

  },
  finally = {

    close(chunk_con)
    close(fh_links)
    if(class(log_con)[1]=="file") close(log_con)

  })
}


#' Parse Processor
#'
#'
#' @param this_dir Fetch directory to parse.
#' @param parseR Function that will be used for parseing.
#' @param n_threads Number of threads to use for parseing.
#' @param log_file Log file name. If null, defaults to stdout().
#' @param n chunk size
#' @return URLS beginning with (http/https), followed by ://www..
#' @export
#'

parseR_old <- function(this_dir=NULL, parser=parse_content,n_threads=4, log_file = NULL,n=4000){

  log_con<-set_log_file(log_file)

  tryCatch({

    parse_dir<-paste0(this_dir,'parsed/')
    dir.create(parse_dir)
    fetched_links<-list()#new.env()
    promise_list<-list()

    chunk_con = gzfile(paste0(this_dir,'fetched.json.gz'),open="rb")
    tot <- 0
    while ( TRUE ) {

      f_chunk<-suppressWarnings(readLines(chunk_con,n=n))
      if (length(f_chunk) == 0 ) break

      tot<-tot + length(f_chunk)

      fetched_pg<-lapply(f_chunk, function(x){
        tryCatch({
          jsonlite::fromJSON(x)$ename
        }, error = function(e)  NA)

      })


      theseNames <- unlist(lapply(fetched_pg, function(x){
        if(is.null(x)) return('drop_me')
        if(is.na(x))   return('drop_me')
        if(is.null(x$url)) return('drop_me')
        x$url
      }))
      names(fetched_pg)<-theseNames
      chunk_size<-floor(length(names(fetched_pg))/n_threads)
      chunk<-list()
      for(i in 1:n_threads){
        chunk_names<-theseNames[(chunk_size*(i-1)+1):(chunk_size*(i-1)+chunk_size)]
        if(i==n_threads){
          chunk_names<-theseNames[(chunk_size*(i-1)+1):length(theseNames)]
        }
        chunk[[i]] <- fetched_pg[chunk_names]
      }

      for(i in 1:n_threads){
        chunk_links<-list()
        this_out <- paste0('parsed_sequential_',i,'.json')

        promise_list[[i]]<- future(
          globals=list(i=i,
                       chunk=chunk,
                       parse_dir=parse_dir,
                       chunk_links=chunk_links,
                       log_con=log_con,
                       get_links=get_links,
                       `%>%` = magrittr::`%>%`,
                       parser=parser,
                       this_out=this_out),{
                         con_out=base::file(base::paste0(parse_dir,this_out),open='a',blocking = F)
                         thisChunk <- chunk[[i]]

                         ## parseR should be called in for loop
                         for(pg in thisChunk){
                           #apply(thisChunk,function(pg){
                           links<-NA
                           # if(is.null(pg)){return()}
                           # if(is.na(pg)){return()}
                           # if(sum(names(pg)=='drop_me')>1){return()}
                           # if(is.null(pg$content)){return()}
                           # if(is.na(pg$content)) {return()}
                           # if(pg$content=='') {return()}

                           links<-NA
                           if(is.null(pg)){next}
                           if(is.na(pg)){next}
                           if(sum(names(pg)=='drop_me')>1){next}
                           #if(is.null(pg$content)){next}
                           #if(is.na(pg$content)) {next}
                           #if(pg$content=='') {next}

                           tryCatch({

                             pg$headers<-curl::parse_headers_list(pg$headers)
                             type1 <- gsub("^\\s+|\\s+$","",strsplit(pg$type,";")[[1]][1])
                             type2 <- gsub("^\\s+|\\s+$","",strsplit(type1,"/")[[1]][1])


                             if(type2=='text'){

                               #links<-get_links(pg)
                               #pg$content <- parser(pg$content)
                               base::writeLines(jsonlite::toJSON(pg), con = con_out)
                             }
                             if(type1=='application/pdf'| type2=='pdf'){
                               next
                               # fname <- pg$content
                               # info <- pdftools::pdf_info(fname)
                               # if(is.null(info)) next
                               # if(!is.null(info$keys)){
                               #   if(is.null(info$keys$Title)) info$keys$Title <- pg$url
                               # }
                               #
                               # pg$content <- list()
                               # pg$content$title <- info$keys$Title
                               # pg$modified <- info$modified
                               # pg$content$content <- pdftools::pdf_text(fname)
                               # base::writeLines(jsonlite::toJSON(pg), con = con_out)
                             }

                           }, error = function(e){
                             writeLines(paste('parseR: Recoverable Error',e), con=log_con)
                           })

                           chunk_links[[pg$url]]<-links
                         }
                         #})

                         base::close(con_out)
                         return(chunk_links)
                       }) %plan% multiprocess
      }

      for(i in 1:length(chunk)){
        chunked_links <- value(promise_list[[i]])
        for(j in names(chunked_links)){
          fetched_links[[j]]<-chunked_links[[j]]
        }
      }
      writeLines(paste('parseR: Total Done:',tot), con = log_con)
    }

    future:::ClusterRegistry("stop")

  }, error = function(e){
    writeLines(paste('parseR:',e), con=log_con)
    e<-paste('parseR:',e)
    class(e) <- 'error'
    e

  }, finally = {
    close(chunk_con)
    fetched_links<-as.list(fetched_links)
    save(fetched_links, file=paste0(this_dir,'fetched_links.rda'))
    if(class(log_con)[1]=="file") close(log_con)
  })


}





