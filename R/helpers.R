#' Log Out
#'
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
#' Sets the out file for logs.
#' @param paths url paths to score
#' @param terms terms to use in scoring
#' @param scores values associated with terms
#' @export
#'
score_urls <- function(paths=NULL,
                       terms=c( 'about','news','press','portfolio','products','announce',
                                'construct','plant','coal','nuclear','wind','refinery',
                                'pipelines','oil','gas','energy','manufacturing',
                                'chemical','processing','mining','ore','drilling',
                                'project','addition','expansion','grassroot','greenfield'),
                       scores=c(rep(1,  6),
                                rep(1, 16),
                                rep(1,  5))){

  if(is.null(paths)) return(0)
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
#' Sets the out file for logs.
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



#' Grab Links Found on Webpage.
#'
#' Parses HTML for URLs and stores then in the linkDB.
#'
#' @param res Return value from curl.
#' @return Returns character vector of links.
#' @export
#'
get_links <- function(res){
  tryCatch({
    #stopifnot(isTRUE(grepl("text/html", res$type)))
    doc <- xml2::read_html(res$content)
    nodes <- xml2::xml_find_all(doc, "//a[@href]")
    links <- xml2::xml_attr(nodes, "href")
    links <- xml2:::url_absolute(links, res$url)
    links <- grep("^https://|^http://", links, value = TRUE)
    links <- sub("#.*", "", links)
    #links <- sub("index.html$", "", links)
    links
  }, error = function(e){
    return()
  })
}


#' Get File Extension from Content-Type
#'
#' Uses lookup provided by rTika to get file extension.
#' content-type: text/html would return ".html" file extension, and
#' content-type: application/pdf would return ".pdf".
#'
#' @param type content-type of crawled page.
#' @return extention as string
#' @export
#'
parseExt <- function(type, extLookUp){
  ext <- extLookUp$file_extension[match(type, extLookUp$type)]
  #ext <- rtika:::tika_mimetype$file_extension[match(type, rtika:::tika_mimetype$type)]
  if(length(ext)>0){
    thisType <- ext
  }else{
    thisType <- '.NA'
  }
  return(thisType)
}


#' Hashes URL to Make Valide (and unique) File Names
#'
#' Used by file output.
#'
#' @param url Url of crawled page.
#' @param type content-type of crawled page.
#' @return Returns character vector of links, or error message.
#' @export
#'
makeHash = function(url){
  return(paste0('crawlR_', openssl::md5(url), collapse=''))
}


#' Base Output Writer
#'
#' Write web page to disk. Return file name of output.
#'
#' @param out_dir Directory where output is written.
#' @param type content-type of crawled page.
#' @param res Crawled page data.
#' @param compress If true, text/html is compressed.
#' @return Returns filename of output.
#' @export
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
    fh <- gzfile(fname, "wb")
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

#' Returns New URL
#'
#' Internal use by crawlR package.
#'
#' @param new_root Root to draw next url from.
#' @return Returns filename of output.
#'
get_url_from_root1 <- function(new_root,links,wait_table){
  #this_link <- links$url[[new_root]][1]
  this_link <- links[[new_root]][1]
  #links$url[[new_root]] <- links$url[[new_root]][-1]

  if(NROW(links[[new_root]])<=1){
    idx <- which(wait_table$url %in% new_root)
    wait_table$url<-wait_table$url[-idx]
    wait_table$dt<-wait_table$dt[-idx]
    wait_table$queue<-wait_table$queue[-idx]
    #links$url[[new_root]] <- NULL
    #rm(links[[new_root]])
    rm(list=new_root, envir=links)
  }else{
    links[[new_root]] <- links[[new_root]][-1]
  }


  return(this_link)
}

#' Returns the Root of a Url
#'
#' Internal use by crawlR package.
#'
#' @param urls List of urls.
#' @return Returns filename of output.
#'
get_root_from_url <- function(urls){
  #domain <- gsub("http://|https://|www\\.", "", urls)
  #domain <- gsub("(.*)(")
  return(xml2::url_parse(urls))
}
#
# library(robotstxt)
# paths_allowed("http://google.com/")
#
# robotstxt::parse_robotstxt()
# rtxt <- robotstxt(domain="wikipedia.org")



#' Filter Out Url's
#'
#' Filter out URL's
#'
#' @param links Links to be filtered.
#' @param urlRegExFilterOut RegEX to filter with.
#' @return filtered list of links
#' @export
#'
do_filt_out <- function(url,regExOut){
  if(is.null(regExOut)) return((rep(T,length(url))))
  return(!grepl(regExOut,url,ignore.case = T))
}

#' Normalize Url's
#'
#' Normalize url's
#'
#' @param url url to normalize
#' @return URLS beginning with (http/https), followed by ://www..
#' @export
#'
normalize_url <- function(url){
  tmp<-try({

    url <- tolower(url)
    # omits http://
    if(grepl('^www.',url)) url <- paste0('http://',url)
    # missing http & http...
    if(!grepl('^http://|^https://',url)){
      url<- paste0('http://',url)
    }
    url <- gsub("/$","",url)
    # anything after #
    url <- gsub('#.*','',url)
  },silent=T)
  if(class(tmp)=='try-error'){
    return(NA)
  }
  return(tmp)

}

#' Creates a Fetch List from linkDB File
#'
#'
#' @param linkDB linkDB
#' @return fetchable list
#' @export
#'
create_fetch_list <- function(fetch_list,crawl_delay){

  fetch_list<-split(fetch_list,fetch_list$server)
  fetch_list<-lapply(fetch_list,function(x) {
    x$batch<-1:NROW(x);
    x$crawl_delay<-crawl_delay
    x#[,c('server','url','batch','crawl_delay','depth','crawl_int')]
    })
  #cnt <- unlist(lapply(fetch_list, function(x) length(x)))



  fetch_list<-do.call(rbind,fetch_list)
  if(NROW(fetch_list)==0) return(NULL)

  # fetch_list<-data.table::data.table(server = rep(names(fetch_list), cnt),
  #                                    url = unlist(fetch_list),
  #                                    batch=unlist(lapply(cnt, function(x) seq(x))),
  #                                    crawl_delay=crawl_delay,
  #                                    depth=fetch_list$depth,
  #                                    stringsAsFactors = F)

  fetch_list$tot_delay<-fetch_list$batch*fetch_list$crawl_delay
  fetch_list <- fetch_list[order(fetch_list$tot_delay),]
  fetch_list$batch <- match(fetch_list$tot_delay,unique(fetch_list$tot_delay))
  return(fetch_list)
}


#' Processes Fetched Link List
#'
#'
#' @param fetched_links Links from fetch phase.
#' @return fetchable list
#' @export
#'
process_fetched_links <- function(fetched_links,fetch_list){
  if(NROW(fetched_links)==0) return(NULL)
  origin_url<-normalize_url(names(fetched_links))
  fetched_links<-lapply(origin_url,function(x){
    urls<-na.omit(normalize_url(fetched_links[[x]]))
    if(NROW(urls)==0)return()
    urls <- xml2::url_parse(unlist(urls))

    urls$url <- paste0(urls$scheme,"://",urls$server,urls$path)
    urls$url <- ifelse(urls$query !="", paste0(urls$url,'?',urls$query),urls$url)
    if(urls$query !="") urls$url<-paste0(urls$url,'?',urls$query)
    urls$url <- gsub('\\?$|/$','',urls$url)
    urls$crawled<-0
    urls$depth<-0
    urls$crawl_int <- fetch_list$crawl_delay[match(urls$server, fetch_list$server)]
    urls$crawl_int[is.na(urls$crawl_int)]<-sample(30:60,sum(is.na(urls$crawl_int)),replace=T)
    if(x %in% fetch_list$url){
      urls$depth<-fetch_list$depth[fetch_list$url==x]+1
    }
    urls
  })

  fetched_links<-do.call(rbind,fetched_links)
  # fetched_links <- na.omit(unlist(lapply(unlist(fetched_links),function(x) normalize_url(x))))
  #
  # fetched_links <- na.omit(unlist(lapply(unlist(fetched_links),function(x) normalize_url(x))))
  # #cnt <- unlist(lapply(fetched_links, function(x) length(x)))
  # if(NROW(fetched_links)==0) return(NULL)
  # fetched_links <- xml2::url_parse(unlist(fetched_links))
  #
  # fetched_links$url <- paste0(fetched_links$scheme,"://",fetched_links$server,fetched_links$path,'?',fetched_links$query)
  # fetched_links$url <- gsub('\\?$|/$','',fetched_links$url)
  fetched_links <- fetched_links[!duplicated(paste(fetched_links$server, fetched_links$path)), ]
  fetched_links <- fetched_links[grepl('http|https',paste(fetched_links$scheme)) & fetched_links$server != "",]
  return(fetched_links)
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
#
#     p <- profvis({
#       for(jj in 1:100){
#        print(jj)
    doc <- xml2::read_html(res$content)
    vals <- list()
    vals[['meta']]<-list()



    ## process meta data
    meta_doc<- doc %>% rvest::html_nodes('meta')
    meta_content<-rvest::html_attr(meta_doc, "content")
    meta_name<-rvest::html_attr(meta_doc, "name")
    meta_prop<-rvest::html_attr(meta_doc, "property")

    ## get meta data names
    #idx<-which(!is.na(meta_name) & meta_name %in% names(map_meta))
    #idx<-stringr::str_detect(meta_name, grep_meta) & !is.na(meta_name)
    #vals[['meta']][meta_name[idx]]<- meta_content[idx]

    ## get meta data properties
    #idx<-which(!is.na(meta_prop) & meta_prop %in% names(map_meta))
    #idx<-stringr::str_detect(meta_prop, grep_meta) & !is.na(meta_prop)
    #vals[['meta']][meta_prop[idx]]<- meta_content[idx]


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
      #vals[['span']]     <- doc %>% rvest::html_nodes('span') %>% rvest::html_text()
      vals[['time']]     <- doc %>% rvest::html_nodes('time') %>% rvest::html_text()
      ##vals[['p']]        <- doc %>% rvest::html_nodes('p')    %>% rvest::html_text()

      ## readability grabs main article
      vals[['content']]  <- read_doc$summary()%>% rvest::html()
      vals[['h1']]       <- vals[['content']] %>% rvest::html_nodes('h1') %>% rvest::html_text()
      ##vals[['h2']]       <- vals[['content']] %>% rvest::html_nodes('h2') %>% rvest::html_text()
      ##vals[['h3']]       <- vals[['content']] %>% rvest::html_nodes('h3') %>% rvest::html_text()
      ##vals[['h4']]       <- vals[['content']] %>% rvest::html_nodes('h4') %>% rvest::html_text()
      vals[['content']]  <- vals[['content']] %>% rvest::html_text()
    }else{
      vals[['content']]  <- doc %>% rvest::html_nodes('p')  %>% rvest::html_text()
      vals[['title']]    <- paste(rvest::html_nodes(doc, "title") %>% rvest::html_text(),collapse= ' ')
      vals[['span']]     <- doc %>% rvest::html_nodes('span')  %>% rvest::html_text()
      vals[['h1']]       <- doc %>% rvest::html_nodes('h1')  %>% rvest::html_text()
      ##vals[['h2']]       <- doc %>% rvest::html_nodes('h2')  %>% rvest::html_text()
      ##vals[['h3']]       <- doc %>% rvest::html_nodes('h3')  %>% rvest::html_text()
      ##vals[['h4']]       <- doc %>% rvest::html_nodes('h4')  %>% rvest::html_text()
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




#' General Parser
#'
#' Extracts the title, headers, span, and p tags from a page.
#'
#' @param res Return value from curl.
#' @return Returns character vector of links, or error message.
#' @export
#'

parse_content <- function(res){

  filter_tags <- function(tag){
    tag_docs <- rvest::html_nodes(doc, tag ) %>% rvest::html_text()
    #if(length(tag_docs)==0) return("")
    # tag_docs <- unlist(lapply(toString(tag_docs), function(x){
    #
    #   x <- enc2native(x)
    #   x <- gsub('<a.*?/a>','',x) %>% rvest::html() %>% rvest::html_nodes(tag) %>% rvest::html_text(trim=T)
    #   x <- x[x!=""]
    #   x
    #
    # }))
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
    doc <- rvest::html(res)
    vals <- list()
    vals[['title']]   <- paste(rvest::html_nodes(doc, "title") %>% rvest::html_text(),collapse= ' ')
    vals[['content']] <- filter_tags("p")
    vals[['span']] <- filter_tags("span")
    vals[['h1']] <- filter_tags("h1")
    vals[['h2']] <- filter_tags("h2")
    vals[['h3']] <- filter_tags("h3")
    vals[['h4']] <- filter_tags("h4")

    vals$meta <- parse_meta(doc,map_meta=map_meta)

    return(vals)

  }, error = function(e){return(NA)} )
}


#' Parse Contact
#'
#' Taken from crawl example given in the curl package.
#'
#' @param res Return value from curl.
#' @return Returns character vector of links, or error message.
#' @export
#'

parse_contact <- function(res){

  getThis <- function(page_list=NULL, this=NULL, innermost=T){
    if(length(page_list)==0) return(NA)
    idx<-unlist(lapply(page_list, function(x){
      grepl(this,x, ignore.case = T)
    }))

    if(!innermost) return(page_list[idx])
    idx2<-1:length(idx)
    idx <- max(idx2[idx])
    return(page_list[ idx] )
  }

  url<-'https://www.electranet.com.au/contact/'
  doc <-  rvest::html(url)
  if(length(doc)==0) return(NA)

  tryCatch({
    doc <- rvest::html(res)
    vals <- list()

    address <-NULL
    address <-doc %>% rvest::xml_nodes('address')  %>% rvest::html_text(trim=T)

    divs <-doc %>% rvest::xml_nodes('div')

    if(length(divs)==0) return(NA)
    contact <- getThis(divs, 'phone:|tel:address') %>% rvest::html_children() %>% rvest::html_text(trim=T)

    a <-doc %>% rvest::xml_nodes('a')
    social <- unlist(getThis(a, 'facebook|linkedin|instagram',innermost=F) %>% rvest::html_attr('href'))
    vals$contact <- c(address,contact)
    vals$contact <- gsub('\\r\\n|\\n|\\t',' ', vals$contact)
    vals$facebook <- social[grepl('www.facebook.com', social)]
    vals$linkedin <- social[grepl('www.linkedin.com', social)]
    vals$instagram <- social[grepl('www.instagram.com', social)]

    return(vals)

  }, error = function(e){return(NA)} )
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

parseR <- function(this_dir=NULL, parser=parse_content,n_threads=4, log_file = NULL,n=4000){

  tryCatch({

    log_con<-set_log_file(log_file)

   # plan(multiprocess, workers = n_threads, gc = TRUE)


    parse_dir<-paste0(this_dir,'parsed/')
    dir.create(parse_dir)
    fetched_links<-list()#new.env()
    promise_list<-list()

    #chunk_con = gzfile(paste0(this_dir,'fetched.json.gz'),open='r')
    chunk_con = file(paste0(this_dir,'fetched.json.gz'),open='r')
    tot <- 0
    while ( TRUE ) {

      f_chunk<-readLines(chunk_con,n=n)
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

parseR_2 <- function(this_dir=NULL,
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
    links$url <- gsub('\\?$|/$','',links$url)
    links$crawled<-0
    links$is_seed<-0
    links$next_crawl<-as.numeric(Sys.Date())
    links$depth<-this_depth + 1
    links$crawl_int<-1
    return(links)
  }


  ## big try catch for passing back to crawlR
  tryCatch({

    ## set logging
    log_con<-crawlR:::set_log_file(log_file)

    ## file to place links
    fh_links<-gzfile(paste0(this_dir,'fetched_links.json.gz'),open='a')

    ## read crawled data
    chunk_con = gzfile(paste0(this_dir,'fetched.json'),open='r')

    pg<-list()
    tot <- 0
    while ( TRUE ) {

      ## since file could be huge, rather than read the
      ## entire file into memory, it is read line by line
      f_chunk<-readLines(chunk_con,n=1)
      tot<-tot + length(f_chunk)
      if (length(f_chunk) == 0 ){break;}

      ## le' counter
      if(tot %% 1000 == 0){writeLines(paste('parseR: Total Done:',tot), con = log_con)}

      ## sometimes reads fail
      pg[[1]] <- tryCatch({jsonlite::fromJSON(f_chunk)$ename}, error = function(e) e )

      if(inherits(pg[[1]], "error")) next
      if(is.null(pg[[1]])) next
      if(sum(is.na(pg[[1]]))==NROW(pg[[1]])) next
      if(is.null(pg[[1]]$url)) next
      if(is.null(pg[[1]]$content))next
      if(is.na(pg[[1]]$content[1]))next

      output<- tryCatch({
         ## headers same for any type
         pg[[1]]$headers<-curl::parse_headers_list(pg[[1]]$headers)

         ## get type for further processing
         type1 <- gsub("^\\s+|\\s+$","",strsplit(pg[[1]]$type,";")[[1]][1])
         type2 <- gsub("^\\s+|\\s+$","",strsplit(type1,"/")[[1]][1])

         ## if text/html
         if(type2=='text'){
           if(length(pg[[1]]$content)>0){
             processed_links<-process_links(list(url=pg[[1]]$url,links=pg[[1]]$content$links,depth=pg[[1]]$depth))
             writeLines(jsonlite::toJSON(processed_links), con = fh_links )
           }
         }else if(type1=='application/pdf'| type2=='pdf'){
           next
           # fname <- pg[[1]]$content
           # info <- pdftools::pdf_info(fname)
           #
           # ## build content to match html content type
           # pg[[1]]$content <- list()
           #
           # ## if info exists add to content
           # if(!is.null(info)){
           #   pg[[1]]$modified <- ifelse(!is.null(info$modified),info$modified,NA)
           #   if(!is.null(info$keys)){
           #     pg[[1]]$content$title <- ifelse(!is.null(info$keys$Title),info$keys$Title,pg[[1]]$url)
           #   }
           # }
           ## actual content of pdf
           pg[[1]]$content$content <- pdftools::pdf_text(fname)
         }
       }, error = function(e){
         writeLines(paste('parseR: Recoverable Error',e), con=log_con)
       })

    }## End of while loop.

  }, error = function(e){

    writeLines(paste('parseR:',e), con=log_con)
    e<-paste('parseR:',e)
    class(e) <- 'error'

  }, finally = {

    close(chunk_con)
    close(fh_links)
    if(class(log_con)[1]=="file") close(log_con)

  })
}






#load(paste0(this_dir,'fetched_pg.rda'))

# con <- gzfile(paste0(this_dir,'fetched.json.gz'))
# fetched_pg<-lapply((readLines(con)), function(x){
#   jsonlite::fromJSON(x)$ename
# })
# close(con)
# names(fetched_pg) <- unlist(lapply(fetched_pg, function(x) x$url))
#
#   parse_dir<-paste0(this_dir,'parsed/')
#   dir.create(parse_dir)
#   fetched_links<-new.env()
#   promise_list<-list()
#
#   theseNames<-names(fetched_pg)
#   chunk_size<-floor(length(theseNames)/n_threads)
#   chunk<-list()
#   for(i in 1:n_threads){
#     writeLines(paste('chunk num:',i))
#     chunk_names<-theseNames[(chunk_size*(i-1)+1):(chunk_size*(i-1)+chunk_size)]
#     if(i==n_threads){
#       chunk_names<-theseNames[(chunk_size*(i-1)+1):length(theseNames)]
#     }
#     chunk[[i]] <- fetched_pg[chunk_names]
#   }
#   ## Running multiple threads means multiple output files.
#   for(i in 1:n_threads){
#     chunk_links<-list()
#     writeLines(paste('thread num:',i,'of',n_threads))
#     this_out <- paste0('parsed_sequential_',i,'.json')
#     promise_list[[i]]<- future(
#        globals=list(i=i,chunk=chunk,parse_dir=parse_dir,chunk_links=chunk_links,
#                     get_links=get_links, `%>%` = magrittr::`%>%`,parser=parser, this_out=this_out),{
#
#
#       con_out=base::file(base::paste0(parse_dir,this_out),open='a',blocking = F)
#       thisChunk <- chunk[[i]]
#
#
#
#       ## parseR should be called in for loop
#       for(pg in thisChunk){
#           links <- NA
#           if(is.null(pg$content)){next}
#           if(is.na(pg$content)) {next}
#           if(pg$content=='') {next}
#           if(trimws(pg$content) ==' ') {next}
#
#           chk<-try({
#             pg$headers<-curl::parse_headers_list(pg$headers)
#             type1 <- trimws(strsplit(pg$type,";")[[1]][1])
#             type2 <- trimws(strsplit(type1,"/")[[1]][1])
#
#             #fname <- pg$content
#             if(type2=='text'){
#               # con=base::gzfile(fname)
#               # pg$content <- try(silent=T,{
#               #     base::paste(base::readLines(con),collapse = ' ')
#               #   })
#               # if(class(pg$content)=='try-error'){
#               #   print(paste('error:',fname))
#               #   base::close(con)
#               #   next
#               # }
#               #base::close(con)
#               links<-get_links(pg)
#
#               pg$content <- parser(pg$content)
#
#               base::writeLines(jsonlite::toJSON(pg), con = con_out)
#             }
#             if(type1=='application/pdf'){
#               links <- NA
#               fname <- pg$content
#               pg$content <- pdftools::pdf_text(fname)
#               base::writeLines(jsonlite::toJSON(pg), con = con_out)
#             }
#           },silent=T)
#
#           if(class(chk)=='try-error') next
#           #fetched_links[[pg$url]]<-links
#           chunk_links[[pg$url]]<-links
#         }
#       base::close(con_out)
#         return(chunk_links)
#       }) %plan% multiprocess
#   }
#   for(i in 1:length(chunk)){
#     chunked_links <- value(promise_list[[i]])
#     #chunked_links<-chunk_links
#     for(j in names(chunked_links)){
#       fetched_links[[j]]<-chunked_links[[j]]
#     }
#   }

