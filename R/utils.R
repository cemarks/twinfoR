## Get unique tweets or statuses

open_user <- function(user_id,screen_name){
  if(missing(user_id)){
    browseURL(
      paste(
        "http://twitter.com/",
        screen_name,
        sep=""
      )
    )
  } else {
    u <- user_show(user_id=user_id)
    screen_name <- u$screen_name
    browseURL(
      paste(
        "http://twitter.com/",
        screen_name,
        sep=""
      )
    )
  }
}

get_unique <- function(obj.list){
  obj.ids <- sapply(obj.list,function(x) return(x$id_str))
  w <- which(!duplicated(obj.ids))
  return(obj.list[w])
}


status_media <- function(
  media.table.row,
  conn,
  save.to.file = FALSE,
  file.name = NULL,
  return.image = FALSE,
  update.media.b64 = FALSE,
  hash.size=16,
  ...
){
  media.url <- media.table.row$media_url
  img.b64 <- media.table.row$img_b64
  if(is.na(img.b64)){
    p <- NA
    try(
      p <- retrieve_web_image(media.url)
    )
    if(is.null(p) || is.na(p)){
      return(NA)
    } else {
      if(update.media.b64){
        a <- OpenImageR::average_hash(OpenImageR::rgb_2gray(p$image),hash_size=hash.size)
        query <- sprintf(
          "UPDATE media SET img_hash='%s',img_b64='%s' WHERE media_id='%s';",
          a,
          p$b64,
          media.table.row$media_id
        )
        if(missing(conn)){
          stop("Data connection not provided")
        }
        DBI::dbExecute(conn,query)
      }
      if(save.to.file){
        if(is.null(file.name)){
          media.url.splt <- strsplit(media.url,"/",fixed=TRUE)[[1]]
          file.name <- media.url.splt[length(media.url.splt)]
        }
        OpenImageR::writeImage(p$image,file.name)
      }
      if(return.image){
        return(p$image)
      }
    }
  } else {
    p <- reconstitute_image(img.b64,media.url,showWarnings=showWarnings,...)
    if(!is.na(p) && save.to.file){
      if(is.null(file.name)){
        media.url.splt <- strsplit(media.url,"/",fixed=TRUE)[[1]]
        file.name <- media.url.splt[length(media.url.splt)]
      }
      OpenImageR::writeImage(p,file.name)
    }
    if(return.image){
      return(p)
    }
  }
}

reconstitute_image <- function(base64.image,media.url,display.image=TRUE,showWarnings=TRUE){
  if(is.na(base64.image)){
    if(showWarnings){
      warn("No image available in table row.")
    }
    return(NA)
  } else {
    z.raw <- base64enc::base64decode(base64.image)
    z.splt <- strsplit(media.url,".",fixed=TRUE)[[1]]
    z.ext <- z.splt[length(z.splt)]
    p <- NA
    if(tolower(z.ext) %in% c("jpg","jpeg")){
      p <- jpeg::readJPEG(z.raw)
    } else if(tolower(z.ext) == "png"){
      p <- png::readPNG(z.raw)
    } else if(tolower(z.ext) %in% c("tif","tiff")){
      p <- tiff::readTIFF(z$raw)
    }
    if(is.na(p) && showWarnings){
      warn("URL does not include file extension")
    }
    if(!is.na(p) && display.image){
      OpenImageR::imageShow(p)
    }
    return(p)
  }
}

retrieve_web_image<- function(media.url,display.image=TRUE,showWarnings=TRUE){
  z.splt <- strsplit(media.url,".",fixed=TRUE)[[1]]
  z.ext <- z.splt[length(z.splt)]
  z.response <- httr::GET(media.url)
  if(z.response$status==200){
    z.raw <- httr::content(z.response,type='raw')
    if(tolower(z.ext) %in% c("jpg","jpeg")){
      p <- jpeg::readJPEG(z.raw)
    } else if(tolower(z.ext) == "png"){
      p <- png::readPNG(z.raw)
    } else if(tolower(z.ext) %in% c("tif","tiff")){
      p <- tiff::readTIFF(z.raw)
    }
    if(!is.na(p) && display.image){
      OpenImageR::imageShow(p)
    }
    return(
      list(
        image = p,
        b64 = base64enc::base64encode(z.raw)
      )
    )
  } else {
    if(showWarnings){
      warning("Cannot retrieve image from URL.")
    }
    return(NA)
  }
}
