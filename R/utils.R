## Get unique tweets or statuses

get_unique <- function(obj.list){
  obj.ids <- sapply(obj.list,function(x) return(x$id_str))
  w <- which(!duplicated(obj.ids))
  return(obj.list[w])
}

date_subtitle <- function(start.date,end.date){
  if(is.null(start.date) && is.null(end.date)){
    subt <- ""
  } else if(is.null(start.date)) {
    subt <- sprintf("Tweets up to %s",end.date)
  } else if(is.null(end.date)){
    subt <- sprintf("Tweets since %s",start.date)
  } else {
    subt <- sprintf("Period: %s -- %s",start.date,end.date)
  }
  return(subt)
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



make_top10_excel <- function(
  top10.df,
  count.col,
  footnote,
  sheet.name="Sheet1",
  file.name = NULL
){
  if(nrow(top10.df) > 1 && var(top10.df[,count.col]) > 0){
    top10.df <- top10.df[order(top10.df[,count.col], decreasing = TRUE),]
    top10.df <- top10.df[which(top10.df[,count.col] != min(top10.df[,count.col])),]
    wb<-createWorkbook(type="xlsx")
    sheet <- createSheet(wb,sheet.name)
    rows <- createRow(sheet,1:(nrow(top10.df)+2))
    cells <- createCell(rows,1:ncol(top10.df))
    TITLE_STYLE <- CellStyle(wb)+
      Font(
        wb,
        heightInPoints=24,
        isBold=TRUE,
        underline=1,
        color="#FFFFFF"
      ) +
      Fill(
        foregroundColor = 'lightblue',
        # backgroundColor = '#FFFFFF00',
        pattern = 'SOLID_FOREGROUND') +
      Border(
        color='#000000FF',
        position=c("BOTTOM","LEFT","TOP","RIGHT")
      ) +
      Alignment(
        horizontal = "ALIGN_CENTER"
      )
    CELL_STYLE_LIST <- list()
    for(j in 1:ncol(top10.df)){
      if(is.numeric(top10.df[,j])){
        align <- "ALIGN_CENTER"
      } else {
        align <- "ALIGN_CENTER"
      }
      CELL_STYLE_LIST[[j]] <- CellStyle(wb) +
        Font(
          wb,
          heightInPoints=24,
          color="#FFFFFF",
          isBold=TRUE,
          underline=0
        ) +
        Fill(
          foregroundColor = '#FFFFFF',
          backgroundColor = 'lightblue',
          pattern = 'SOLID_FOREGROUND')+
        Border(
          color='#000000FF',
          position=c("BOTTOM","LEFT","TOP","RIGHT")
        ) +
        Alignment(
          horizontal = align
        )
    }
    CELL_STYLE_FOOT <- CellStyle(wb)+
      Font(
        wb,
        heightInPoints=20,
        color="#FFFFFF",
        isBold=FALSE,
        underline=0
      ) +
      Fill(
        foregroundColor = '#FFFFFF',
        # backgroundColor = 'lightblue',
        pattern = 'SOLID_FOREGROUND')+
      Border(
        color='#000000FF',
        position=c("TOP")
      ) +
      Alignment(
        horizontal = "ALIGN_RIGHT"
      )
    for(j in 1:ncol(top10.df)){
      if(is.numeric(top10.df[,j])){
        setColumnWidth(sheet, colIndex = j, 15)
      } else {
        z <- as.character(top10.df[,j])
        n <- max(nchar(z))
        if(n > 90){
          setColumnWidth(sheet, colIndex = j, 100)
        } else {
          setColumnWidth(sheet, colIndex = j, n+10)
        }
      }
    }
    mapply(setCellValue,cells[1,],names(top10.df))
    for(j in 1:ncol(top10.df)){
      mapply(setCellValue,cells[2:(nrow(top10.df)+1),j],top10.df[,j])
    }
    setCellValue(cells[[nrow(top10.df)+2,1]],footnote)
    i<-1
    for(j in 1:ncol(top10.df)){
      setCellStyle(cells[[i,j]],TITLE_STYLE)
    }
    for(i in 2:(nrow(top10.df)+1)){
      for(j in 1:ncol(top10.df))
      setCellStyle(cells[[i,j]],CELL_STYLE_LIST[[j]])
    }
    setCellStyle(cells[[nrow(top10.df)+2,1]],CELL_STYLE_FOOT)
    ind<-addMergedRegion(sheet,nrow(top10.df)+2,nrow(top10.df)+2,1,ncol(top10.df))
    if(is.null(file.name)){
      return(wb)
    } else {
      saveWorkbook(wb,file.name)
    }
  }
}
