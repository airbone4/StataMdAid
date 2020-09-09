#' Insert \%in\%.
#'
#' Call this function as an addin to insert \code{ \%in\% } at the cursor position.
#'
#' @export
#'


#library(stringr)

extractStataCode<-function() {

  docinfo<-rstudioapi::getActiveDocumentContext()
  rmdfile<-docinfo$path
  srcName<-tools::file_path_sans_ext(basename(rmdfile))
  srcdir<-dirname(rmdfile)
  dstName<-paste0(srcdir,"/_xx_",srcName,".do")
  #txt<-readLines(rmdfile,encoding="UTF-8")
  #dstName<-"./_xx.do"
  txt <-docinfo$contents

  idx<-1:length(unlist(txt))
  chunkb<-"^\\s*```\\{\\s*stata.*\\}$"
  chunke<-"^\\s*```\\s*$"
  #不是NA,就是符合的字串
  sb<-stringr::str_extract( txt,chunkb)
  se<-stringr::str_extract(txt,chunke)
  bidx<-idx[!is.na(sb)]
  eidx<-idx[!is.na(se)]
  unlink(dstName)
  zz <- file(dstName,"w")
  for (val in bidx){
    valb<-val+1
    eline<-eidx[eidx>valb]
    vale<-eline[1]-1
    #print(paste0(valb,"-->",vale))
    code<-txt[valb:vale]

    writeLines(code,con=zz,sep="\n",useBytes = T)
  }
  close(zz)
  print(paste0(dstName," generated"))
  #return(dstName)
  file.edit(dstName)
}


